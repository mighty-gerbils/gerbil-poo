(export #t)

(import
  (only-in :gerbil/gambit object->serial-number object->string)
  :gerbil/gambit
  (only-in :gerbil/runtime/mop write-style)
  :std/assert
  :std/format
  :std/generic
  :std/iter
  :std/misc/repr
  :std/misc/walist
  (only-in :std/sugar defrule ignore-errors hash try catch)
  :std/stxutil
  :std/text/json
  :clan/base
  :clan/hash
  :clan/io
  :clan/json
  ./object
  ./mop
  ./brace)

;;; Byte I/O

;; gf to effectfully read a byte from a stream or stream-like object.
;; Byte <- In
(.defgeneric (object-read-byte in)
  default:
  (λ (in)
    (if (input-port? in) (read-u8 in)
        (error "Trying to read-u8 from unsupported object" in))))

;; gf to effectfully write a byte to a stream or stream-like object.
;; Unit <- Byte Out
(.defgeneric (object-write-byte byte out))

;; gf to effectfully read bytes from a stream or stream-like object into a u8vector
;; Unit <- In Bytes ?offset: UInt ?length: UInt
(.defgeneric (read-bytes-into bs in offset: (offset 0) length: (length (- (u8vector-length bs) offset)))
  default:
  (λ (in l)
    (for (i (in-range length))
      (let ((b (read-u8 in))) ;; TODO: handle EOF, return number of bytes read???
        (u8vector-set! bs (+ i offset) b)))))

;; gf to effectfully read bytes from a stream or stream-like object into a new u8vector
;; Bytes <- In UInt
(.defgeneric (object-read-bytes length in)
  default:
  (λ (in length)
    (def bs (make-u8vector length))
    (if (input-port? in)
      (let ((n (read-u8vector bs in))) (assert! (= n length)))
      (read-bytes-into bs in length: length))
    bs))

;; gf to effectfully write bytes to a stream or stream-like object from a u8vector
;; Unit <- Bytes Out ?offset: UInt ?length: UInt
(.defgeneric (object-write-bytes bs out offset: (offset 0) length: (length (- (u8vector-length bs) offset)))
  default:
  (λ (out bs offset: (offset 0) length: (length (- (u8vector-length bs) offset)))
    (for (i (in-range length))
      (write-u8 (u8vector-ref bs (+ i offset)) out))))


;;; Printing objects as per std/misc/repr

(defmethod (@@method :pr object)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (def (d . l) (for-each (cut display <> port) l))
    (cond
     ((and (object-%instance self) (not (object-%slot-funs self)))
      (d "#" (object->serial-number self) "#;{(inconsistent object)")
      (for-each (lambda (slot) (d " " slot)) (map car (append (object-slots self) (object-defaults self))))
      (d "}"))
     ((.has? self .type .print-representation) ((.@ self .type .print-representation) self port options))
     ((.has? self .type .sexp<-) (write ((.@ self .type .sexp<-) self) port))
     ((.has? self .type) (print-class-object self port options))
     ((.has? self print-representation) (.call self print-representation port options))
     ((.has? self sexp) (write (.@ self sexp) port))
     ;; TODO: have a better fallback
     (else
      (let ()
        (def first? #t)
        (def slots (.all-slots self))
        (def h (or (object-%instance self) (hash)))
        (d "#" (object->serial-number self) " #;{")
        (for-each (lambda (k)
                    (if first? (set! first? #f) (d " "))
                    (d (symbol->string k) ": ")
                    (if (and h (hash-key? h k))
                      (pr (hash-get h k) port options)
                      (d "…")))
                  slots))
      (d "}")))))

(def (print-class-object
      x (port (current-output-port)) (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "#") (d (object->serial-number x)) (d "#;{")
  (try
   (for-each (λ-match ([k . v] (d " (") (w k) (d " ") (prn v) (d ")"))) (.alist x))
   (catch (e) (void)))
  (d "}"))

(.defgeneric (printable-slots x) from: type default: .all-slots slot: .printable-slots)


;;; JSON I/O

(defmethod (@@method :write-json object)
  (lambda (self port)
    (cond
     ((.has? self .type .write-json) ((.@ self .type .write-json) self port))
     ((.has? self .type .json<-) (write-json ((.@ self .type .json<-) self) port))
     ((.has? self sexp) (write-json (object->string (.@ self sexp)) port))
     (else (write-json (walist (.alist self)) port)))))
(defmethod (@@method :json object)
  (lambda (self)
    (cond
     ;;((.has? self .type .write-json) self) ;; TODO: Uncomment after vyzo/gerbil#595 goes through
     ((.has? self .type .json<-) ((.@ self .type .json<-) self))
     ((.has? self sexp) (object->string (.@ self sexp)))
     (else (hash<-object self)))))

(def (json-string<- type x)
  (string<-json (json<- type x)))
(def (<-json-string type x)
  (<-json type (json<-string x)))

(define-type (methods.string<-json @ [] .json<- .<-json)
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string))


;;; (Un)Marshaling data to/from bytes
;; The output has to be self-delimited, so marshaling output can be concatenated given the types.
;; A trivial way to satisfy this constraint is for the output to have fixed length,
;; or for the length to be prepended. Another way is to have some terminator value (e.g. 0 in C strings).

(.defgeneric (marshal type x port) slot: .marshal)
(.defgeneric (unmarshal type port) slot: .unmarshal)

(defrule (marshal-product port (val type) ...)
  (begin (marshal type val port) ...))
(defrule (unmarshal-product port type ...)
  (left-to-right values (unmarshal type port) ...))


;;; Converting to/from bytes
;; Unlike with marshaling, these bytes do NOT have to be self-delimited in case length is variable.

;; The bytes<- and its corresponding .bytes<- methods return bytes (a byte vector)
;; that need not be self-delimiting, as contrasted to the marshal method that must output
;; a self-delimiting sequence of bytes. There is usually no difference for a fixed-size data types,
;; but there may be a difference for variable-size types, wherein bytes<- may omit size information
;; that marshal must include.
;; : Bytes <- 'a:Type 'a
(.defgeneric (bytes<- type x) slot: .bytes<-)

;; The <-bytes function and its .<-bytes methods take bytes as output by the bytes<- function,
;; and return an object of the given type that will be equal to the originally encoded object,
;; according to whichever equality predicate makes sense for the data type.
;; : 'a <- 'a:Type Bytes
(.defgeneric (<-bytes type b) slot: .<-bytes)

(define-type (methods.bytes<-marshal @ [] .marshal .unmarshal)
  .bytes<-: (u8vector<-<-marshal .marshal)
  .<-bytes: (<-u8vector<-unmarshal .unmarshal))

(define-type (methods.marshal<-bytes @ [] .<-bytes .bytes<- .Bytes)
  .marshal: (lambda (x port) (marshal .Bytes (.bytes<- x) port))
  .unmarshal: (lambda (port) (.<-bytes (unmarshal .Bytes port))))

(define-type (methods.marshal<-fixed-length-bytes @ [] .<-bytes .bytes<- .length-in-bytes)
  .marshal: (lambda (x port) (write-u8vector (.bytes<- x) port))
  .unmarshal: (lambda (port) (.<-bytes (unmarshal-n-u8 .length-in-bytes port))))

;;; Converting to/from string

;; : String <- 'a:Type 'a
(.defgeneric (string<- type x) slot: .string<-)

;; : 'a <- 'a:Type String
(.defgeneric (<-string type b) slot: .<-string)


;;; Gambit printer hook for object. See https://github.com/vyzo/gerbil/issues/589
;;; See also ##inverse-eval.
(defmethod (@@method :wr object)
  (lambda (self we)
    (ignore-errors (instantiate-object! self))
    (def inconsistent? (not (object-%slot-funs self)))
    (defvalues (slots h)
      (if inconsistent?
        (values (map car (append (object-slots self) (object-defaults self))) (hash))
        (values (.all-slots self) (object-%instance self))))
    (def style (write-style we))
    (def mark? (eq? style 'mark))
    (def (s x) (unless mark? (##wr-str we x)))
    (def (w x) (##wr we x))
    (def (comma)
      (case style
        ((mark display) (void))
        ((write write-shared pretty-print) (s ","))))
    (def (sn)
      (s "#") (w (object->serial-number self)) (s " #;"))
    (def (wr-fields)
      (def first #t)
      (for-each (lambda (k)
                  (unless mark?
                    (if first
                      (begin
                        (set! first #f)
                        (s "{")
                        (when inconsistent?
                          (s "(inconsistent object) ")))
                      (s " "))
                    (w (string->keyword (symbol->string k)))
                    (s " "))
                  (if (and h (hash-key? h k))
                    (w (hash-get h k))
                    (s "…")))
                slots)
      (s "}"))
    (cond
     ((.has? self .type .write-object)
      ((.@ self .type .write-object) we self))
     ((.has? self .type .sexp<-)
      (comma) (w ((.@ self .type .sexp<-) self)))
     ;;((.has? self .type) (print-class-object self (macro-writeenv-port we) (current-representation-options)))
     ((.has? self write-object) (.call self write-object we))
     ((.has? self sexp) (comma) (w (.@ self sexp)))
     ((.has? self .sexp) (comma) (w (.@ self .sexp)))
     (else (sn) (wr-fields)))))

(defstruct TV (type value) ;; Typed Value
  transparent: #t)
(defmethod (@@method :wr TV)
  (lambda (self writeenv)
    (def style (write-style writeenv))
    (def mark? (eq? style 'mark))
    (def (s x) (unless mark? (##wr-str writeenv x)))
    (def (w x) (##wr writeenv x))
    (with ((TV type value) self)
      (if (.has? type write-object)
        ;; write-object specializes :wr
        (let (f (cut (.@ type write-object) value writeenv))
          (case style
            ((mark) (w type) (f))
            ((display) (f))
            ((write write-shared pretty-print)
             (s ",(TV ") (w type) (s " ") (f) (s ")"))))
        (case style
          ((mark) (w type) (w value))
          ((display)
           (w type) (s ":")
           (cond
            ((.has? type .json<-) (s (json-string<- type value)))
            ((.has? type .string<-) (w (string<- type value)))
            ((.has? type .sexp<-) (s (object->string (sexp<- type value))))
            (else (s (object->string (sexp<- type value))))))
          ((write write-shared pretty-print)
           (cond
            ((.has? type .string<-) (s ",(TS ") (w type) (s " ") (w (string<- type value)) (s ")"))
            ((.has? type .sexp<-) (s ",(TV ") (w type) (s " ") (w (sexp<- type value)) (s ")"))
            ((.has? type .json<-) (s ",(TJ ") (w type) (s " ") (w (json-string<- type value)) (s ")"))
            (else (s "#") (w (object->serial-number self))
                  (s " #;(TV ") (w type) (s " ") (w value) (s ")")))))))))

(def (TS type json-string) (TS type (<-string type string)))
(def (TJ type json-string) (TV type (<-json-string type json-string)))
