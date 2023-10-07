;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)

(import
  (only-in :std/assert assert!)
  (only-in :std/iter for/collect for in-range in-iota)
  (only-in :std/misc/bytes n-bits->n-u8 nat->u8vector u8vector->nat)
  (only-in :std/misc/hash hash-key-value-map hash-ensure-ref)
  (only-in :std/misc/list pop! acons)
  (only-in :std/srfi/1 append-map any every)
  (only-in :std/srfi/43 vector-index vector-map vector-unfold vector-for-each)
  (only-in :std/sugar defrule hash with-id)
  (only-in :std/text/hex hex-decode hex-encode)
  (only-in :clan/assert assert-equal!)
  (only-in :clan/base λ compose invalid ignore-errors)
  (only-in :clan/io u8vector<-<-marshal <-u8vector<-unmarshal
           write-nat-u8vector read-nat-u8vector unmarshal-n-u8)
  (only-in :clan/json json-normalize string<-json json<-string)
  (only-in :clan/list index-of alist<-plist)
  (only-in ./object .def .@ .ref object<-alist .slot? .call .o)
  (only-in ./mop define-type Type Type. Class. Any
           raise-type-error validate element? :sexp sexp<- json<- <-json)
  (only-in ./number UInt Real Integer Nat)
  (only-in ./brace @method)
  (only-in ./io methods.bytes<-marshal methods.marshal<-bytes
           methods.marshal<-fixed-length-bytes methods.string<-json
           marshal unmarshal string<- <-string))

;; vector-map-in-order : [Index A B ... -> C] [Vectorof A] [Vectorof B] ... -> [Vectorof C]
;; The applictions of `f` are in order, unlike `vector-map`, but like `vector-for-each`
(def (vector-map-in-order f v . rst)
  (def n (vector-length v))
  (for ((v2 rst))
    (assert! (= n (vector-length v2)) "vector-map-in-order: lengths should be the equal"))
  (vector-unfold
   (lambda (i)
     (apply f i (vector-ref v i) (map (cut vector-ref <> i) rst)))
   n))

(.def (Tuple. @ [methods.bytes<-marshal Type.] types)
  sexp: `(Tuple ,@(map (cut .@ <> sexp) type-list))
  type-list: (vector->list types)
  .element?:
    (λ (x)
      (def l (vector-length types))
      (and (vector? x) (= (vector-length x) l)
           (let/cc return
             (for ((i (in-iota l)))
               (unless (element? (vector-ref types i) (vector-ref x i)) (return #f)))
             #t)))
  .sexp<-: (lambda (v) `(vector ,@(vector->list (vector-map (lambda (_ t x) (sexp<- t x)) types v))))
  .json<-: (lambda (v) (vector->list (vector-map (lambda (_ t x) (json<- t x)) types v)))
  .<-json: (lambda (j) (vector-map (lambda (_ t x) (<-json t x)) types (if (list? j) (list->vector j) j)))
  .marshal: (lambda (v port)
              (vector-for-each (lambda (_ type val) (marshal type val port))
                               types v))
  .unmarshal: (lambda (port) (vector-map-in-order (lambda (_ type) (unmarshal type port)) types)))
(def (Tuple . type-list) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) type-list)))
  {(:: @ Tuple.) (types)})

(.def (List. @ [methods.bytes<-marshal methods.string<-json Type.] type)
  sexp: `(List ,(.@ type sexp))
  .Length: Nat
  .element?: (λ (x) (and (list? x) (every (cut element? type <>) x)))
  .sexp<-: (lambda (v) ['@list (map (.@ type .sexp<-) v) ...])
  .json<-: (let (m (.@ type .json<-)) (cut map m <>))
  .<-json: (let (m (.@ type .<-json)) (cut map m <>))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (marshal .Length (length v) port) (for-each (cut m <> port) v)))
  .unmarshal: (let ((ul (.@ .Length .unmarshal))
                    (ut (.@ type .unmarshal)))
                (lambda (port) (def l (ul port)) (for/collect (_ (in-range l)) (ut port)))))
(def (List type)
  (validate Type type)
  {(:: @ List.) (type)})

(define-type (methods.bytes @ [])
  .sexp<-: (lambda (x) ['hex-decode (hex-encode x)])
  .string<-: hex-encode
  .<-string: hex-decode
  .bytes<-: identity
  .<-bytes: identity
  .json<-: .string<-
  .<-json: .<-string)
(define-type (Bytes @ [methods.bytes Type.])
  .sexp<-: (lambda (x) `(hex-decode ,(hex-encode x)))
  .element?: u8vector?
  .Length: Nat
  .zero: #u8()
  .marshal: (lambda (x port) (marshal .Length (u8vector-length x) port) (write-u8vector x port))
  .unmarshal: (lambda (port) (def n (unmarshal .Length port)) (unmarshal-n-u8 n port)))
(.def (BytesN. @ [methods.bytes Type.] n)
  sexp: `(BytesN ,n)
  .element?: (λ (x) (and (u8vector? x) (= (u8vector-length x) n)))
  .validate: (λ (x)
               (unless (u8vector? x) (raise-type-error @ x))
               (unless (= (u8vector-length x) n)
                 (raise-type-error @ x [length-mismatch: expected: n given: (u8vector-length x)]))
               x)
  .sexp<-: (.@ Bytes .sexp<-)
  .length-in-bytes: n
  .zero: (make-u8vector n)
  .<-string: (λ (x) (validate @ (hex-decode x)))
  .<-bytes: (cut validate @ <>)
  .marshal: write-u8vector
  .unmarshal: (cut unmarshal-n-u8 n <>))
(def BytesN<-n (make-hash-table))
(def (BytesN n) (hash-ensure-ref BytesN<-n n (lambda () {(:: @ BytesN.) n})))
(def Bytes32 (BytesN 32))
(def Bytes64 (BytesN 64))

(define-type (String @ [methods.marshal<-bytes Type.])
  .element?: string?
  .Bytes: Bytes
  .zero: ""
  .add: string-append
  .=?: string=?
  .sexp<-: identity
  .<-string: identity
  .string<-: identity
  .bytes<-: string->bytes
  .<-bytes: bytes->string
  .<-json: (cut validate @ <>)
  .json<-: identity)
(define-type (methods.bytes&marshal<-string @ [methods.bytes<-marshal] .<-string .string<-)
  .String: String
  .marshal: (lambda (x port) (marshal .String (.string<- x) port))
  .unmarshal: (lambda (port) (.<-string (unmarshal .String port))))
(define-type (methods.json&bytes&marshal<-string @ [methods.bytes&marshal<-string] .<-string .string<-)
  .<-json: .<-string
  .json<-: .string<-)
(define-type (Symbol @ [methods.json&bytes&marshal<-string Type.])
  .element?: symbol?
  .sexp<-: (cut list 'quote <>)
  .<-string: string->symbol
  .string<-: symbol->string)
(define-type (Keyword @ [methods.json&bytes&marshal<-string Type.])
  .element?: keyword?
  .sexp<-: identity
  .<-string: string->keyword
  .string<-: keyword->string)
(define-type (Json @ [methods.bytes&marshal<-string Type.])
  .element?: true
  .sexp<-: (lambda (x) `(json<-string ,(.string<- x)))
  .json<-: identity
  .<-json: identity
  .string<-: string<-json
  .<-string: json<-string)
(define-type (methods.string&bytes&marshal<-json @ [methods.bytes&marshal<-string] .json<- .<-json)
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string))

(.def (Or. @ [methods.bytes<-marshal Type.] types)
  sexp: `(Or ,@(map (cut .@ <> sexp) types))
  types@: (list->vector types)
  .element?: (λ (x) (any (cut element? <> x) types))
  ;; WE ASSUME THE JSON'S ARE DISJOINT, AS ARE THE VALUES (BUT WE DISCRIMINATE WHEN MARSHALLING)
  .discriminant-length-in-bits: (integer-length (1- (length types)))
  .discriminant-length-in-bytes: (n-bits->n-u8 .discriminant-length-in-bits)
  .discriminant<-: (lambda (v) (let/cc return (vector-for-each (lambda (i t) (when (element? t v) (return i))) types@) #f))
  .sexp<-: (lambda (v) (sexp<- (vector-ref types@ (.discriminant<- v)) v))
  .json<-: (lambda (v) (def disc (.discriminant<- v))
              ;;[disc (json<- (vector-ref types@ disc) v)])
              (json<- (vector-ref types@ disc) v))
  .<-json: ;;(lambda (j) (<-json (vector-ref types@ (car j)) (cadr j)))
  (lambda (v) (let/cc return (for-each (lambda (t) (return (<-json t v))) types) #f))
  .marshal: (lambda (v port)
              (def disc (.discriminant<- v))
              (write-nat-u8vector disc .discriminant-length-in-bytes port)
              (marshal (vector-ref types@ disc) v port))
  .unmarshal: (lambda (port)
                (def disc (read-nat-u8vector .discriminant-length-in-bytes port))
                (unmarshal (vector-ref types@ disc) port)))
(def (Or . types) {(:: @ Or.) (types)})

;; Bottom tells you everything about nothing
(define-type (Bottom @ Type.)
  .element?: false
  .sexp<-: invalid
  .bytes<-: invalid .<-bytes: invalid
  .json<-: invalid .<-json: invalid
  .marshal: invalid .unmarshal: invalid)
(defalias ⊥ Bottom)

;; Top tells you nothing about everything
(define-type (Top @ [Type.])
  .element?: true
  .sexp<-: invalid
  .string<-: invalid .<-string: invalid
  .bytes<-: invalid .<-bytes: invalid
  .json<-: invalid .<-json: invalid)
(defalias ⊤ Top)

(.def (Exactly. @ Type. value)
  sexp: `(Exactly ,(:sexp value)) ;; TODO: have a better generic sexp function?
  .element?: (λ (x) (equal? x value))
  .Log: Bottom
  kvalue: (lambda _ value)
  jsvalue: (json-normalize value)
  .sexp<-: (.@ Any .sexp<-)
  .json<-: (lambda (x) (assert-equal! x value) jsvalue)
  .<-json: (lambda (x) (assert-equal! x jsvalue) value)
  .bytes<-: (lambda _ #u8())
  .<-bytes: kvalue
  .marshal: void
  .unmarshal: kvalue)
(def (Exactly value) {(:: @ Exactly.) (value)})

(define-type Null (Exactly '()))
(define-type False (Exactly #f))
(define-type True (Exactly #t))
(define-type Unit (Exactly (void)))

(.def (Enum. @ [methods.marshal<-fixed-length-bytes Type.] vals)
  sexp: `(Enum ,@(map :sexp vals))
  .element?: (cut member <> vals)
  .vals@: (list->vector vals)
  .json@: (list->vector (map json-normalize vals))
  .length-in-bits: (integer-length (1- (vector-length .vals@)))
  .length-in-bytes: (n-bits->n-u8 .length-in-bits)
  .<-nat: (cut vector-ref .vals@ <>)
  .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
  .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
  .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
  .bytes<-: (compose (cut nat->u8vector <> .length-in-bytes) .nat<-)
  .<-bytes: (compose .<-nat u8vector->nat)
  .marshal: => (lambda (super) (if (zero? .length-in-bytes) void super))
  .unmarshal: => (lambda (super) (cond
                             ((< 0 .length-in-bytes) super)
                             ((void? vals) (lambda _ (error "no value to unmarshal of type" sexp)))
                             (else (let (val (car vals)) (lambda _ val)) super))))
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

;; Untagged union. Can be used for JSON, but no automatic marshaling.
(.def (Union. @ [methods.string&bytes&marshal<-json Type.] types)
  sexp: `(Union ,@(map (cut .@ <> sexp) types))
  .element?: (λ (x) (any (cut element? <> x) types))
  .json<-: (lambda (v) (let/cc return
                    (for-each (λ (t) (when (element? t v) (return (json<- t v))))
                              types)
                    (error "invalid element of type" v sexp)))
  .<-json: (lambda (j) (let/cc return
                    (for-each (λ (t) (ignore-errors (return (<-json t j))))
                              types)
                    (error "invalid json for type" j sexp))))
(def (Union . types) ;; type of tuples, heterogeneous arrays of given length and type
  {(:: @ Union.) (types)})

(.def (Pair. @ [methods.bytes<-marshal Type.] left right)
  sexp: `(Pair ,(.@ left sexp) ,(.@ right sexp))
  .element?: (lambda (v) (and (pair? v) (element? left (car v)) (element? right (cdr v))))
  .sexp<-: (lambda (v) `(cons ,(sexp<- left (car v)) ,(sexp<- right (cdr v))))
  .json<-: (lambda (v) [(json<- left (car v)) (json<- right (cdr v))])
  .<-json: (lambda (j) (cons (car j) (cadr j)))
  .marshal: (lambda (v port)
              (marshal left (car v) port)
              (marshal right (cdr v) port))
  .unmarshal: (lambda (port) (let* ((a (unmarshal left port))
                               (d (unmarshal right port)))
                          (cons a d))))
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(define-type (TypeValuePair @ Type.)
  .element?: (match <> ([t . v] (and (element? Type t) (element? t v))) (_ #f))
  .validate: (lambda (x)
               (match x ([t . v] (validate Type t) (validate t x))
                      (_ (raise-type-error @ x "not a type-value pair"))))
  .sexp<-: (match <> ([t . v] `(cons ,(.@ t sexp) ,(sexp<- t v))))
  .json<-: (match <> ([t . v] [(sexp<- Type t) (json<- t v)])) ;; supposes a simple enough type
  .<-json: invalid ;; maybe we should somehow register types that are valid for I/O into a table?
  .marshal: invalid
  .unmarshal: invalid)



;; This was not put in number.ss because it depended on Pair
(define-type (Rational @ Real)
  .element?: rational?
  ;; NB: a Scheme "rational" includes floating point numbers.
  ;; For actual ratios between integers, we should have a separate type "Ratnum" or some such.
  .Pair: (Pair Integer Nat) ;; Pair isn't defined until a later file. Commenting out for now.
  .pair<-: (lambda (x) (cons (numerator x) (denominator x)))
  .<-pair: (lambda (numerator denominator) (/ numerator denominator))
  .marshal: (lambda (x port) (marshal .Pair (.pair<- x) port))
  .unmarshal: (compose .<-pair (.@ .Pair .unmarshal))
  .bytes<-: (u8vector<-<-marshal .marshal)
  .<-bytes: (<-u8vector<-unmarshal .unmarshal)
  .json<-: (compose (.@ .Pair .json<-) .<-pair)
  .<-json: (compose .<-pair (.@ .Pair .<-json)))

(.def (Maybe. @ [methods.bytes<-marshal Type.] type)
  sexp: `(Maybe ,(.@ type sexp))
  .element?: (lambda (x) (or (void? x) (element? type x)))
  .sexp<-: (lambda (v) (if (void? v) '(void) (sexp<- type v)))
  .json<-: (lambda (v) (if (void? v) v ((.@ type .json<-) v)))
  .<-json: (lambda (j) (if (void? j) j ((.@ type .<-json) j)))
  .marshal: (λ (x port) (cond ((void? x) (write-u8 0 port))
                              (else (write-u8 1 port) (marshal type x port))))
  .unmarshal: (λ (port) (if (zero? (read-u8 port)) (void) (unmarshal type port))))
(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (OrFalse. @ [methods.bytes<-marshal Type.] type)
  sexp: `(OrFalse ,(.@ type sexp))
  .element?: (lambda (x) (or (not x) (element? type x)))
  .sexp<-: (lambda (v) (and v (sexp<- type v)))
  .json<-: (lambda (v) (and v ((.@ type .json<-) v)))
  .<-json: (lambda (j) (and j ((.@ type .<-json) j)))
  .marshal: (λ (x port) (cond (x (write-u8 1 port) (marshal type x port))
                              (else (write-u8 0 port))))
  .unmarshal: (λ (port) (and (not (zero? (read-u8 port))) (unmarshal type port))))
(def (OrFalse type) {(:: @ OrFalse.) (type)})

(.def (Map. @ [methods.string&bytes&marshal<-json Type.] Key Value)
  sexp: `(Map ,(.@ Value sexp) <- ,(.@ Key sexp))
  .element?: (lambda (x) (and (hash-table? x)
                         (let/cc return
                           (hash-for-each
                            (lambda (k v) (unless (and (element? Key k) (element? Value v))
                                       (return #f)))
                            x) #t)))
  .empty: (make-hash-table)
  .json<-: (lambda (m) (hash-key-value-map (lambda (k v) (cons (string<- Key k) (json<- Value v))) m))
  .<-json: (lambda (j) (hash-key-value-map (lambda (k v) (cons (<-string Key k) (<-json Value v))) j)))
(defrules Map (<- ->)
  ((_ Value <- Key) {(:: @ Map.) Key: Key Value: Value})
  ((_ Key -> Value) {(:: @ Map.) Key: Key Value: Value}))

(def (RecordSlot type . options)
  (object<-alist
   (acons 'type type
          (map (match <> ([k . v] (cons (make-symbol k) v))) (alist<-plist options)))))

;; TODO: Generate a proto field that supports initialization-time defaults.
;; TODO: Support single inheritance.
;; TODO: Support multiple inheritance.
(def (Record . args)
  (def supers (if (keyword? (car args)) [] (pop! args)))
  {(:: @ (cons supers Class.))
   slots: =>.+ (object<-alist
                (map (match <> ([kw type . options]
                                (cons (make-symbol kw) (apply RecordSlot type options))))
                     (alist<-plist args)))})

;; Sum : {Kw Type} ... -> Type
;; Sum types aka tagged unions, each kw is a tag
(def (Sum . plist)
  ;; a : [Assocof Symbol Type]
  (def a (map (match <> ([kw . type] (cons (make-symbol kw) type))) (alist<-plist plist)))
  (def tag-marsh-t (UInt (integer-length (max 0 (1- (length a))))))
  {(:: @ [methods.bytes<-marshal Type.])
      sexp: ['Sum (append-map (match <> ([k . t] [k (.@ t sexp)])) a)...]
      variants: (object<-alist a)
      variant-names: (map car a)
      types: (map cdr a)
      make: (lambda (tag value) {(tag) (value)})
      .validate:
      (lambda (x)
        (match x
          ({tag value}
           (unless (.slot? variants tag) (raise-type-error @ x ["invalid tag" tag]))
           (validate (.ref variants tag) value)
           x)
          (_ (raise-type-error @ x "not a tag-value variant"))))
      .element?:
      (lambda (v)
        (match v
          ({(tag) (value)}
           (and (.slot? variants tag)
                (element? (.ref variants tag) value)))
          (_ #f)))
      .sexp<-: (lambda (v)
                 (def tag (.@ v tag))
                 `(.call ,sexp make ',tag ,(sexp<- (.ref variants tag) (.@ v value))))
      .json<-: (lambda (v)
                 (def tag (.@ v tag))
                 (def tagj (symbol->string tag))
                 (def valj (json<- (.ref variants tag) (.@ v value)))
                 (hash ("tag" tagj) ("value" valj)))
      .<-json: (lambda (j)
                 (def tag (string->symbol (hash-ref j "tag")))
                 (make tag (<-json (.ref variants tag) (hash-ref j "value"))))
      .marshal: (lambda (v port)
                  (def tag (.@ v tag))
                  (def tag-n (index-of variant-names tag))
                  (marshal tag-marsh-t tag-n port)
                  (marshal (.ref variants tag) (.@ v value) port))
      .unmarshal: (lambda (port)
                    (def tag-n (unmarshal tag-marsh-t port))
                    (def tag (list-ref variant-names tag-n))
                    (def value (unmarshal (.ref variants tag) port))
                    (make tag value))})

(begin-syntax
  (def ((sum-constructor-match-transformer tag-sym) stx)
    (syntax-case stx ()
      ((_ p) (with-syntax ((tag* tag-sym)) #'(.o tag: 'tag* value: p)))))
  (def ((sum-constructor-expr-transformer sum-id tag-sym) stx)
    (syntax-case stx ()
      ((_ e) (with-syntax ((sum sum-id) (tag* tag-sym)) #'(.call sum make 'tag* e))))))
(defrule (define-sum-constructors sum-id variant-id ...)
  (begin
    (with-id sum-id ((sum-variant-id #'sum-id "-" #'variant-id))
      (defsyntax-for-match sum-variant-id
        (sum-constructor-match-transformer 'variant-id)
        (sum-constructor-expr-transformer #'sum-id 'variant-id)))
    ...))
