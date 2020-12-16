;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/hash :std/misc/list
  :std/srfi/1 :std/srfi/43 :std/sugar :std/text/hex :std/text/json
  :clan/assert :clan/base :clan/hash :clan/io :clan/json :clan/list :clan/maybe :clan/number
  :clan/syntax :clan/with-id
  ./poo ./mop ./brace ./io ./number)

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

(.def methods.bytes
  .sexp<-: (lambda (x) ['hex-decode (hex-encode x)])
  .string<-: hex-encode
  .<-string: hex-decode
  .bytes<-: identity
  .<-bytes: identity
  .json<-: .string<-
  .<-json: .<-string)
(.def (Bytes @ [methods.bytes Type.])
  sexp: 'Bytes
  .element?: bytes?
  .Length: Nat
  .zero: #u8()
  .marshal: (lambda (x port) (marshal .Length (bytes-length x) port) (write-u8vector x port))
  .unmarshal: (lambda (port) (def n (unmarshal .Length port)) (read-bytes* n port)))
(.def (BytesN. @ [methods.bytes Type.] n)
  sexp: `(BytesN ,n)
  .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
  .length-in-bytes: n
  .zero: (make-bytes n)
  .<-string: (λ (x) (validate @ (hex-decode x)))
  .<-bytes: (cut validate @ <>)
  .marshal: write-u8vector
  .unmarshal: (cut read-bytes* n <>))
(def (BytesN n) (.cc BytesN. n: n))

(.def (String @ [methods.marshal<-bytes Type.])
  sexp: 'String
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
(.def (methods.bytes&marshal<-string @ [methods.bytes<-marshal] .<-string .string<-)
  .String: String
  .marshal: (lambda (x port) (marshal .String (.string<- x) port))
  .unmarshal: (lambda (port) (.<-string (unmarshal .String port))))
(.def (methods.json&bytes&marshal<-string @ [methods.bytes&marshal<-string] .<-string .string<-)
  .<-json: .<-string
  .json<-: .string<-)
(.def (Symbol @ [methods.json&bytes&marshal<-string Type.])
  sexp: 'Symbol
  .element?: symbol?
  .<-string: string->symbol
  .string<-: symbol->string)
(.def (Keyword @ [methods.json&bytes&marshal<-string Type.])
  exp: 'Keyword
  .element?: keyword?
  .<-string: string->keyword
  .string<-: keyword->string)
(.def (Json @ [methods.bytes&marshal<-string Type.])
  sexp: 'Json
  .element?: true
  .sexp<-: identity ;; TODO: recursively handle tables
  .json<-: identity
  .<-json: identity
  .string<-: string<-json
  .<-string: json<-string)
(.def (methods.string&bytes&marshal<-json @ [methods.bytes&marshal<-string] .json<- .<-json)
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string))

(.def (Or. @ [methods.bytes<-marshal Type.] types)
  sexp: `(Or ,@(map (cut .@ <> sexp) types))
  types@: (list->vector types)
  .element?: (λ (x) (any (cut element? <> x) types))
  ;; WE ASSUME THE JSON'S ARE DISJOINT, AS ARE THE VALUES (BUT WE DISCRIMINATE WHEN MARSHALLING)
  .discriminant-length-in-bits: (integer-length (1- (length types)))
  .discriminant-length-in-bytes: (n-bytes<-n-bits .discriminant-length-in-bits)
  .discriminant<-: (lambda (v) (let/cc return (vector-for-each (lambda (i t) (when (element? t v) (return i))) types@) #f))
  .json<-: (lambda (v) (def disc (.discriminant<- v))
              ;;[disc (json<- (vector-ref types@ disc) v)])
              (json<- (vector-ref types@ disc) v))
  .<-json: ;;(lambda (j) (<-json (vector-ref types@ (car j)) (cadr j)))
  (lambda (v) (let/cc return (for-each (lambda (t) (return (<-json t v))) types) #f))
  .marshal: (lambda (v port)
              (def disc (.discriminant<- v))
              (write-integer-bytes disc .discriminant-length-in-bytes port)
              (marshal (vector-ref types@ disc) v port))
  .unmarshal: (lambda (port)
                (def disc (read-integer-bytes .discriminant-length-in-bytes port))
                (unmarshal (vector-ref types@ disc) port)))
(def (Or . types) {(:: @ Or.) (types)})

;; Bottom tells you everything about nothing
(.def (Bottom @ Type.)
  sexp: 'Bottom
  .element?: false
  .bytes<-: invalid .<-bytes: invalid
  .json<-: invalid .<-json: invalid
  .marshal: invalid .unmarshal: invalid)
(defalias ⊥ Bottom)

;; Top tells you nothing about everything
(.def (Top @ [Type.])
  sexp: 'Top
  .element?: true
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
  .length-in-bytes: (n-bytes<-n-bits .length-in-bits)
  .<-nat: (cut vector-ref .vals@ <>)
  .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
  .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
  .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
  .bytes<-: (compose (cut bytes<-nat <> .length-in-bytes) .nat<-)
  .<-bytes: (compose .<-nat nat<-bytes)
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

;; This was not put in number.ss because it depended on Pair
(.def (Rational @ Real)
  sexp: 'Rational
  .element?: rational?
  ;; NB: a Scheme "rational" includes floating point numbers.
  ;; For actual ratios between integers, we should have a separate type "Ratnum" or some such.
  .Pair: (Pair Integer Nat) ;; Pair isn't defined until a later file. Commenting out for now.
  .pair<-: (lambda (x) (cons (numerator x) (denominator x)))
  .<-pair: (lambda (numerator denominator) (/ numerator denominator))
  .marshal: (lambda (x port) (marshal .Pair (.pair<- x) port))
  .unmarshal: (compose .<-pair (.@ .Pair .unmarshal))
  .bytes<-: (bytes<-<-marshal .marshal)
  .<-bytes: (<-bytes<-unmarshal .unmarshal)
  .json<-: (compose (.@ .Pair .json<-) .<-pair)
  .<-json: (compose .<-pair (.@ .Pair .<-json)))

(.def (Maybe. @ [methods.bytes<-marshal Type.] type)
  sexp: `(Maybe ,(.@ type sexp))
  .element?: (lambda (x) (or (void? x) (element? type x)))
  .sexp<-: (lambda (v) (if (void? v) '(void) (sexp<- type v)))
  .json<-: (lambda (v) (if (void? v) v ((.@ type .json<-) v)))
  .<-json: (lambda (j) (if (void? j) j ((.@ type .<-json) j)))
  .marshal: (λ (x port) (cond ((void? x) (write-byte 0 port))
                              (else (write-byte 1 port) (marshal type x port))))
  .unmarshal: (λ (port) (if (zero? (read-byte port)) (void) (unmarshal type port))))
(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (OrFalse. @ [methods.bytes<-marshal Type.] type)
  sexp: `(OrFalse ,(.@ type sexp))
  .element?: (lambda (x) (or (not x) (element? type x)))
  .sexp<-: (lambda (v) (and v (sexp<- type v)))
  .json<-: (lambda (v) (and v ((.@ type .json<-) v)))
  .<-json: (lambda (j) (and j ((.@ type .<-json) j)))
  .marshal: (λ (x port) (cond (x (write-byte 1 port) (marshal type x port))
                              (else (write-byte 0 port))))
  .unmarshal: (λ (port) (and (not (zero? (read-byte port))) (unmarshal type port))))
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
  .json<-: (lambda (m) (hash-key-value-map m (cut string<- Key <>) (cut json<- Value <>)))
  .<-json: (lambda (j) (hash-key-value-map j (cut <-string Key <>) (cut <-json Value <>))))
(defrules Map (<- ->)
  ((_ Value <- Key) {(:: @ Map.) Key: Key Value: Value})
  ((_ Key -> Value) {(:: @ Map.) Key: Key Value: Value}))

(def (RecordSlot type . options)
  (def o (.<-alist (map (match <> ([k . v] (cons (symbolify k) v))) (alist<-plist options))))
  {(:: @ [o]) (type) optional: (or (.ref o 'optional false) (.has? o default))})

;; TODO: Generate a proto field that supports initialization-time defaults.
;; TODO: Support single inheritance.
;; TODO: Support multiple inheritance.
(def (Record . plist)
  (def a (map (match <> ([kw type . options] (cons (symbolify kw) (apply RecordSlot type options))))
              (alist<-plist plist)))
  {(:: @ [methods.bytes<-marshal Class.] proto)
   sexp: ['Record (append-map (match <> ([k . s] [(symbol->keyword k) ['@list (.@ s type sexp)]])) a)...]
   slots: (.<-alist a)
   slot-names: (map car a)
   types: (map (lambda (s) (.@ (.ref slots s) type)) slot-names)
   optionals: (map (lambda (s) (.@ (.ref slots s) optional)) slot-names)
   defaults: (map (lambda (s) (.ref (.ref slots s) 'default void)) slot-names)
   .sexp<-: (lambda (v) `(instance ,sexp
                      ,@(append-map (lambda (s t o)
                                      (when/list (or (not o) (.key? v s))
                                        [(keywordify s) (sexp<- t (.ref v s))]))
                                    slot-names types optionals)))
   .json<-: (lambda (v) (list->hash-table
                    (append-map (lambda (s t o d) (when/list (or (not o)
                                                            (and (.key? v s)
                                                                 (not (equal? (.ref v s) d))))
                                               [(cons (symbol->string s) (json<- t (.ref v s)))]))
                         slot-names types optionals defaults)))
   .<-json: (lambda (j)
              (.mix (.<-alist (append-map (lambda (s t o)
                                            (def ss (symbol->string s))
                                            (when/list (or (not o) (hash-key? j ss))
                                              [(cons s (<-json t (hash-ref j ss)))]))
                                          slot-names types optionals))
                    proto))
   .marshal: (lambda (v port) (for-each (lambda (s t o)
                                     (if o
                                       (let (has? (.key? v s))
                                         (marshal Bool (.key? v s) port)
                                         (when has? (marshal t (.ref v s) port)))
                                       (marshal t (.ref v s) port)))
                                   slot-names types optionals))
   .unmarshal: (lambda (port)
                 (.mix (.<-alist
                        (with-list-builder (c)
                          (for-each (lambda (s t o)
                                      (if o
                                        (let (has? (unmarshal Bool port))
                                          (when has? (c (cons s (unmarshal t port)))))
                                        (c (cons s (unmarshal t port)))))
                                    slot-names types optionals)))
                       proto))
   .tuple-list<-: (lambda (x) (map (lambda (s) (.ref x (car s))) a))
   .<-tuple-list: (lambda (x) (.<-alist (map (lambda (s v) (cons (car s) v)) a x)))
   .tuple<-: (compose list->vector .tuple-list<-)
   .<-tuple: (compose .<-tuple-list vector->list)})


;; Sum : {Kw Type} ... -> Type
;; Sum types aka tagged unions, each kw is a tag
(def (Sum . plist)
  ;; a : [Assocof Symbol Type]
  (def a (map (match <> ([kw . type] (cons (symbolify kw) type))) (alist<-plist plist)))
  (def tag-marsh-t (UInt (integer-length (max 0 (1- (length a))))))
  {(:: @ [methods.bytes<-marshal Type.])
      sexp: ['Sum (append-map (match <> ([k . t] [k (.@ t sexp)])) a)...]
      variants: (.<-alist a)
      variant-names: (map car a)
      types: (map cdr a)
      make: (lambda (tag value) {(tag) (value)})
      .element?:
      (lambda (v)
        (match v
          ({(tag) (value)}
           (and (.key? variants tag)
                (element? (.ref variants tag) value)))
          (_ #f)))
      .sexp<-: (lambda (v)
                 (def tag (.@ v tag))
                 `(.call ,sexp make ',tag ,(sexp<- (.ref variants tag) (.@ v value))))
      .json<-: (lambda (v)
                 (def tag (.@ v tag))
                 (def tagj (symbol->string tag))
                 (def valj (json<- (.ref variants tag) (.@ v value)))
                 (if (json-symbolic-keys)
                     (hash (tag tagj) (value valj))
                     (hash ("tag" tagj) ("value" valj))))
      .<-json: (lambda (j)
                 (def tag (string->symbol (hash-ref j (if (json-symbolic-keys) 'tag "tag"))))
                 (make tag (<-json (.ref variants tag) (hash-ref j (if (json-symbolic-keys) 'value "value")))))
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
