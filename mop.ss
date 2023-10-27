;;-*- Gerbil -*-
;;; Classes on top of POO
;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

(export #t)

(import
  (for-syntax
   (only-in :std/srfi/1 list-index split-at))
  (only-in :std/error deferror-class defraise/context)
  (only-in :std/generic defgeneric)
  (only-in :std/misc/list-builder with-list-builder)
  (only-in :std/misc/list pop!)
  (only-in :std/misc/repr repr pr)
  (only-in :std/misc/walist walist)
  (only-in :std/srfi/1 every append-map)
  (only-in :std/sugar defrule try catch)
  (only-in :std/values list->values)
  (only-in :clan/base λ compose nest invalid)
  (only-in :clan/io u8vector<-<-marshal <-u8vector<-unmarshal
           marshal<-u8vector<- unmarshal<-<-u8vector
           marshal-sized16-u8vector unmarshal-sized16-u8vector)
  (only-in :clan/json string<-json json<-string)
  (only-in :clan/syntax call<-formals)
  (only-in ./object object .def/ctx .cc .mix .ref .@ .get .has? .call
           .all-slots with-slots .slot? .putslot! .putdefault! .for-each!
           NoApplicableMethod?
           .alist object<-alist
           $constant-slot-spec $computed-slot-spec)
  (only-in ./brace @method @@method))

;; * Options
;; default: default value when no method is defined, also bottom value for fixed-point
;; from: one of the symbols instance (default), or type. Specifies who provides methods
;;   for the generic function: the instance directly, or its .type field, respectively.
;; slot: slot-name can specify an alternate slot-name, which helps avoid with-slot shadowing
;;   when calling the gf from a method definition.
;; TODO: add method combination, which requires integration with the recursion in .ref.
;;   at some point, the information about the slot needs to be made available in the meta object.
;; TODO: make this information somehow available at compile-time when possible,
;;   to enable inlining and partial evaluation.
(defsyntax (.defgeneric stx) ;; define a generic function that invokes the prototype's slot
  (def (parse-options options)
    (def slot-name #f)
    (def compute-default #f)
    (def from #f)
    (let loop ((options options))
      (match options
        ([] (values slot-name compute-default (or from 'instance)))
        ([kw val . rest]
         (match (syntax-e kw)
           (slot:
            (when slot-name (error "slot-name set twice" stx))
            (set! slot-name val))
           (from:
            (when from (error "from set twice" stx))
            (case (syntax-e val)
              ((type) (set! from 'type))
              ((instance) (set! from 'instance))
              (else (error "invalid option" stx val))))
           (compute-default:
            (when compute-default (error "default set twice" stx))
            (set! compute-default val))
           (default:
            (when compute-default (error "default set twice" stx))
            (set! compute-default (with-syntax ((expr val)) #'(lambda (_ _) expr))))
           (x (error "invalid option" x stx)))
         (loop rest))
        (_ (error "invalid options" options (syntax->datum options))))))
  (syntax-case stx ()
    ((.defgeneric (gf self . formals) options ...)
     (begin
       (define-values (slot-name compute-default from) (parse-options (syntax->list #'(options ...))))
       (with-syntax* ((slot-name (or slot-name (make-symbol "." (syntax->datum #'gf))))
                      (methods (case from
                                 ((instance) #'self)
                                 ((type) #'(.get self .type))
                                 (else (error "invalid from" (syntax->datum stx)))))
                      (method (if compute-default
                                (with-syntax ((compute-default compute-default))
                                  #'(try (.ref methods 'slot-name)
                                         (catch (NoApplicableMethod? _) (compute-default self 'slot-name))))
                                #'(.ref methods 'slot-name)))
                      ((selfvar ...) (case from ((type) #'(self)) ((instance) #'())))
                      (body (if (and (stx-null? #'formals) (eq? from 'instance))
                              #'method
                              (call<-formals #'(method selfvar ...) #'formals))))
           #'(def (gf self . formals) body))))))

;;(def (and-combination new-value old-value) (and (new-value dont-call-next-method) (old-value)))

(defrule (.call.method object slot args ...) ((.get object .type slot) object args ...))

(.defgeneric (element? type x)
   ;;default: false
   ;;combination: and-combination
   slot: .element?)

;;; SEXP from values

;; gf to extract a source sexp from a value of given type
(.defgeneric (sexp<- type x) slot: .sexp<-)

(defgeneric :sexp
  (lambda (x)
    (if (or (number? x) (boolean? x) (string? x) (char? x) (void? x) (keyword? x) (eof-object? x))
      x
      `',x))) ;; TODO: do better than that.

(defmethod (@@method :sexp object)
  (λ (self)
    (cond
     ((.has? self .type .sexp<-) (.call (.@ self .type) .sexp<- self))
     ((.has? self sexp) (.@ self sexp))
     (else self))))

;; gf to extract a value of given type from some json
(.defgeneric (<-json type j) slot: .<-json)
;; gf to extract some json from a value of given type
(.defgeneric (json<- type x) slot: .json<-)

;; For now, types are just runtime descriptors...
(defsyntax (define-type stx)
  (syntax-case stx ()
    ((_ name desc) (identifier? #'name)
     #'(def name (.cc desc sexp: 'name)))
    ((_ (name opts ...) spec ...) (identifier? #'name)
     (with-syntax ((ctx stx))
       #'(.def/ctx ctx (name opts ...) sexp: 'name spec ...)))))

(define-type (Type. @ [])
  .type: Type
  ;; NB: either `validate` or `element?` can be primary, with the other method deduced from it.
  ;; But if you fail to override either, it's bottomless mutual recursion calling them.
  .element?: ;; : Bool <- Any ;; is this an element of this type?
  (lambda (x) (try (.validate x) #t (catch (_) #f)))
  .validate: ;; : @ <- Any ?(List Any) ;; identity for an @, throws a type-error if input isn't a @
  (lambda (x) (if (.element? x) x (raise-type-error (TV Type @) x))))

(def (display-object l (port (current-output-port)))
  (let d ((space? #f))
    (unless (null? l)
      (let (x (pop! l))
        (when (and space? (not (string? x))) (display " " port))
        (cond
         ((string? x)
          (display x port) (d #f))
         ((or (symbol? x) (keyword? x))
          (display x port) (d #t))
         ((element? Type x)
          (write (sexp<- x (pop! l)) port) (d #t))
         (else (pr x port) (d #t)))))))

(def (display-object-ln . l)
  (def port (current-output-port))
  (display-object l port)
  (newline port)
  (force-output port))

(deferror-class TypeError ())
(defraise/context (raise-type-error where irritants ...)
  (TypeError "type error" irritants: [irritants ...]))

;; TODO: teach .defgeneric about optional arguments.
;; TODO: use macro that leaves source info by default, returns a function when passed as argument.
(def (validate type x) (.call type .validate x))

;; Any accepts any Scheme object, but only does best effort at printing, invalid reading
(define-type (Any @ [Type.])
  .element?: true
  .sexp<-: (lambda (x) (if (or (pair? x) (null? x) (symbol? x)) (list 'quote x) x))
  .string<-: repr .<-string: invalid
  .bytes<-: (compose string->bytes .string<-)
  .<-bytes: (compose .<-string bytes->string)
  .json<-: .string<- .<-json: .<-string
  .marshal: (lambda (x port) (marshal-sized16-u8vector (.bytes<- x) port))
  .unmarshal: (lambda (port) (.<-bytes (unmarshal-sized16-u8vector port)))
  .=?: equal?)

(define-type (Object @ Type.)
  .element?: object? .sexp<-: identity) ;; TODO: improve on that

(define-type (Bool @ Type.)
  .length-in-bytes: 1
  .element?: boolean?
  .sexp<-: identity
  .json<-: identity
  .<-json: (cut validate @ <>)
  .bytes<-: (lambda (x) (if x #u8(1) #u8(0)))
  .<-bytes: (λ (b) (< 0 (u8vector-ref b 0)))
  .marshal: (marshal<-u8vector<- .bytes<-)
  .unmarshal: (unmarshal<-<-u8vector .<-bytes .length-in-bytes))

(def (object-values x) (map (cut .ref x <>) (.all-slots x)))
(def (monomorphic-object? type x)
  (and (object? x) (every (cut element? type <>) (object-values x))))

(define-type (MonomorphicObject. @ Type. type) ;; all the values are of given type
  .element?: (cut monomorphic-object? type <>)
  .sexp<-: (lambda (x) `(.o ,@(append-map (match <> ([s . v] [(make-keyword s) (sexp<- type v)])) (.alist x))))
  .json<-: (lambda (x) (list->hash-table (map (match <> ([s . v] (cons s (json<- type v)))) (.alist x))))
  .<-json: (lambda (j) (object<-alist (map (match <> ([s . v] (cons (make-symbol s) (<-json type v)))) (hash->list j)))))

(def (MonomorphicObject type) {(:: @ MonomorphicObject.) type
                               sexp: `(MonomorphicObject ,(.@ type sexp))})
(def ObjectObject (MonomorphicObject Object))
(def (map-object-values f object)
  (object<-alist (map (lambda (slot) (cons slot (f (.ref object slot)))) (.all-slots object))))

;; TODO: support optional and keyword arguments in the input types, and multiple arities a la case-lambda
;; TODO: support contract-checking validation wrapping that works well with tail-calls and continuations,
;; by avoiding the accumulation of wrappers for the same types, instead having a single wrapper that
;; accumulates and de-duplicates types to check, maybe in a weak-values hash-table.
;; TODO: support validation in incremental amortized O(1) rather than O(n) for recursive data-structures,
;; possibly with a weak-values hash-table for less trivial invariants.
(define-type (Function. @ Type. outputs inputs)
  .element?: procedure? ;; we can't dynamically test that a function has the correct signature :-(
  .validate: (lambda (f)
               (unless (procedure? f) (raise-type-error Type @ Any f))
               (def (validate-row kind types elems k)
                 (unless (= (length elems) (length types))
                   (raise-type-error @ f invalid-number-of: kind))
                 (k (map (lambda (type elem) (validate type elem))
                         types elems)))
               (nest
                (lambda ins)
                (validate-row inputs: inputs ins) (lambda (vins))
                (call/values (lambda () (apply f vins))) (lambda outs)
                (validate-row outputs: outputs outs list->values)))
  arity: (length inputs))

(def (Function outputs inputs)
  (for-each (cut validate Type <>) outputs)
  (for-each (cut validate Type <>) inputs)
  {(:: @ Function.) (outputs) (inputs)
   sexp: `(Function (@list ,@(map (cut .@ <> sexp) outputs)) (@list ,@(map (cut .@ <> sexp) inputs)))})

;; The expander complains "Syntax Error: Ambiguous pattern".
;; TODO: Use syntax-case, detect when there are opposite arrows, curry when there are multiple ones?
;; With no arrow, it's a thunk (no inputs) with the given outputs.
(defsyntax (Fun stx)
  (syntax-case stx ()
    ((_ . io)
     (let (iol (syntax->list #'io))
       (cond
        ((list-index (lambda (x) (eq? (stx-e x) '<-)) iol)
         => (lambda (k)
              (defvalues (outputs inputs) (split-at iol k))
              (let loop ((o outputs) (i (cdr inputs)))
                (cond
                 ((list-index (lambda (x) (eq? (stx-e x) '<-)) i)
                  => (lambda (k)
                       (defvalues (inputs moreinputs) (split-at i k))
                       (loop [[#'Function [#'@list . o] [#'@list . inputs]]] (cdr moreinputs))))
                 (else [#'Function [#'@list . o] [#'@list . i]])))))
        ((list-index (lambda (x) (eq? (stx-e x) '->)) iol)
         => (lambda (k)
              (defvalues (inputs ios) (split-at iol k))
              (let loop ((i inputs) (iol (cdr ios)))
                (cond
                 ((list-index (lambda (x) (eq? (stx-e x) '->)) i)
                  => (lambda (k)
                       (defvalues (inputs moreios) (split-at i k))
                       [#'Function [#'@list (loop inputs (cdr moreios))] [#'@list . i]]))
                 (else [#'Function [#'@list . iol] [#'@list . i]])))))
        (else
         [#'Function [#'@list . iol] []]))))))

(.defgeneric (slot-checker slot-descriptor slot-name x) slot: .slot.check from: type)
(.defgeneric (slot-definer slot-descriptor slot-name x) slot: .slot.define from: type)

(define-type (Class. class Type. slots sexp sealed) ;; this is the class descriptor for class descriptor objects.
  .type: Class
  effective-slots:
   (let (slot-base (.@ .type slot-descriptor-class proto))
     (map-object-values (cut .mix <> slot-base) slots))
  .element?:
   (λ (x)
     (and (object? x)
          (every (λ (slot-name)
                   (def slot (.ref effective-slots slot-name))
                   (slot-checker slot slot-name x))
                 (.all-slots effective-slots))
          (or (not sealed) ;; sealed means only defined slots can be present.
              (every (cut .slot? effective-slots <>) (.all-slots x)))))
  slots: ? {}
  sealed: ? #f
  ;;slots: {.type: {type: Type default: class hidden: #t}} ;; should it be optional?
  proto:
   (let (p {})
     (for-each (λ (slot-name)
                 (def slot (.ref effective-slots slot-name))
                 (slot-definer slot slot-name p))
               (.all-slots effective-slots))
     p)
  .sexp<-: (lambda (v) `(instance ,sexp
                     ,@(with-list-builder (c)
                         (def (add s v) (c (make-keyword s)) (c v))
                         (.for-each! effective-slots
                                     (lambda (name slot) (.call.method slot .slot.sexp<- name v add))))))
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string)
  .json<-: (lambda (v) (walist
                   (with-list-builder (c)
                     (def (add s v) (c (cons (symbol->string s) v)))
                     (.for-each! effective-slots
                                 (lambda (name slot) (.call.method slot .slot.json<- name v add))))))
  .<-json: (lambda (j)
             (object<-alist supers: proto
              (with-list-builder (c)
                (def add (compose c cons))
                (.for-each! effective-slots
                            (lambda (name slot) (.call.method slot .slot.<-json name j add))))))
  .marshal: (lambda (v port)
              (.for-each! effective-slots
                          (lambda (name slot) (.call.method slot .slot.marshal name v port))))
  .unmarshal: (lambda (port)
                (object<-alist supers: proto
                 (with-list-builder (c)
                   (def add (compose c cons))
                   (.for-each! effective-slots
                               (lambda (name slot) (.call.method slot .slot.unmarshal name port add))))))
  .bytes<-: (u8vector<-<-marshal .marshal)
  .<-bytes: (<-u8vector<-unmarshal .unmarshal)
  .tuple-list<-: (lambda (x) (map (lambda (s) (.ref x s)) (.all-slots effective-slots)))
  .<-tuple-list: (lambda (x) (object<-alist (map (lambda (s v) (cons s v)) (.all-slots effective-slots) x)))
  .tuple<-: (compose list->vector .tuple-list<-)
  .<-tuple: (compose .<-tuple-list vector->list))

(define-type (Slot @ Class.)
  slots:
   {type: {type: Type optional: #t}
    constant: {type: Any optional: #t}
    compute: {type: (Fun Any <- Object (Fun Any <-)) optional: #t} ;; a function that takes self and superfun, suitable to be passed to $computed-slot-spec
    default: {type: Any optional: #t}
    optional: {type: Bool default: #f}
    hidden: {type: Bool default: #f}}
  proto: {.type: @ optional: #f hidden: #f}
  .effectively-optional?: (lambda (x) (or (.@ x optional?) (.has? x default)))
  .slot.check:
    (λ (@@ slot-name x)
      (with-slots (type constant optional) @@
        (if (.slot? x slot-name)
          (let ((value (.ref x slot-name)))
            (and
              (or (not (.has? @@ type)) (element? type value))
              (or (not (.has? @@ constant)) (equal? constant value))))
          (and (.has? @@ optional) optional))))
  .slot.define:
    (λ (@@ slot-name x)
       (with-slots (type constant compute default) @@
         (cond
          ((.has? @@ constant) (.putslot! x slot-name ($constant-slot-spec constant)))
          ((.has? @@ compute) (.putslot! x slot-name ($computed-slot-spec compute)))
          ((.has? @@ default) (.putdefault! x slot-name default))
          ((and (.has? @@ type) (.has? type proto))
           (.putslot! x slot-name ($constant-slot-spec (.@ type proto)))))))
    ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name self))))
  .slot.marshal:
    (λ (@@ slot-name x port)
      (with-slots (type constant optional default) @@
        (unless (.has? @@ constant)
          (let (default? (.has? @@ default))
            (cond
             ((or optional default?)
              (if (or (not (.slot? x slot-name))
                      (and default? (equal? (.ref x slot-name) default)))
                (write-u8 0 port)
                (begin
                  (write-u8 1 port)
                  (.call type .marshal (.ref x slot-name) port))))
             ((.slot? x slot-name)
              (.call type .marshal (.ref x slot-name) port))
             (else (error "Can't marshal missing slot" slot-name x)))))))
  .slot.unmarshal:
    (λ (@@ slot-name port add)
       (with-slots (type constant optional default) @@
         (unless (.has? @@ constant)
           (let (default? (.has? @@ default))
             (cond
              ((or optional default?)
               (match (read-u8 port)
                 (0 (when default? (add slot-name default)))
                 (1 (add slot-name (.call type .unmarshal port)))))
              (else (add slot-name (.call type .unmarshal port))))))))
  .slot.sexp<-:
    (λ (@@ slot-name x c)
      (with-slots (type constant optional default) @@
        (unless (.has? @@ constant)
          (let (default? (.has? @@ default))
            (cond
             ((or optional default?)
              (when (and (.slot? x slot-name)
                         (not (and default? (equal? (.ref x slot-name) default))))
                (c slot-name (.call type .sexp<- (.ref x slot-name)))))
             ((.slot? x slot-name)
              (c slot-name (.call type .sexp<- (.ref x slot-name))))
             (else (error "Can't convert missing slot to sexp" slot-name x)))))))
  .slot.json<-:
    (λ (@@ slot-name x c)
      (with-slots (type constant optional default) @@
        (unless (.has? @@ constant)
          (let (default? (.has? @@ default))
            (cond
             ((or optional default?)
              (when (and (.slot? x slot-name)
                         (not (and default? (equal? (.ref x slot-name) default))))
                (c slot-name (.call type .json<- (.ref x slot-name)))))
             ((.slot? x slot-name)
              (c slot-name (.call type .json<- (.ref x slot-name))))
             (else (error "Can't convert missing slot to json" slot-name x)))))))
  .slot.<-json:
    (λ (@@ slot-name j c)
       (with-slots (type constant optional default) @@
         (unless (.has? @@ constant)
           (let (key (symbol->string slot-name))
             (cond
              ((hash-key? j key) (c slot-name (.call type .<-json (hash-ref j key))))
              ((.has? @@ default) (c slot-name default))
              (optional (void))
              (else (error "Missing slot in json" slot-name j))))))))
(def Slot. (.@ Slot proto))

;; TODO: functional lenses in (.lens foo) as well as imperative accessors
(define-type (Lens @ Class.) ;; Lens 's 'a
  slots: =>.+ { ;; or should we have just a ((f a) <- (f : Functor) ((f b) <- b) a) ?
    .get: { } ;; 's -> 'a
    .set: { }} ;; 'a -> 's -> 's
  .get: (lambda (l s) (.call l .get s))
  .set: (lambda (l a s) (.call l .set a s))
  .modify: (lambda (l f s)
             (.call l .set (f (.call l .get s)) s)) ;; over in haskell
  ;; Same order as in Haskell, opposite to OCaml. (compose x y) will access x then y
  ;; (Lens 'a 'c)  <- (Lens 'a 'b) (Lens 'b 'c)
  .compose: (lambda (l1 l2) {.get: (lambda (s) (.call l2 .get (.call l1 s)))
                        .set: (lambda (a s) (.modify l1 (cut .call l2 .set a <>) s))}))

(define-type (Type @ Class.)
  slots: {sexp: {type: Any}
          .element?: {type: (Fun Bool <- Any)}}
  .element?: (λ (x) (and (object? x) (.has? x sexp) (.has? x .element?)))
  .sexp<-: (lambda (x) (.@ x sexp)) ;; : SEXP <- @
  proto: Type.)

(define-type (Class @ Type)
   slot-descriptor-class: Slot ;; MOP magic!
   slots: =>.+
    {slots: {type: ObjectObject} ;; would be (MonomorphicObject Slot) if we didn't automatically append Slot
     sealed: {type: Bool default: #f}}
   proto: Class.)

(def (instance class . plist) (apply .cc (.@ class proto) plist))

;; TODO: make-instance or new should .instantiate the object.
;; TODO: What name for a syntax that does not instantiate it?
(defrules .new ()
  ((_ (class self slots ...) slot-defs ...)
   {(:: self (.@ class proto) slots ...) slot-defs ...})
  ((_ (class) slot-defs ...)
   {(:: self (.@ class proto)) slot-defs ...})
  ((_ class slot-defs ...)
   {(:: self (.@ class proto)) slot-defs ...}))

(defrules .defclass ()
  ((_ (class class-options ...) (slotdefs ...) options ...)
   (define-type (class class-options ...) (slots =>.+ {slotdefs ...}) options ...))
  ((_ class (slotdefs ...) options ...)
   (.defclass (class) (slotdefs ...) options ...)))

(def (slot-lens slot-name)
  {(:: @ (instance Lens))
   .get: (lambda (s) (.ref s slot-name))
   .set: (lambda (x s) (.cc s slot-name x))})

;; TODO: a special slot .parameters, and a function that specialized a object on its positional parameters?
