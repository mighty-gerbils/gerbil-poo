;;-*- Gerbil -*-
;;; Classes on top of POO
;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

(export #t)

(import
  (for-syntax :std/srfi/1 :clan/syntax)
  :clan/syntax
  :gerbil/gambit/exact :gerbil/gambit/ports
  :std/error :std/format :std/generic :std/iter :std/lazy
  :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/base :clan/error :clan/hash :clan/io :clan/list :clan/syntax
  ./poo ./brace)

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
    (def default #f)
    (def from #f)
    (let loop ((options options))
      (match options
        ([] (values slot-name default (or from 'instance)))
        ([kw val . rest]
         (match (syntax-e kw)
           (slot: (when slot-name (error "slot-name set twice" stx))
                  (set! slot-name val))
           (from: (when from (error "from set twice" stx))
                  (case (syntax-e val)
                    ((type) (set! from 'type))
                    ((instance) (set! from 'instance))
                    (else (error "invalid option" stx val))))
           (default: (when default (error "default set twice" stx))
                     (set! default val))
           (x (error "invalid option" x stx)))
         (loop rest))
        (_ (error "invalid options" options (syntax->datum options))))))
  (syntax-case stx ()
    ((.defgeneric (gf self . formals) options ...)
     (begin
       (define-values (slot-name default from) (parse-options (syntax->list #'(options ...))))
       (with-syntax* ((slot-name (or slot-name (symbolify "." #'gf)))
                      (default* default)
                      (base (if default #'(λ () default*) #'(no-such-slot self 'slot-name)))
                      (methods (case from
                                 ((instance) #'self)
                                 ((type) #'(.get self .type))
                                 (else (error "invalid from" (syntax->datum stx)))))
                      (method #'(.ref methods 'slot-name base))
                      ((selfvar ...) (case from ((type) #'(self)) ((instance) #'())))
                      (body (if (and (stx-null? #'formals) (eq? from 'instance))
                              #'method
                              (call<-formals #'(method selfvar ...) #'formals))))
           #'(def (gf self . formals) body))))))

;;(def (and-combination new-value old-value) (and (new-value dont-call-next-method) (old-value)))

(defrules .method ()
  ((_ poo slot) (.get poo .type slot))
  ((_ poo slot args ...) ((.method poo .type slot) args ...)))

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

(defmethod (@@method :sexp poo)
  (λ (self)
    (cond
     ((.has? self .type .sexp<-) (.call (.@ self .type) .sexp<- self))
     ((.has? self sexp) (object->string (.@ self sexp))))))

;; gf to extract a value of given type from some json
(.defgeneric (<-json type j) slot: .<-json)
;; gf to extract some json from a value of given type
(.defgeneric (json<- type x) slot: .json<-)

(.def (Type. @)
  .type: Type
  sexp: (error "missing type sexp" @) ;; Any
  ;; NB: either `validate` or `element?` can be primary, with the other method deduced from it.
  ;; But if you fail to override either, it's bottomless mutual recursion calling them.
  .element?: ;; : Bool <- Any ;; is this an element of this type?
  (lambda (x) (try (.validate x '()) #t (catch (_) #f)))
  .validate: ;; : @ <- Any ?(List Any) ;; identity for an @, throws a type-error if input isn't a @
  (lambda (x (context '())) (if (.element? x) x (type-error context Type @ [value: x]))))

(def (display-poo l (port (current-output-port)))
  (let d ((space? #f))
    (unless (null? l)
      (when space? (display " " port))
      (let (x (pop! l))
        (cond
         ((string? x)
          (display x port) (d #f))
         ((or (symbol? x) (keyword? x))
          (display x port) (d #t))
         ((element? Type x)
          (write (sexp<- x (pop! l)) port) (d #t))
         (else (pr x port) (d #t)))))))

(def (display-poo-ln . l) (display-poo l (current-output-port)) (newline (current-output-port)))

(def (display-context c (port (current-output-port)))
  (for-each (lambda (l) (display-poo l port) (newline port))
            (reverse (append c (current-error-context)))))

(defstruct (<Error> exception) (tag args context) transparent: #t)
(def (Error tag (context '()) . args) (raise (<Error> tag args context)))
(def (type-error (context '()) . args) (apply Error type-error: context args))
(defmethod (@@method display-exception <Error>)
  (lambda (self port)
    (display-context (<Error>-context self) port)
    (display-poo (cons (<Error>-tag self) (<Error>-args self)) port)
    (newline port))
  rebind: #t)

(defstruct tv (type value) transparent: #t)
(.def (TV @)
  sexp: 'TV ;; Type and Value
  .validate: (lambda (x (ctx '()))
               (def c [[validating: x] . ctx])
               (match x ((tv t v) (validate t v c)) (_ (type-error c "no tv"))))
  .sexp<-: (lambda (x) `(tv ,(.@ @ sexp) (sexp<- (tv-type x) (tv-value x)))))

;; TODO: teach .defgeneric about optional arguments.
;; TODO: use macro that leaves source info by default, returns a function when passed as argument.
(def (validate type x (context '())) (.call type .validate x context))

;; Any accepts any Scheme object, but only does best effort at printing, invalid reading
(.def (Any @ [Type.])
  sexp: 'Any
  .element?: true
  .sexp<-: (cut list 'quote <>)
  .string<-: repr .<-string: invalid
  .bytes<-: (compose string->bytes .string<-)
  .<-bytes: (compose .<-string bytes->string)
  .json<-: .string<- .<-json: .<-string
  .marshal: (lambda (x port) (write-sized16-bytes (.bytes<- x) port))
  .unmarshal: (lambda (port) (.<-bytes (read-sized16-bytes port)))
  .=?: equal?)

(.def (Poo @ Type.) sexp: 'Poo .element?: poo?
      .sexp<-: identity) ;; TODO: improve on that

(.def (Bool @ Type.)
  sexp: 'Bool
  .length-in-bytes: 1
  .element?: boolean?
  .sexp<-: identity
  .json<-: identity
  .<-json: (cut validate @ <>)
  .bytes<-: (lambda (x) (if x #u8(1) #u8(0)))
  .<-bytes: (λ (b) (< 0 (u8vector-ref b 0)))
  .marshal: (marshal<-bytes<- .bytes<-)
  .unmarshal: (unmarshal<-<-bytes .<-bytes .length-in-bytes))

(def (poo-values x) (map (cut .ref x <>) (.all-slots x)))
(def (monomorphic-poo? type x)
  (and (poo? x) (every (cut element? type <>) (poo-values x))))

(.def (MonomorphicPoo. @ Type. type) ;; all the values are of given type
  sexp: `(MonomorphicPoo ,(.@ type sexp))
  .element?: (cut monomorphic-poo? type <>)
  .sexp<-: (lambda (x) `(.o ,@(append-map (match <> ([s . v] [(keywordify s) (sexp<- type v)]))
                                     (.sorted-alist x))))
  .json<-: (lambda (x) (list->hash-table (map (match <> ([s . v] (cons s (json<- type v)))) (.alist x))))
  .<-json: (lambda (j) (.<-alist (map (match <> ([s . v] (cons (symbolify s) (<-json type v)))) (hash->list j)))))

(def (MonomorphicPoo type) {(:: @ MonomorphicPoo.) type})
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m {})
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot))))
            (.all-slots poo))
  m)

;; TODO: support optional and keyword arguments in the input types, and multiple arities a la case-lambda
;; TODO: support contract-checking validation wrapping that works well with tail-calls and continuations,
;; by avoiding the accumulation of wrappers for the same types, instead having a single wrapper that
;; accumulates and de-duplicates types to check, maybe in a weak-values hash-table.
;; TODO: support validation in incremental amortized O(1) rather than O(n) for recursive data-structures,
;; possibly with a weak-values hash-table for less trivial invariants.
(.def (Function. @ Type. outputs inputs)
  sexp: `(Function (@list ,@(map (cut .@ <> sexp) outputs)) (@list ,@(map (cut .@ <> sexp) inputs)))
  .element?: procedure? ;; we can't dynamically test that a function has the correct signature :-(
  .validate: (lambda (f (ctx '()))
               (unless (procedure? f) (type-error ctx Type @ Any f))
               (def (validate-row context kind types elems k)
                 (unless (= (length elems) (length types))
                   (type-error context invalid-number-of: kind))
                 (k (map (lambda (type elem i)
                           (validate type elem [[position: i] . ctx]))
                         types elems (iota (length types)))))
               (nest
                (lambda ins) (let (ctx2 [[calling: f type: (tv Type @) inputs: ins] . ctx]))
                (validate-row ctx2 inputs: inputs ins) (lambda (vins))
                (call/values (lambda () (apply f vins))) (lambda outs)
                (validate-row [[outputs: outs] . ctx2] outputs: outputs outs list->values)))
  arity: (length inputs))

(def (Function outputs inputs)
  (for-each (cut validate Type <>) outputs)
  (for-each (cut validate Type <>) inputs)
  {(:: @ Function.) (outputs) (inputs)})

;; The expander complains "Syntax Error: Ambiguous pattern".
;; TODO: Use syntax-case, detect when there are opposite arrows, curry when there are multiple ones?
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
        (else (error "illegal Fun type" stx))))))) ;; or should it be (Values . io) ?

(.defgeneric (slot-checker slot-descriptor slot-name base x) slot: .slot-checker from: type)
(.defgeneric (slot-definer slot-descriptor slot-name x) slot: .slot-definer from: type)

(.def (Class. class Type. slots sexp sealed) ;; this is the class descriptor for class descriptor objects.
  .type: Class
  effective-slots:
   (let (slot-base (.@ .type slot-descriptor-class proto))
     (map-poo-values (cut .mix <> slot-base) slots))
  .sexp<-: (lambda (x) (.@ x sexp))
  .element?:
   (λ (x)
     (and (poo? x)
          (every (λ (slot-name)
                   (def slot (.ref effective-slots slot-name))
                   (def base (.ref slot 'base (λ () (no-such-slot x slot-name))))
                   (slot-checker slot slot-name base x))
                 (.all-slots effective-slots))
          (or (not sealed) ;; sealed means only defined slots can be present.
              (every (cut .key? effective-slots <>) (.all-slots x)))))
  slots: {.type: {type: Type default: class hidden: #t}}
  proto:
   (let ((p {}))
     (for-each (λ (slot-name)
                 (def slot (.ref effective-slots slot-name))
                 (slot-definer slot slot-name p))
               (.all-slots effective-slots))
     p)
  sealed: #f)

(def ClassProto Class.)

(def (constant-slot x) (λ (_ _ _) x))

(.def (Slot @ Class.)
  sexp: 'Slot
  slots:
   {type: {type: Type optional: #t}
    constant: {type: Any optional: #t}
    compute: {type: (Fun Any <- Poo Any (Fun Any <-)) optional: #t} ;; second Any should be (List Poo)
    base: {type: Any optional: #t default: no-such-slot}
    default: {type: Any optional: #t}
    optional: {type: Bool default: #f}
    hidden: {type: Bool default: #f}}
  proto: {.type: @ optional: #f hidden: #f}
  .slot-checker:
    (λ (@@ slot-name base x)
      (with-slots (type constant optional) @@
        (if (.key? x slot-name)
          (let ((value (.ref x slot-name base)))
            (and
              (or (not (.has? @@ type)) (element? type value))
              (or (not (.has? @@ constant)) (equal? constant value))))
          (and (.has? @@ optional) optional))))
  .slot-definer:
    (λ (@@ slot-name x)
       (with-slots (type constant compute default) @@
         (cond
          ((.has? @@ constant) (.putslot! x slot-name (constant-slot constant)))
          ((.has? @@ compute) (.putslot! x slot-name compute))
          ((.has? @@ default) (.putslot! x slot-name (constant-slot default)))
          ((and (.has? @@ type) (.has? type proto))
           (.putslot! x slot-name (constant-slot (.@ type proto)))))
         ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name base self))))
         )))

;; TODO: functional lenses in (.lens foo) as well as imperative accessors
(.def (Lens @ Class.)
  sexp: 'Lens. ;; Lens 's 'a
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

(.def (Type @ Class.)
  sexp: 'Type
  slots: {sexp: {type: Any}
          .element?: {type: (Fun Bool <- Any)}}
  .element?: (λ (x) (and (poo? x) (.has? x sexp) (.has? x .element?)))
  .sexp<-: (lambda (x) (.@ x sexp)) ;; : SEXP <- @
  proto: Type.)

(.def (Class @ Type)
   sexp: 'Class
   slot-descriptor-class: Slot ;; MOP magic!
   slots: =>.+
    {slots: {type: PooPoo} ;; would be (MonomorphicPoo Slot) if we didn't automatically append Slot
     sealed: {type: Bool default: #f}}
   proto: Class.)

(def (instance class . plist) (apply .cc (.@ class proto) plist))

;; TODO: make-instance or new should .instantiate the object.
;; TODO: What name for a syntax that does not instantiate it?
(defrules new ()
  ((_ (class self slots ...) slot-defs ...)
   {(:: self (.@ class proto) slots ...) slot-defs ...})
  ((_ (class) slot-defs ...)
   {(:: self (.@ class proto)) slot-defs ...})
  ((_ class slot-defs ...)
   {(:: self (.@ class proto)) slot-defs ...}))

(defrules .defclass ()
  ((_ (class class-options ...) (slotdefs ...) options ...)
   (.def (class class-options ...) sexp: 'class (slots =>.+ {slotdefs ...}) options ...))
  ((_ class (slotdefs ...) options ...)
   (.defclass (class) (slotdefs ...) options ...)))

(def (slot-lens slot-name)
  {(:: @ (instance Lens))
   .get: (lambda (s) (.ref s slot-name))
   .set: (lambda (x s) (.cc s slot-name x))})

;; For now, types are just runtime descriptors...
(defrule (define-type a desc) (def a (.mix {sexp: 'a} desc)))

;; TODO: a special slot .parameters, and a function that specialized a poo on its positional parameters?
