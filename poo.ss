;;-*- Gerbil -*-
;; Trivial implementation of Prototype Object Orientation in Gerbil Scheme.
;;
;; See doc/poo.md for documentation
;; TODO: see Future Features and the Internals TODO sections in document above.

(export #t)

(import
  ;;(for-syntax :std/misc/repr) :std/misc/repr ;; XXX debug
  :std/lazy :std/misc/hash :std/iter :std/misc/list :std/sort :std/srfi/1 :std/srfi/13 :std/sugar
  (for-syntax :clan/base :std/iter)
  :clan/base :clan/with-id)

;; Type signature of the various components involved in specifying a Poo object.
;; * A is an indexed type that maps symbol slot s to value of dependent type (A s).
;; * Note the final argument s to the SlotSpec is meant for reflection purposes,
;;   as are the arguments provided to the base case;
;;   it not meant to be used in regular methods,
;;   except maybe for error messages and other debugging purposes.
;;
;; (deftype (BaseSpec A s) (Fun (A s) <- (Poo A) s:Symbol))
;; (deftype (SlotSpec A s) (Fun (A s) <- Poo (Listof (Prototype A)) s:Symbol (BaseSpec A s)))
;; (deftype (Prototype A) (Table (SlotSpec A s) <- s:Symbol))
;; (deftype (Instance A) (Table (A s) <- s:Symbol))
;; (deftype (Poo A) (Record prototypes: [(Listof (Prototype A))] instance: [(OrFalse (Instance A))]))

(defstruct poo ;; : (Poo A)
  (prototypes ;; : (Listof (Prototype A))
   instance) ;; : (Instance A)
  constructor: :init!)
(defmethod {:init! poo}
  (lambda (self prototypes (instance #f))
    (struct-instance-init! self prototypes instance)))

;; Ensure that a poo object has a table to hold instance slot values, *and*, in some near future,
;; run any initialization code and assertions associated to the instance.
;; : Unit <- (Poo _)
(def (.instantiate poo.)
  (match poo.
    ((poo _ #f) (set! (poo-instance poo.) (hash))) ;; TODO: call .init method?
    ((poo _ _) (void)) ;; already instantiated
    (else (error "No poo" poo.))))

;; : (Bottom <-) <- (Poo _) Symbol
(def (no-such-slot poo. slot)
  (error "No such slot" poo. slot))

;; For a slot of type A
;; : (A s) <- (Poo A) s:Symbol ?((A s) <-)
(def (.ref poo. slot (base no-such-slot))
  (unless (poo? poo.) (error ".ref: No poo" poo. slot: slot))
  (.instantiate poo.)
  (match poo.
    ((poo prototypes instance)
     (hash-ensure-ref instance slot
                      (cut compute-slot poo. prototypes slot base)))))

;; Computing a slot
;; : (A s) <- (Poo A) (Listof (Prototype A)) s:Symbol (BaseSpec A s)
(def (compute-slot poo. prototypes slot base)
  (match prototypes
    ([] (base poo. slot))
    ([prototype . super-prototypes]
     (if-let (fun (hash-get prototype slot))
        (fun poo. super-prototypes slot base)
        (compute-slot poo. super-prototypes slot base)))))

;; : (BaseSpec A s) <- (A s)
(def (base<-value value) (lambda (_self _slot) value))

;; : (BaseSpec A s) <- ((A s) <-)
(def (base<-thunk thunk) (lambda (_self _slot) (thunk)))

;; Flatten a nested list of prototypes into a single list.
;; The optional argument is the tail of the flattened list so far, to which to prepend the rest.
;; : (Listof (Prototype A)) <- Any ?(Listof (Prototype A))
(def (append-prototypes x (prototypes []))
  (match x ;; TODO: use lazy merging of patricia trees to maximize the sharing of structure? hash-consing?
    ([] prototypes)
    ((cons x y) (append-prototypes x (append-prototypes y prototypes)))
    ((poo ps _) (append ps prototypes))
    (_ (error "invalid poo spec" x))))

;; Combine multiple poos. Leftmost is closer to the instance, rightmost is cloesr to the base.
;; : (Poo A) <- (Poo A) ...
(def (.mix . poos)
  (poo (append-prototypes poos) #f))

;; Combine multiple poos, but put the first one at the end
;; (note: the rest is in the usual order. Maybe we will reverse it in some future.)
;; : (Poo A) <- (Poo A) (Poo A) ...
(def (.+ base . mixins)
  (.mix mixins base))

;; : Bool <- (Poo _) Symbol
(def (.key? poo. slot)
  (match poo.
    ((poo prototypes instance)
     (or (and instance (hash-key? instance slot)) ;; fast check for already-computed slot, also includes slots from .put! or .set!
         (any (cut hash-key? <> slot) prototypes)))
    (else (error ".key?: not poo" poo. slot))))

(defrules .has? ()
  ((_ x) #t)
  ((_ x slot) (.key? x 'slot))
  ((_ x slot1 slot2 slot3 ...) (and (.has? x slot1) (.has? x slot2 slot3 ...))))

;; : (Listof Symbol) <- (Poo _)
(def .all-slots
  (nest
   (λ-ematch) ((poo prototypes instance))
   (with-list-builder (c))
   (let (h (if instance (hash-copy instance) (hash)))
     (when instance (for-each! (hash-keys instance) c)))
   (for-each! prototypes) (λ (p))
   ((cut hash-for-each <> p)) (λ (k _))
   (unless (hash-key? h k)
     (hash-put! h k #t)
     (c k))))

;; : (Listof Symbol) <- (Poo _)
(def (.all-slots-sorted poo)
  (sort (.all-slots poo) symbol<?))

;; : (Listof (Pair s:Symbol (A s))) <- (Poo A)
(def (.alist poo)
  (map (λ (slot) (cons slot (.ref poo slot))) (.all-slots poo)))

;; : (Listof (Pair s:Symbol (A s))) <- (Poo A)
(def (.sorted-alist poo)
  (map (λ (slot) (cons slot (.ref poo slot))) (.all-slots-sorted poo)))

;; Force the lazy computation of all slots in a poo --- assuming no-such-slot as the base for all of them
;; : (Poo A) <- (Poo A)
(def (force-poo poo) (for-each (cut .ref poo <>) (.all-slots poo)) poo)

;; For (? test :: proc => pattern),
;;   test = (o?/keys ks)
;;   proc = (.refs/keys ks)
;; : Bool <- (Poo _) <- (Listof Symbol)
(def ((o?/keys ks) o) (and (poo? o) (andmap (cut .key? o <>) ks)))
;; : (IndexedList ss A) <- (Poo A) <- ss:(Listof Symbol)
(def ((.refs/keys ks) o) (map (cut .ref o <>) ks))

;; : (SlotSpec A s) <- (A s)
(def (constant-slot x) (λ (_ _ _ _) x))

;; : (SlotSpec A s) <- ((A s) <- (A s))
(def (override-slot f)
  (λ (self super-prototypes slot-name base)
    (f (compute-slot self super-prototypes slot-name base))))

;; : (SlotSpec A s) <- ((A s) <- (Lazy (A s)))
(def (lazy-override-slot f)
  (λ (self super-prototypes slot-name base)
    (f (lazy (compute-slot self super-prototypes slot-name base)))))

;; the ctx argument exists for macro-scope purposes
;; TODO: use a syntax-parameter for self instead of the ctx argument?
(defrules .o/ctx ()
  ((_ ctx (:: self) slot-spec ...)
   (poo/slots ctx self [] () slot-spec ...))
  ((_ ctx (:: self super slots ...) slot-spec ...)
   (poo/slots ctx self super (slots ...) slot-spec ...))
  ((_ ctx () slot-spec ...)
   (poo/slots ctx self [] () slot-spec ...))
  ((_ ctx slot-spec ...)
   (poo/slots ctx self [] () slot-spec ...)))

(begin-syntax
  ;; TODO: is there a better option than (stx-car stx) to introduce correct identifier scope?
  ;; the stx argument is the original syntax #'(.o args ...) or #'(@method args ...)
  (def (unkeywordify-syntax ctx k)
    (!> k
        syntax->datum
        keyword->string
        string->symbol
        (cut datum->syntax (stx-car ctx) <>)))

  ;; A NormalizedSlotSpec is one of:
  ;;  - (slot-name value-expr)                           ; ignore parent, override
  ;;  - (slot-name => function-expr extra-arg-expr ...)  ; invoke parent, pass into function
  ;;  - (slot-name (inherited-computation) value-expr)   ; lazy reference to parent
  ;;  - (slot-name)                                      ; same-named var from surrounding scope
  ;;  - (slot-name =>.+ poo-expr)                        ; combine parent with a mixin
  ;; Interpretation according to `doc/poo.md` section `POO Definition Syntax`

  (def (normalize-named-slot-specs ctx name specs)
    (syntax-case specs (=> =>.+)
      ((=> value-spec . more)
       (with-syntax ((name name))
         (cons #'(name => value-spec) (normalize-slot-specs ctx #'more))))
      ((=>.+ value-spec . more)
       (with-syntax ((name name))
         (cons #'(name =>.+ value-spec) (normalize-slot-specs ctx #'more))))
      ((value-spec . more)
       (with-syntax ((name name))
         (cons #'(name value-spec) (normalize-slot-specs ctx #'more))))
      (() (error "missing value after slot name" name (syntax->datum name) ctx (syntax->datum ctx)))))

  (def (normalize-slot-specs ctx specs)
    (syntax-case specs ()
      (() '())
      ((arg . more)
       (let ((e (syntax-e #'arg)))
         (cond
          ((pair? e)
           (cons #'arg (normalize-slot-specs ctx #'more)))
          ((symbol? e)
           (cons #'(arg) (normalize-slot-specs ctx #'more)))
          ((keyword? e)
           (normalize-named-slot-specs ctx (unkeywordify-syntax ctx #'arg) #'more))
          (else
           (error "bad slot spec" #'arg)))))))

  (def (normalize-slot-specs-for-match ctx specs)
    (for/collect ((s (normalize-slot-specs ctx specs)))
      (syntax-case s (=> =>.+)
        ((slot form) #'(slot form))
        ((slot) #'(slot slot))
        ((slot => . _) (raise-syntax-error #f "=> not allowed in patterns" ctx))
        ((slot =>.+ . _) (raise-syntax-error #f "=>.+ not allowed in patterns" ctx))
        ((slot (next-method) form) (raise-syntax-error #f "(inherited-computation) not allowed in patterns" ctx))))))

;; the ctx argument exists for macro-scope purposes
(defsyntax (poo/slots stx)
  (syntax-case stx ()
    ((_ ctx self super (slots ...) . slot-specs)
     (with-syntax ((((slot spec ...) ...) (normalize-slot-specs #'ctx #'slot-specs)))
       #'(poo/init self super (slots ... slot ...) (slot spec ...) ...)))))

(defrules poo/init ()
  ((_ self super slots (slot slotspec ...) ...)
   (poo (cons (hash (slot (poo/slot-init-form self slots slot slotspec ...)) ...)
              (append-prototypes super)) #f)))

(defrules poo/form-named (lambda λ)
  ((_ slot (lambda formals body ...)) (fun (slot . formals) body ...)) ;; L A M B D A
  ((_ slot (λ formals body ...)) (fun (slot . formals) body ...)) ;; unicode Λ
  ((_ slot form) form))

(defrules poo/slot-init-form (=> =>.+)
  ((_ self slots slot form)
   (λ (self super-prototypes slot-name base)
     (with-slots slots self (poo/form-named slot form))))
  ((_ self slots slot => form args ...)
   (λ (self super-prototypes slot-name base)
     (let ((inherited-value (compute-slot self super-prototypes 'slot base)))
       (with-slots slots self (form inherited-value args ...)))))
  ((_ self slots slot =>.+ args ...)
   (poo/slot-init-form self slots slot => .+ args ...))
  ((_ self slots slot (next-method) form)
   (λ (self super-prototypes slot-name base)
     (let ((inherited-value (lazy (compute-slot self super-prototypes 'slot base))))
       (let-syntax ((next-method (syntax-rules () ((_) (force inherited-value)))))
         (with-slots slots self (poo/form-named slot form))))))
  ((_ self slots slot)
   (λ (self super-prototypes slot-name base) slot)))

;;NB: This doesn't work, because of slots that appear more than once.
#;(defrule (with-slots (slot ...) self body ...) (let-id-rule ((slot (.@ self slot)) ...) body ...))

(defrules with-slots ()
  ((_ () self body ...) (begin body ...))
  ((_ (slot slots ...) self body ...)
   (let (poo self)
     (let-id-rule (slot (.@ poo slot)) (with-slots (slots ...) poo body ...)))))

(defrule (def-slots (slot ...) self)
  (begin
    (def poo self)
    (defvalues (slot ...) (values (.@ poo slot) ...))))

;; TODO: have it called with-slots in both cases, but autodetect
;; that the first argument is a keyword or string?
(defrules with-prefixed-slots ()
  ((ctx (prefix slot ...) self body ...)
   (with-prefixed-slots ctx (prefix slot ...) self body ...))
  ((_ ctx (prefix) self body ...) (begin body ...))
  ((_ ctx (prefix slot slots ...) self body ...)
   (with-id/expr ctx ((var #'prefix #'slot))
     (let-id-rule (var (.@ self slot))
       (with-prefixed-slots ctx (prefix slots ...) self body ...)))))

(defrules def-prefixed-slots ()
  ((ctx (prefix slot ...) self)
   (def-prefixed-slots ctx (prefix slot ...) self))
  ((_ ctx (prefix) self) (void))
  ((_ ctx (prefix slot slots ...) self)
   (with-id ctx ((var #'prefix #'slot))
     (def var (.@ self slot))
     (def-prefixed-slots ctx (prefix slots ...) self))))

;; TODO: use defsyntax-for-match, and in the pattern use (? test :: proc => pattern) to do the job
(defsyntax-for-match .o
  (lambda (stx)
    (syntax-case stx ()
      ((_ args ...)
       (with-syntax ((((k v) ...) (normalize-slot-specs-for-match stx #'(args ...))))
        #'(? (o?/keys '(k ...)) :: (.refs/keys '(k ...)) => [v ...])))))
  (lambda (stx)
    (syntax-case stx ()
      ((_ args ...)
       (with-syntax ((ctx stx)) #'(.o/ctx ctx args ...))))))

(defsyntax (.def stx)
  (syntax-case stx ()
    ((_ args ...)
     (with-syntax ((ctx stx)) #'(.def/ctx ctx args ...)))))

;; the ctx argument exists for macro-scope purposes
(defrules .def/ctx ()
  ((_ ctx (name options ...) slot-defs ...)
   (def name (.o/ctx ctx (:: options ...) slot-defs ...)))
  ((_ ctx name slot-defs ...)
   (def name (.o/ctx ctx () slot-defs ...))))

(defrules .get ()
  ((_ poo) poo)
  ((_ poo slot slots ...) (.get (.ref poo 'slot) slots ...)))

(defalias .@ .get)

(defrules .get-set! ()
  ((_ poo slot v) (.put! poo 'slot v))
  ((_ poo slot0 slot1 slots ... v) (.get-set! (.@ poo slot0) slot1 slots ... v)))

(defalias .@-set! .get-set!)

(defrules .call ()
  ((_ poo slot args ...) ((.get poo slot) args ...)))

;; : Unit <- (Poo A) s:Symbol (SlotSpec A s)
(def (.putslot! poo. slot definition)
  (ematch poo. ((poo [proto . protos] _) (hash-put! proto slot definition))))

(defrules .setslot! () ((_ poo. slot value) (.putslot! poo. 'slot value)))

(defrules .def! () ;; TODO: check prototype mutability status first
  ((_ poo slot (slots ...) slotspec ...)
   (.putslot! poo 'slot (poo/slot-init-form poo (slot slots ...) slot slotspec ...))))

;; : Unit <- (Poo A) s:Symbol (A s)
(def (.put! poo. slot value) ;; TODO: check instance mutability status first
  (.instantiate poo.)
  (hash-put! (poo-instance poo.) slot value))

(defrules .set! () ((_ poo. slot value) (.put! poo. 'slot value)))

;; : (Poo A) <- IndexedList ? ((Pair s (A s)) <- s:Symbol)
(def (.<-alist alist)
  (def o (.o))
  (for-each (match <> ([k . v] (.putslot! o k (λ (self super-prototypes slot base) v)))) alist)
  o)

;; carbon copy / clone c...
;; : (Poo A') <- (Poo A) <<TODO: type for overrides from A to A' ...>>
(def (.cc poo. . overrides)
  (def i (cond ((poo-instance poo.) => hash-copy) (else (hash))))
  (def o (poo (poo-prototypes poo.) i))
  (let loop ((l overrides))
    (match l
      ([] (void))
      ([(? symbol? s) v . r] (hash-put! i s v) (loop r))
      ([(? keyword? k) v . r] (hash-put! i (string->symbol (keyword->string k)) v) (loop r))
      (else (error "invalid poo overrides" overrides))))
  o)
;; TODO: a syntax that allows for => / =>.+ overrides as well as setting values.
;; TODO: find an efficient way to repeatedly override one field in a pure way without leaking memory,
;; in O(log n) rather than O(n)?
;; TODO: maybe have explicitly distinct variants of stateful vs pure poo?
;; Could one convert from the other, with some freezing and thawing or copying operations?
;; When overriding a field, should we invalidate all the cached values for all fields?
;; Should we keep an indefinitely growing list of the super formulas?
;; Should we maintain flags as to which formulas do or don't use the super and/or self references,
;; so we know what to invalidate or not?

;; TODO: For type validation,
;; 1. cache which types an instance was tested to be part of,
;; which allows for fast checking of types for recursive data structures,
;; maybe even with cycles.
;; 2. have a mechanism to extend the above to non-instances.
