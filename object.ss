;;-*- Gerbil -*-
;; Prototype Object Orientation Functionally, in Gerbil Scheme.
;;
;; Next version of poo, with multiple inheritance and a budding meta-object protocol.
;; See doc/poo.md for documentation
;; TODO: see Future Features and the Internals TODO sections in document above.

(export #t)

(import
  (for-syntax :clan/base :std/iter :std/misc/hash :std/misc/list)
  :std/lazy :std/misc/hash :std/iter :std/misc/alist :std/misc/list
  :std/sort :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/base :clan/hash :clan/list :clan/syntax :clan/with-id)

;; TODO: formalize (Object A S D) and the type conditions under which an object is instantiatable?
(defstruct object ;; = (Object A)
  (supers ;; : (Listof (Object ?))
   slots ;; : (Listof (Pair Symbol (SlotSpec ?))) ; direct slot methods in order
   defaults ;; : (Listof (Pair Symbol ?)) ; direct slot defaults in order
   %instance ;; : (Table (A_ k) <- k:Sym) ; hash table from slot keys to slot values
   %precedence-list ;; : (Listof (Object ?)) ; linearization of the supers DAG
   %slot-funs ;; : (Table (Fun (A_ k)) <- k:Symbol) ; functions to compute slots
   %all-slots) ;; : (Listof Symbol) ; definition order for all slot keys
  constructor: :init!)
(defmethod {:init! object}
  (lambda (self ;; : (Object A)
      supers: (supers '()) ;; : (Listof Objects) ;; Actually, can be a pair-tree of Object's and nulls.
      slots: (slots '()) ;; : (Listof (Pair Symbol (SlotSpec ? ?)))
      defaults: (defaults '())) ;; : (Listof (Pair Symbol ?))
    (set! (object-supers self) (flatten-pairs supers))
    (set! (object-slots self) slots)
    (set! (object-defaults self) defaults)
    (set! (object-%instance self) #f)
    (set! (object-%precedence-list self) #f)
    (set! (object-%slot-funs self) #f)
    (set! (object-%all-slots self) #f)))

(def (instantiate-object! self)
  (if (object? self)
    (unless (object-%instance self)
      (set! (object-%instance self) (make-hash-table))
      (compute-precedence-list! self)
      (compute-slot-funs! self)
      #;(check-assertions! self)) ;; TODO: allow for instantiation-time assertions
    (error "Not an object" self)))

(def (uninstantiate-object! self)
  (when (object-%instance self)
    (set! (object-%instance self) #f)
    (set! (object-%precedence-list self) #f)
    (set! (object-%slot-funs self) #f)
    (set! (object-%all-slots self) #f)))

(defstruct (InvalidObject Exception) (slots) transparent: #t)
(def (invalid-object-summary self)
  (InvalidObject (map car (append (object-slots self) (object-defaults self)))))

(def (compute-precedence-list! self (heads '()))
  (cond
   ((object-%precedence-list self))
   ((member self heads) => (lambda (l) (error "Circular precedence graph" l)))
   (else
    (for-each (rcurry compute-precedence-list! [self . heads]) (object-supers self))
    (let (precedence-list
          (c3-compute-precedence-list
           self get-supers: object-supers
           get-name: invalid-object-summary
           get-precedence-list: object-%precedence-list))
      (set! (object-%precedence-list self) precedence-list)
      precedence-list))))

(def (compute-slot-funs! self)
  (def h (make-hash-table))
  (def supers (reverse (object-%precedence-list self)))
  ;; Handle defaults
  (for (super supers)
    (for (([slot . value] (object-defaults super)))
      (hash-put! h slot (constantly value))))
  ;; Handle methods
  (for (super supers)
    (for (([slot . spec] (object-slots super)))
      (hash-ensure-modify! h slot
                           (lambda () (cut no-applicable-method self slot))
                           (cut apply-slot-spec self spec <>))))
  (set! (object-%slot-funs self) h))

;; Given a list of list of keys, in precedence order,
;; return a list of keys from containing from left to right
;; all the keys from tail to head of the precedence list, skipping repetitions.
(def (merge-super-slots super-slots)
  (def h (make-hash-table))
  (with-list-builder (c)
    (for-each (lambda (l)
    (for-each (lambda (k)
      (unless (hash-key? h k)
        (hash-put! h k #t) (c k)))
    l)) (reverse super-slots))))

(defstruct $slot-spec () transparent: #t) ;; = (SlotSpec A k)
(defstruct ($constant-slot-spec $slot-spec) (value) transparent: #t) ;; constant value
(defstruct ($thunk-slot-spec $slot-spec) (thunk) transparent: #t) ;; thunk
(defstruct ($self-slot-spec $slot-spec) (fun) transparent: #t) ;; fun to be passed self as argument, not super
(defstruct ($computed-slot-spec $slot-spec) (fun) transparent: #t) ;; fun to be passed self and superfun as arguments

(def (apply-slot-spec self spec superfun)
  (match spec
    (($constant-slot-spec val) (constantly val))
    (($thunk-slot-spec fun) fun)
    (($self-slot-spec fun) (cut fun self))
    (($computed-slot-spec fun) (cut fun self superfun))))

(def (object-instance self)
  (instantiate-object! self)
  (object-%instance self))

(def (.ref self slot)
  (hash-ensure-ref (object-instance self) slot
                   (lambda () ((hash-ref/default (object-%slot-funs self) slot
                                            (cut cut no-applicable-method self slot))))))

;; Get an existing cached slot value from an object
(def (.ref/cached self slot (default false))
  (hash-ref/default (object-instance self) slot default))

(defstruct (NoApplicableMethod Exception) (self slot) transparent: #t)

;; Prototype to put at the end of the list, to handle
;; undefined-prototype-behavior a bit better.
;; Should it send a special message in that case?
(def (no-applicable-method self slot)
  (def nam (and (not (eq? slot 'no-applicable-method))
                (with-catch false (cut .@ self no-applicable-method))))
  (if nam (nam self slot) (raise (NoApplicableMethod self slot))))

(def (object<-alist a supers: (supers '()) defaults: (defaults '()))
  (make-object slots: (map (match <> ([k . v] (cons k ($constant-slot-spec v)))) a)
               supers: supers defaults: defaults))
(def (object<-hash h supers: (supers '()) defaults: (defaults '()))
  (object<-alist (hash->list/sort h symbol<?) supers: supers defaults: defaults))
(def (object<-fun f keys: (keys '()) supers: (supers '()) defaults: (defaults '()))
  (make-object slots: (map (lambda (k) (cons k ($thunk-slot-spec (cut f k)))) keys)
               supers: supers defaults: defaults))
(def (.mix slots: (slots '()) defaults: (defaults '()) . supers)
  (make-object supers: supers slots: slots defaults: defaults))
(def (.+ base . overrides) (.mix overrides base))
(def (extend-object self defaults: (defaults '()) . slots)
  (make-object slots: slots supers: [self] defaults: defaults))

;; : Bool <- (Object _) Symbol
(def (.slot? self slot)
  (instantiate-object! self)
  (hash-key? (object-%slot-funs self) slot))

(defrules .has? ()
  ((_ x) #t)
  ((_ x slot) (.slot? x 'slot))
  ((_ x slot1 slot2 slot3 ...) (and (.has? x slot1) (.has? x slot2 slot3 ...))))

;; : (Listof Symbol) <- (Object _)
(def (.all-slots self)
  (instantiate-object! self)
  (or (object-%all-slots self)
      (let (esl (merge-super-slots
                 (map (lambda (super) (map car (append (object-slots super) (object-defaults super))))
                      (object-%precedence-list self))))
        (set! (object-%all-slots self) esl)
        esl)))

;; : (Listof Symbol) <- (Object _)
(def (.all-slots/sort object) (sort (.all-slots object) symbol<?))

;; : (Listof (Pair s:Symbol (A s))) <- (Object A)
(def (.alist self)
  (map (λ (slot) (cons slot (.ref self slot))) (.all-slots self)))

;; : (Listof (Pair s:Symbol (A s))) <- (Object A)
(def (.alist/sort self)
  (map (λ (slot) (cons slot (.ref self slot))) (.all-slots/sort self)))

;; Force the lazy computation of all slots in an object
;; : (Object A) <- (Object A)
(def (force-object self) (for-each (cut .ref self <>) (.all-slots self)) self)

;; For (? test :: proc => pattern),
;;   test = (o?/slots ks)
;;   proc = (.refs/slots ks)
;; : Bool <- (Object _) <- (Listof Symbol)
(def ((o?/slots slots) o) (and (object? o) (andmap (cut .slot? o <>) slots)))
;; : (IndexedList ss A) <- (Object A) <- ss:(Listof Symbol)
(def ((.refs/slots slots) o) (map (cut .ref o <>) slots))

;; the ctx argument exists for macro-scope purposes
;; TODO: use a syntax-parameter for self instead of the ctx argument?
(defrules .o/ctx ()
  ((_ ctx (:: self) slot-spec ...)
   (object/slots ctx self [] () slot-spec ...))
  ((_ ctx (:: self super slots ...) slot-spec ...)
   (object/slots ctx self super (slots ...) slot-spec ...))
  ((_ ctx () slot-spec ...)
   (object/slots ctx self [] () slot-spec ...))
  ((_ ctx slot-spec ...)
   (object/slots ctx self [] () slot-spec ...)))

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
  ;;  - (slot-name =>.+ object-expr)                        ; combine parent with a mixin
  ;;  - (slot-name ?: default-expr)                      ; default for the slot
  ;; Interpretation according to `doc/poo.md` section `POO Definition Syntax`

  (def (normalize-slot-specs ctx specs)
    (def methods (make-hash-table))
    (def defaults (make-hash-table))
    (def defaults-list '())
    (def (d x) (push! x defaults-list))
    (def slot-methods
      (with-list-builder (c) (%normalize-slot-specs ctx specs methods defaults c d)))
    [slot-methods (reverse defaults-list)])

  (def (%normalize-slot-specs ctx specs methods defaults c d)
    (def (loop more) (%normalize-slot-specs ctx more methods defaults c d))
    (syntax-case specs ()
      (() (void))
      ((arg . more)
       (let ((e (syntax-e #'arg)))
         (cond
          ((pair? e) (%normalize-slot-spec ctx #'arg methods defaults c d) (loop #'more))
          ((symbol? e) (%normalize-slot-spec ctx #'(arg) methods defaults c d) (loop #'more))
          ((keyword? e) (%normalize-named-slot-specs
                         ctx (unkeywordify-syntax ctx #'arg) #'more methods defaults c d))
          (else (raise-syntax-error #f "bad slot spec" #'arg ctx)))))))

  (def (%normalize-slot-spec ctx spec methods defaults c d)
    (def (do-method name more)
      (def sym (syntax-e name))
      (unless (symbol? sym) (raise-syntax-error #f "Slot name not a symbol" ctx name))
      (if (hash-key? methods sym)
        (raise-syntax-error #f "Multiple slot methods specified" name ctx)
        (hash-put! methods sym #t))
      (c [name . more]))
    (def (do-defaults name value)
      (def sym (syntax-e name))
      (unless (symbol? sym) (raise-syntax-error #f "Slot name not a symbol" ctx name))
      (if (hash-key? defaults sym)
        (raise-syntax-error #f "Multiple slot defaults specified" name ctx)
        (hash-put! defaults sym #f))
      (d [name value]))
    (syntax-case spec (?)
      ((name value) (do-method #'name #'(value)))
      ((name ? value) (do-defaults #'name #'value))
      ((name ? . more) (raise-syntax-error #f "Invalid slot default specified" ctx #'name))
      ((name . more) (do-method #'name #'more))))

  (def (%normalize-named-slot-specs ctx name specs methods defaults c d)
    (def (loop spec more)
      (%normalize-slot-spec ctx spec methods defaults c d)
      (%normalize-slot-specs ctx more methods defaults c d))
    (with-syntax ((name name))
      (syntax-case specs (=> =>.+ ?)
        ((=> value-spec . more) (loop #'(name => value-spec) #'more))
        ((=>.+ value-spec . more) (loop #'(name =>.+ value-spec) #'more))
        ((? value-spec . more) (loop #'(name ? value-spec) #'more))
        ((value-spec . more) (loop #'(name value-spec) #'more))
        (() (raise-syntax-error #f "missing value after slot name" #'name (syntax->datum #'name) ctx (syntax->datum ctx))))))

  (def (normalize-slot-specs-for-match ctx specs)
    (with ([slot-methods defaults] (normalize-slot-specs ctx specs))
      (unless (null? defaults) (raise-syntax-error #f "? not allowed in patterns" ctx))
      (for/collect ((s slot-methods))
        (syntax-case s (=> =>.+ ?)
          ((slot form) #'(slot form))
          ((slot) #'(slot slot))
          ((slot ? . _) (raise-syntax-error #f "? not allowed in patterns" ctx))
          ((slot => . _) (raise-syntax-error #f "=> not allowed in patterns" ctx))
          ((slot =>.+ . _) (raise-syntax-error #f "=>.+ not allowed in patterns" ctx))
          ((slot (next-method) form) (raise-syntax-error #f "(inherited-computation) not allowed in patterns" ctx)))))))

;; the ctx argument exists for macro-scope purposes
(defsyntax (object/slots stx)
  (syntax-case stx ()
    ((_ ctx self super (slots ...) . slot-specs)
     (with-syntax (((((slot spec ...) ...)
                     ((default-slot . default-value) ...))
                    (normalize-slot-specs #'ctx #'slot-specs)))
       #'(object/init self super (slots ... default-slot ... slot ...)
                      ((default-slot . default-value) ...)
                      (slot spec ...) ...)))))

(defrule (object/defaults (default-slot default-value) ...)
  (list (cons 'default-slot default-value) ...))

(defrule (object/init self super slots ((default-slot default-value) ...) (slot slotspec ...) ...)
  (make-object
   supers: super
   slots: (list (cons 'slot (object/slot-spec self slots slot slotspec ...)) ...)
   defaults: (object/defaults (default-slot default-value) ...)))

(defrules object/slot-spec (=> =>.+)
  ((_ self slots slot form)
   ($self-slot-spec (lambda (self)
    (%with-slots slots self form))))
  ((_ self slots slot => form args ...)
   ($computed-slot-spec (lambda (self superfun)
    (%with-slots slots self (form (superfun) args ...)))))
  ((_ self slots slot =>.+ args ...)
   (object/slot-spec self slots slot => .+ args ...))
  ((_ self slots slot (next-method) form)
   ($computed-slot-spec (lambda (self superfun)
     (let (inherited-value (lazy (superfun)))
       (let-syntax ((next-method (syntax-rules () ((_) (force inherited-value)))))
         (%with-slots slots self form))))))
  ((_ self slots slot)
   ($constant-slot-spec slot)))

;;NB: This doesn't work, because of slots that appear more than once.
#;(defrule (with-slots (slot ...) self body ...) (let-id-rule ((slot (.@ self slot)) ...) body ...))

(defrules %with-slots ()
  ((_ () self body ...) (begin body ...))
  ((_ (slot slots ...) self body ...)
   (let-id-rule (slot (.@ self slot)) (with-slots (slots ...) self body ...))))

(defrules with-slots ()
  ((_ () self body ...) (begin body ...))
  ((_ (slots ...) self body ...) (let (object self) (%with-slots (slots ...) object body ...))))

(defrule (def-slots (slot ...) self)
  (begin
    (def object self)
    (defvalues (slot ...) (values (.@ object slot) ...))))

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
        #'(? (o?/slots '(k ...)) :: (.refs/slots '(k ...)) => [v ...])))))
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
  ((_ object) object)
  ((_ object slot slots ...) (.get (.ref object 'slot) slots ...)))

(defalias .@ .get)

(defrules .get-set! ()
  ((_ object slot v) (.put! object 'slot v))
  ((_ object slot0 slot1 slots ... v) (.get-set! (.@ object slot0) slot1 slots ... v)))

(defalias .@-set! .get-set!)

(defrules .call ()
  ((_ object slot args ...) ((.get object slot) args ...)))

;; : Unit <- (Object A) s:Symbol (SlotSpec A s)
(def (.putslot! self slot slot-spec)
  (uninstantiate-object! self)
  (modify! (object-slots self)
           (lambda (slot-specs)
             (if (find (looking-for slot key: car) slot-specs)
               (aset slot-specs slot slot-spec)
               (append slot-specs [[slot . slot-spec]]))))) ;; add the spec at the end

(def (.putdefault! self slot default)
  (uninstantiate-object! self)
  (modify! (object-defaults self)
           (lambda (defaults)
             (if (find (looking-for slot key: car) defaults)
               (aset defaults slot default)
               (append defaults [[slot . default]]))))) ;; add the spec at the end

(defrules .setslot! () ((_ object. slot slot-spec) (.putslot! object. 'slot slot-spec)))

(defrules .def! () ;; TODO: check prototype mutability status first. Also, invalidate subobjects???
  ((_ object slot (slots ...) slotspec ...)
   (.putslot! object 'slot (object/slot-spec object (slot slots ...) slot slotspec ...))))

;; Side-effect the value of a field into an object.
;; Assumes the top-object-proto was used.
;; : Unit <- (Object A) s:Symbol (A s)
(def (.put! self slot value) ;; TODO: check instance mutability status first (?)
  (hash-put! (object-instance self) slot value))

(defrules .set! () ((_ object. slot value) (.put! object. 'slot value)))

;; : (Object A) <- IndexedList ? ((Pair s (A s)) <- s:Symbol)
(def (.<-alist alist)
  (def o (.o))
  (for-each (match <> ([k . v] (.putslot! o k (λ (self super-prototypes slot base) v)))) alist)
  o)

;; carbon copy / clone c...
;; : (Object A') <- (Object A) <<TODO: type for overrides from A to A' ...>>
(def (.cc self . overrides)
  (def added-hash (make-hash-table))
  (def added
    (with-list-builder (c)
      (def (add-slot! slot value)
        (when (hash-key? added-hash slot) (error "Cannot override slot twice" self slot))
        (hash-put! added-hash slot ($constant-slot-spec value))
        (c slot))
      (let loop ((l overrides))
        (match l
          ([] (void))
          ([(? symbol? s) v . r] (add-slot! s v) (loop r))
          ([(? keyword? k) v . r] (add-slot! (symbolify k) v) (loop r))
          (else (error "invalid object overrides" overrides))))))
  (def slots
    (with-list-builder (c)
      (for ((ss (object-slots self)))
        (def slot (car ss))
        (cond
         ((hash-get added-hash slot) => (lambda (spec) (hash-remove! added-hash slot) (c [slot . spec])))
         (else (c ss))))
      (for (slot added)
        (awhen (spec (hash-get added-hash slot))
          (hash-remove! added-hash slot) (c [slot . spec])))))
  (make-object slots: slots supers: (object-supers self) defaults: (object-defaults self)))

;; TODO: a syntax that allows for => / =>.+ overrides as well as setting values.
;; TODO: find an efficient way to repeatedly override one field in a pure way without leaking memory,
;; in O(log n) rather than O(n)?
;; TODO: maybe have explicitly distinct variants of stateful vs pure object?
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
