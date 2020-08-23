;;; Pure Big Endian Patricia Trees (Tries), mapping natural integers to values.
;;; See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
;;; http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/error :std/format :std/iter :std/sugar
  :clan/base :clan/option :clan/number
  ./poo ./mop ./brace ./number ./type ./fun ./io ./table)

#;(import :clan/debug :clan/exception :gerbil/gambit/threads :gerbil/gambit/continuations)
#;(defrule (aver cond msg expr ...)
  (unless cond
    (DBG msg expr ...)
    (with-catch/cont
     (lambda (_ k)
       (eprintf "In thread ~a:\n" (thread-name (current-thread)))
       (display-continuation-backtrace k (current-error-port) #t #t 20 20))
     (cut error "foo"))
    (##repl-debug)))

(defstruct $Trie () transparent: #t)
(defstruct (Empty $Trie) () transparent: #t)
(defstruct (Leaf $Trie) (value) transparent: #t)
(defstruct (Branch $Trie) (height left right) transparent: #t) ;; height of the top bit, not integer-length
(defstruct (Skip $Trie) (height bits-height bits child) transparent: #t) ;; bits-height is similarly top bit of bits, the number of valid bits in bits is (1+ bits-height)

(defstruct $Step () transparent: #t)
(defstruct (BranchStep $Step) (branch) transparent: #t)
(defstruct (SkipStep $Step) (bits-height) transparent: #t)

;; The height in a Costep (context for applying a step) is the height of the current trie so far,
;; *before* next step is taken. It is -1 if a leaf, the length of the key *minus one* for another node,
;; thus 0 for a branch whose bit of decision is 1, h for a branch whose bit of decision is 1<<h.
;; Note that steps may be applied to an empty trie, whose height is undefined outside of such a context,
;; and that the height of a Costep may itself be #f (or -inf.0, or whatever it is for .empty),
;; when the Costep is itself the result of trying to find a path from the empty trie.
;; The key in a Costep is the bits *above* the height, to be multiplied by 2**(height+1),
;; to get the actual key of the start of the current trie,
;; i.e. prepend those Costep bits as high bits to the bits provided by the subtrie-in-focus as low bits.
(defstruct $Costep (height key) transparent: #t) ; height: (Or Height '(-1 #f)) key: Key
(defstruct $Unstep (left right skip) transparent: #t) ;; left: (Fun trunk <- Key Height trunk branch) right: (Fun trunk <- Key Height branch trunk) skip: (Fun trunk <- Key Height Height Key trunk) ;; the first argument, key, only contains the high bits and must be shifted by 1+ the second argument Height, to get the full key for the first element of the trie node resulting from unstepping. ;; trunk is conceptually branch plus optional path-dependent information
(defstruct $Path (costep steps) transparent: #t) ; costep: Costep steps: (List (Step t))

(.def (Trie. @ [Wrap. methods.table] ;; @ <: (Wrap T)
       .validate ;; : @ <- Any
       .wrap ;; : (Wrap t) <- t
       .unwrap) ;; : t <- (Wrap t)

  sexp: 'Trie.

  Wrapper: Identity

  ;; The type of values stored in a Trie.
  ;; ... except that some binary methods deal with two different value types;
  ;; we then write @[a/Value] to denote a Trie where Value has been overriden with a (type variable)
  Value: Any ;; : Type

  ;; The type of keys used to index the trie. It should be a subtype of Nat.
  ;; The Trie construction could be conceivably extended to handle signed Integer's,
  ;; with a specially handled sign bit, but this adds complication, and may better be done as
  ;; as separate layer on top: two of these Nat Tries, one for non-negative keys,
  ;; the other of negative keys.
  Key: Nat ;; or e.g. UInt256 for Ethereum

  ;; The Height of a node is the bit-set? index of its top key bit,
  ;; which is 1- the integer-length of the key.
  ;; This allows the height of a skip node to always fit in a UInt8, when Key is UInt256.
  ;; Leaves thus have height -1, which is not stored.
  ;; Empty has height #f, also not stored -- or should we instead use -inf.0 ?
  Height: Nat ;; : Type = (Log Key 2) =/default Nat, i.e. Height=UInt8 for Key=UInt256

  ;; The underlying, unwrapped, Trie type.
  ;; A wrapper layer can add cached digests, lazy-loading, maybe cached count, etc.
  #;T: #;(Sum ;; the wrapped type
      (Empty)
      (Leaf value: Value)
      (Branch height: Height left: T right: T)
      (Skip height: Height bits-height: Height bits: Key child: T))

  T: {(:: @T [methods.bytes<-marshal Type.])
      sexp: `(.@ ,(.@ @ sexp) T)
      .element?: $Trie?
      ;; TODO: autogenerate that
      .marshal:
      (lambda (v port)
        (match v
          ((Empty)
           (write-byte 0 port))
          ((Leaf v)
           (write-byte 1 port) (marshal Value v port))
          ((Branch h l r)
           (write-byte 2 port) (marshal Height h port)
           (marshal @ l port) (marshal @ r port))
          ((Skip h bh b c)
           (write-byte 3 port) (marshal Height h port) (marshal Height bh port)
           (marshal Key b port) (marshal @ c port))))
      ;; TODO: autogenerate that for sum types
      .unmarshal:
      (lambda (port)
        (match (read-byte port)
          (0 (Empty))
          (1 (Leaf (unmarshal Value port)))
          (2 (left-to-right Branch (unmarshal Height port)
                            (unmarshal @ port) (unmarshal @ port)))
          (3 (left-to-right Skip (unmarshal Height port) (unmarshal Height port)
                            (unmarshal Key port) (unmarshal @ port)))))}

  ;; An (Unstep @ @) provides methods to undo zipping steps, operating on smaller @ tries
  ;; to yield larger tries. With parameters trunk and branch other than @, an Unstep can be used
  ;; to compute on abstractions of tries, e.g. on their merkleized digests.
  Unstep: {(:: @U Type.)  ;; : Type <- Type Type ;; actually, a two-parameter type
           ;; (lambda (trunk branch) (Struct left: (Fun trunk <- Key Height trunk branch)
           ;;                    right: (Fun trunk <- Key Height branch trunk)
           ;;                    skip: (Fun trunk <- Key Height Height Key trunk)))
           sexp: `(.@ ,(.@ @ sexp) Unstep)
           .element?: $Unstep?
           ;; : (Unstep a a) <- branch:(Fun a <- Key Height a a) skip:(Fun a <- Key Height Height Key a)
           .symmetric: (lambda (branch: branch skip: skip) ($Unstep branch branch skip))
           .up: (.symmetric branch: (lambda (_ h l r) (.make-branch h l r))
                            skip: (lambda (_ h l b c) (.make-skip h l b c)))}

  .sexp<-:
  (lambda (t) (match (.unwrap t)
           ((Empty) '(E .empty))
           ((Leaf v) `(F .leaf ,(sexp<- Value v)))
           ((Branch h l r) `(F .branch ,h ,(.sexp<- l) ,(.sexp<- r)))
           ((Skip h bh b c) `(F .branch ,h ,bh ,b ,(.sexp<- c)))))

  ;; a zipping (Step @) describes how a smaller trie was extracted from a slightly larger trie,
  ;; an can be undone to recover a larger trie from the same smaller trie or a modified variant thereof.
  ;; More generally, a (Step branch) lets you work on an abstraction of a trie, e.g. its merkleization.
  Step: {(:: @S Type.) ;;(lambda (t) (Sum (BranchStep branch: t) (SkipStep bits-height: Height)))
    sexp: `(.@ ,(.@ @ sexp) Step)
    .element?: $Step?
    ;; Given a record of (Unstep trunk branch) methods, a (Step branch) operates on a (Pair trunk Costep)
    ;; : (forall (trunk branch)
    ;;      (Pair trunk Costep) <- (Unstep trunk branch) (Step branch) (Pair trunk Costep))
    .op: (lambda (unstep step acc)
           (let-match ((cons trie ($Costep height key)) acc)
             (unless height (error "Trie.Step.op: height must be non-empty for a non-empty path"))
             (match step
               ((BranchStep branch)
                (let* ((h (1+ height)) ;; new height after un-branching
                       (upkey (arithmetic-shift key -1)))
                  (cons (if (bit-set? 0 key)
                          (($Unstep-right unstep) upkey h branch trie)
                          (($Unstep-left unstep) upkey h trie branch))
                        ($Costep h upkey))))
               ((SkipStep bh)
                (let* ((l (1+ bh))
                       (h (+ height l)) ;; new height after un-skipping
                       (bits (extract-bit-field l 0 key))
                       (upkey (arithmetic-shift key (- l))))
                  (cons (($Unstep-skip unstep) upkey h bh bits trie) ($Costep h upkey)))))))

    .up: (let (up (.@ Unstep .up)) (lambda (step acc) (.op up step acc)))
    ;; Can be used to trivially merkleize a step.
    ;; : (Step a) <- (Fun a <- b) (Step b)
    .map: (lambda (f s) (match s ((BranchStep branch) (BranchStep (f branch))) ((SkipStep _) s)))}

  Costep: {(:: @C Type.) ;;(Struct height: Height key: Key)
    sexp: `(.@ ,(.@ @ sexp) Costep)
    .validate:
    (lambda (x ctx)
      (def c [[validate: x] . ctx])
      (match x
        (($Costep height key)
         (unless (member height '(-1 #f))
           (validate Height height c))
         (validate Key key)))
      x)}

  Path: {(:: @P Type.) ;;(lambda (t) (Struct costep: Costep steps: (List (Step t))))
    sexp: (if (eq? A @) `(.@ ,(.@ @ sexp) Path) `(.o (:: @P (.@ ,(.@ @ sexp) Path)) A: ,(.@ A sexp)))
    A: @ ;; type parameter for the content of the path attribute
    ;; TODO: sexp<- json<- bytes<- <-json <-bytes
    ;;.sexp<-: (lambda (x) `($Path ,(sexp<- Costep ($Path-costep x)) (@list ,@(map (.@ (.+ Step {A}) .sexp<-) ($Path-steps x)))))
    ;; TODO: have validate function and protocol with better error messages?
    ;; validate wrt the parameter type?
    ;; : Bool <- Any
    .validate:
    (lambda (path ctx)
      (let-match (($Path (and costep ($Costep height key)) steps) path)
        (def ctx2 [[validate: path] . ctx])
        (validate Costep costep ctx2)
        (let c ((height height) (steps steps))
          (match steps
            ([] #t)
            ([step . steps]
             (match step
               ((BranchStep a)
                (validate A a ctx2)
                (c (1+ height) steps))
               ((SkipStep bits-height)
                (validate Height bits-height ctx2)
                (let (new-height (+ height bits-height 1))
                  (validate Height new-height ctx2)
                  (c new-height steps))))))))
      path)

    ;; : (Pair trunk Costep) <- (Unstep trunk branch) trunk (Path branch)
    .op: (let (apply-step (.@ Step .op))
           (lambda (unstep t path)
             (let-match (($Path costep steps) path) (let-match (($Costep height _) costep)
               (foldl (cut apply-step unstep <> <>) (cons t costep) steps)))))

    ;; : (Pair trunk Costep) <- (Unstep trunk branch) trunk (Path branch)
    .op: (let (apply-step (.@ Step .op))
           (lambda (unstep t path)
             (let-match (($Path costep steps) path) (let-match (($Costep height _) costep)
               (foldl (cut apply-step unstep <> <>) (cons t costep) steps)))))

    ;; : (Pair @ Costep) <- @ (Path @)
    .up: (let (up (.@ Unstep .up))
           (lambda (t path)
             (def h (.trie-height t))
             (def ph ($Costep-height ($Path-costep path)))
             (when (and h ph (> h ph)) (invalid sexp '(Path .up) t path))
             (.op up (.ensure-height ph t) path)))

    .map: ;; : (Path a) <- (a <- b) (Path b)
    (let (step-map (.@ Step .map))
      (lambda (f path) (def f/step (cut step-map f <>))
         ($Path ($Path-costep path) (map f/step ($Path-steps path)))))}

  ;; Given a Branch at given height and key for its lowest binding,
  ;; return the key for the lowest binding of its right branch
  ;; (for its left branch, the key is the same).
  ;; : Key <- Height Key
  .right-key: (lambda (height key) (replace-bit-field 1 height 1 key))

  ;; Given a Skip at given height, with given length and bits,
  ;; and given the key for the lowest binding it covers,
  ;; return the key for the lowest binding covered by its child.
  ;; : Key <- Height Height Key Key
  .skip-key: (lambda (height bits-height bits key)
               (replace-bit-field (1+ bits-height) (- height bits-height) bits key))

  ;; Describes a recursive computation over a tree in great generality.
  ;; : (Fun o <-
  ;;    Key ;; Prefix key of node
  ;;    @ ;; the input tree.
  ;;    recurse: (Fun o <- Key @) ;; recurse into the key-th nodes of a tree
  ;;    branch: (Fun o <- Key Height o o) ;; recurse down both branches from node
  ;;     ;; at specified key, height, with synthesized values from left and right subnodes.
  ;;    skip: (Fun o <- Key Height Height Key o) ;; recurse down a skip node
  ;;    leaf: (Fun o <- Key Value)
  ;;    empty: (Fun o <-)) ;; NB: only called at top-level, no need for Key parameter. Use a constant?
  .recurse/trie:
  (lambda (key t recurse: recurse branch: branch skip: skip leaf: leaf empty: empty)
    (match (.unwrap t)
      ((Empty) (empty))
      ((Leaf value) (leaf key value))
      ((Branch height left right)
       (left-to-right branch key height
                      (recurse key left) (recurse (.right-key height key) right)))
      ((Skip height bits-height bits child)
       (let (child-key (.skip-key height bits-height bits key))
         (skip key height bits-height bits (recurse child-key child))))))

  ;; : @
  .empty: (.wrap (Empty))

  ;; Is this trie empty, i.e. having no mapping from key to value?
  ;; : Bool <- @
  .empty?: (lambda (x) (equal? (.unwrap x) (Empty)))

  ;; : (Or Height '#f '-1) <- T
  .trie-height:
  (lambda (t)
    (match (.unwrap t)
      ((Empty) #f)
      ((Leaf _) -1)
      ((Branch height _ _) height)
      ((Skip height _ _ _) height)))

  ;; : T <- (Maybe Height) T
  .ensure-height:
  (lambda (h t)
    (if h
        (let (th (.trie-height t))
          (if (and th (< th h))
            (.make-skip h (- h th 1) 0 t)
            t))
        t))

  ;; : T T <- T T
  .ensure-same-height:
  (lambda (ta tb) (values (.ensure-height (.trie-height tb) ta)
                     (.ensure-height (.trie-height ta) tb)))

  ;; : Bool <- Any
  .validate:
  (lambda (t (ctx '()))
    (match (.unwrap t)
      ((Empty) t) ;; NB: In a normalized trie, Empty only happens at the toplevel:
      ;; Otherwise, a Branch with an Empty child is normalized to a Skip
      ((Leaf value) (validate Value value) t)
      ((Branch height left right)
       (let (c [[validate: @ t] . ctx])
         (validate Height height c)
         (.validate left c)
         (or (= (1- height) (.trie-height left)) (type-error c wrong-left-height:))
         (.validate right c)
         (or (= (1- height) (.trie-height right)) (type-error c wrong-right-height:)))
       t)
      ((Skip height bits-height bits child)
       (let (c [[validate: @ t] . ctx])
         (validate Height height c)
         (validate Height bits-height c)
         (or (<= bits-height height) (type-error c bad-bits-height:))
         (or (<= 0 bits) (type-error c negative-bits:))
         (or (<= (integer-length bits) (1+ bits-height)) (type-error c bits-too-big:))
         (.validate child c)
         (or (= (- height bits-height 1) (.trie-height child)) (type-error c child-height-mismatch:)))
       t)))

  ;; : (Fun Value <- @ Key (Fun Value <-))
  .ref:
  (lambda (trie key (default undefined))
    (let/cc return
      (def h (.trie-height trie))
      (unless h (return (default))) ;; catch the empty case on top
      (when (< h (1- (integer-length key))) (return (default)))
      (let r ((height h) (trie trie))
        (match (.unwrap trie)
          ((Leaf v) v)
          ((Branch _ left right)
           (r (1- height) (if (bit-set? height key) right left)))
          ((Skip _ bits-height bits child)
           (let* ((length (1+ bits-height))
                  (child-height (- height length)))
               (if (= bits (extract-bit-field length (1+ child-height) key))
                 (r child-height child)
                 (default))))))))

  ;; : (Fun @ <- Value)
  .leaf: (lambda (v) (.wrap (Leaf v)))

  ;; : (Fun @ <- Height @ @)
  .branch:
  (lambda (height left right)
    #;(aver (and (element? Height height)
               (equal? (1- height) (.trie-height left))
               (equal? (1- height) (.trie-height right)))
          "bad .branch" height left right)
    (.wrap (Branch height left right)))

  ;; : (Fun @ <- Key Height Height @)
  .skip:
  (lambda (height bits-height bits child)
    #;(aver (and (element? Height height)
               (element? Height bits-height)
               (element? Key bits)
               (<= (integer-length bits) (1+ bits-height))
               (equal? (- height bits-height 1) (.trie-height child)))
          "bad .skip" height bits-height bits child)
    (.wrap (Skip height bits-height bits child)))

  ;; Higher-level trie constructors, normalizing the skip cases
  ;; : (Fun @ <- Height Key Value)
  .make-leaf: (lambda (height key value)
                (if (> 0 height) (.leaf value)
                    (.skip height height (extract-bit-field (1+ height) 0 key) (.leaf value))))

  ;; : (Fun @ <- @ @ Height)
  .make-branch: (lambda (height left right)
                  (cond
                   ((.empty? right) (.make-skip height 0 0 left))
                   ((.empty? left) (.make-skip height 0 1 right))
                   (else (.branch height left right))))

  ;; : (Fun @ <- Height Height Key @)
  .make-skip:
  (lambda (height bits-height bits child)
    (if (> 0 bits-height) child
        (let* ((length (1+ bits-height))
               (bits (extract-bit-field length 0 bits))) ;; normalize bits to desired size, if needed
          (match (.unwrap child)
            ((Empty) .empty) ;; NB: return the constant wrapped variant.
            ((Skip _ bits-height1 bits1 child1)
             (let* ((length1 (1+ bits-height1))
                    (bits-height2 (+ bits-height length1))
                    (bits2 (replace-bit-field length length1 bits bits1)))
               (.skip height bits-height2 bits2 child1)))
            (_ (.skip height bits-height bits child))))))

  ;; Normalize the head of a trie, removing any unneeded skipping of zeroes
  ;; : (Fun @ <- @)
  .make-head:
  (lambda (trie)
    (match (.unwrap trie)
      ((Skip height bits-height bits child)
       (let (bh (1- (integer-length bits)))
         (if (< bh bits-height)
           (.make-skip (- height (- bits-height bh)) bh bits child)
           trie)))
      (_ trie)))

  ;; : (Fun @ <- Key Value)
  .singleton: (lambda (key value) (.make-leaf (1- (integer-length key)) key value))

  ;; (Zipper Trunk Branch) = (Pair Trunk Branch)
  ;; : (Zipper @) <- Key Value (Zipper @)
  .zipper-acons:
  (lambda (key value zipper)
    (match (.refocus ($Costep -1 key) zipper)
      ((cons t path) (cons (.leaf value) path))))

  ;; : @ <- Key Value @
  .acons:
  (lambda (key value trie)
    (match (.refocus ($Costep -1 key) (.zipper<- trie))
      ((cons t path)
       (match (.unwrap t)
         ((Leaf (? (cut eqv? value <>))) trie) ;; value unchanged
         (_ (.<-zipper (cons (.leaf value) path)))))))

  ;; : @ <- @ Key
  .remove: (lambda (trie key) (.make-head (.remove-from-subtrie trie key)))

  ;; : @ <- @ Key
  .remove-from-subtrie:
  (lambda (trie key)
    (let/cc return
      (def h (.trie-height trie))
      (unless h (return trie)) ;; key is absent from empty trie; return unchanged empty trie
      (let r ((h h) (t trie))
        (match (.unwrap t)
          ((Leaf) .empty) ;; found an entry for the key, substitute the empty trie
          ((Branch _ left right)
           (let (child-height (1- h))
             (if (bit-set? h key)
               (.make-branch h left (r child-height right))
               (.make-branch h (r child-height left) right))))
          ((Skip _ bits-height bits child)
           (if (= bits (extract-bit-field (1+ bits-height) (- h bits-height) key))
             (.make-skip h bits-height bits (r (- h bits-height 1) child))
             (return trie))))))) ;; key is in skipped branch, return unchanged trie

  ;; : @[Val2/Value] <- (Fun Val2 <- Value) @
  .map: (lambda (f trie)
          (match (.unwrap trie)
            ((Empty) .empty)
            ((Leaf value) (.leaf (f value)))
            ((Branch height left right)
             (left-to-right .branch height (.map f left) (.map f right)))
            ((Skip height bits-height bits child)
             (.skip height bits-height bits (.map f child)))))

  ;; : @[Val2/Value] <- (Fun Val2 <- Key Value) @
  .map/key:
  (lambda (f t (k 0))
    (let m ((k k) (t t))
      (match (.unwrap t)
        ((Empty) .empty)
        ((Leaf value) (.leaf (f k value)))
        ((Branch height left right)
         (left-to-right .branch height (m k left) (m (.right-key height k) right)))
        ((Skip height bits-height bits child)
         (.skip height bits-height bits (m (.skip-key height bits-height bits k) child))))))

  ;; : @[Val2/Value] <- (Fun (Option Val2) <- Key Value) @
  .map/key/opt: ;; note: only map over present keys, possibly removing them.
  (lambda (f t (k 0))
    (let m ((k k) (t t))
      (match (.unwrap t)
        ((Empty) .empty)
        ((Leaf v) (.leaf<-opt (f k v)))
        ((Branch height left right)
         (left-to-right .make-branch height (m k left) (m (.right-key height k) right)))
        ((Skip height bits-height bits child)
         (.make-skip height bits-height bits (m (.skip-key height bits-height bits k) child))))))

  ;; Fold the key-values, starting from the low keys up.
  ;; : (Fun o <- (Fun o <- Key Value o) o @ ?Key)
  .foldl:
  (lambda (f acc trie (key 0))
    (let rec ((k key) (t trie) (a acc))
      (match (.unwrap t)
        ((Empty) a)
        ((Leaf v) (f k v a))
        ((Branch height left right)
         (rec (.right-key height k) right (rec k left a)))
        ((Skip height bits-height bits child)
         (rec (.skip-key height bits-height bits k) child a)))))

  ;; Fold the key-values, starting from the high keys down.
  ;; : (Fun o <- (Fun o <- Key Value o) o @ ?Key)
  .foldr:
  (lambda (f acc trie (key 0))
    (let rec ((k key) (t trie) (a acc))
      (match (.unwrap t)
        ((Empty) a)
        ((Leaf v) (f k v a))
        ((Branch height left right)
         (rec k left (rec (.right-key height k) right a)))
        ((Skip height bits-height bits child)
         (rec (.skip-key height bits-height bits k) child a)))))

  ;; : @ <- (Fun Bool <- Key Value) @
  .filter:
  (lambda (pred t)
    (first-value
     (let r ((k 0) (t t))
       (match (.unwrap t)
         ((Empty) (values .empty #t))
         ((Leaf v) (if (pred k v) (values t #t) (values .empty #f)))
         ((Branch height left right)
          (let-values (((newleft sameleft) (r k left))
                       ((newright sameright) (r (.right-key height k) right)))
            (if (and sameleft sameright) (values t #t)
                (values (.make-branch height newleft newright) #f))))
         ((Skip height bits-height bits child)
          (let-values (((newchild samechild) (r (.skip-key height bits-height bits k) child)))
            (if samechild (values t #t) (values (.make-skip height bits-height bits newchild) #f))))))))

  ;; : (Fun Nat <- @)
  .count: (lambda (trie)
            (match (.unwrap trie)
              ((Empty) 0)
              ((Leaf _) 1)
              ((Branch _ left right) (+ (.count left) (.count right)))
              ((Skip _ _ _ child) (.count child))))

  ;; Binary search given a monotonic predicate f that is #f then #t.
  ;; This would be more efficient on non-random sparse tries if the wrapper kept a count,
  ;; so we could balance the search.
  ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @
  .find-first/opt:
  (lambda (pred trie)
    (letrec ((divide ;; (Option (Cons Key Value)) <- Key (Option (Cons Key Value)) (List (Cons Key @)) (List (Cons Key @)) @
              (lambda (k default leftward rightward t)
                (match (.unwrap t)
                  ((Empty) default)
                  ((Leaf value) (conquer k value default leftward rightward))
                  ((Branch height left right)
                   (match leftward
                     ([] (divide (.right-key height k) default
                                 [[k . left]] rightward right))
                     (_ (divide k default
                                leftward [[(.right-key height k) . right] . rightward] left))))
                  ((Skip height bits-height bits child)
                   (divide (.skip-key height bits-height bits k) default leftward rightward child)))))
             (conquer
              (lambda (k value default leftward rightward)
                (let-values (((new-default tries)
                              (if (pred k)
                                (values (some [k . value]) leftward)
                                (values default (reverse rightward)))))
                  (match tries
                    ([] new-default)
                    ([[k . trie] . leftward] (divide k default leftward [] trie)))))))
      (divide 0 #f [] [] trie)))

  ;; Binary search given a monotonic predicate f that is #t then #f.
  ;; This would be more efficient on non-random sparse tries if the wrapper kept a count,
  ;; so we could balance the search.
  ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @
  .find-last/opt:
  (lambda (pred trie)
    (letrec ((divide ;; (Option (Cons Key Value)) <- Key (Option (Cons Key Value)) (List (Cons Key @)) (List (Cons Key @)) @
              (lambda (k default leftward rightward t)
                (match (.unwrap t)
                  ((Empty) default)
                  ((Leaf value) (conquer k value default leftward rightward))
                  ((Branch height left right)
                   (match rightward
                     ([] (divide k default leftward [[(.right-key height k) . right]] left))
                     (_ (divide k default [[k . left] . leftward] rightward right))))
                  ((Skip height bits-height bits child)
                   (divide (.skip-key height bits-height bits k) default leftward rightward child)))))
             (conquer
              (lambda (k value default leftward rightward)
                (let-values (((new-default tries)
                              (if (pred k)
                                (values (some [k . value]) rightward)
                                (values default (reverse leftward)))))
                  (match tries
                    ([] new-default)
                    ([[k . trie] . rightward] (divide k default [] rightward trie)))))))
      (divide 0 #f [] [] trie)))

  ;; : @ @ <- (Fun Bool <- Key Value) @
  .partition:
  (lambda (p t)
    (let prec ((k 0) (t t))
      (match (.unwrap t)
        ((Empty) (values .empty .empty))
        ((Leaf value) (if (p k value) (values t .empty) (values .empty t)))
        ((Branch height left right)
         (let*-values (((ly ln) (prec k left))
                       ((ry rn) (prec (.right-key height k) right)))
           (values (.make-branch height ly ry) (.make-branch height ln rn))))
        ((Skip height bits-height bits child)
         (let-values (((y n) (prec (.skip-key height bits-height bits k) child)))
           (def (f child) (.make-skip height bits-height bits child))
           (values (f y) (f n)))))))

  ;; The (.split k t) operation returns a triple (l data r), where
  ;; l is the map with all the bindings of t whose key is strictly less than k;
  ;; r is the map with all the bindings of t whose key is strictly greater than k;
  ;; data is #f if t contains no binding for k,
  ;; or (some v) if t binds v to k
  ;; : @ (Option Value) @ <- Key @
  .split:
  (lambda (k t)
    (let/cc return
      (when (< k 0) (return (values .empty #f t)))
      (def h (.trie-height t))
      (unless (and h (>= h (integer-length k))) (return (values t #f .empty)))
      (let srec ((k 0) (t t))
        (match (.unwrap t)
          ((Empty) (values .empty #f .empty))
          ((Leaf v) (values .empty (some v) .empty))
          ((Branch height left right)
           (if (bit-set? (1- height) k)
             (let-values (((l x r) (srec (.right-key height k) right)))
               (values (.make-branch height left l)
                       x
                       (.make-skip height 0 1 r)))
             (let-values (((_ x r) (srec k left)))
               (values (.make-skip height 0 0 left)
                       x
                       (.make-branch height r right)))))
          ((Skip height bits-height bits child)
           (let-values (((l x r) (srec (.skip-key height bits-height bits k) child)))
             (def (f child) (.make-skip height bits-height bits child))
             (values (f l) x (f r))))))))

  .Zipper: {(:: @Z [])
    A: @
    Z: (Pair A {(:: @P Path) A})
    .element?: (.@ Z .element?)
    .validate: (.@ Z .validate)
    .sexp<-: (.@ Z .sexp<-)
    .bytes<-: (.@ Z .bytes<-)
    .<-bytes: (.@ Z .<-bytes)
    .json<-: (.@ Z .json<-)
    .<-json: (.@ Z .<-json)
    .marshal: (.@ Z .marshal)
    .unmarshal: (.@ Z .unmarshal)
    .map: (lambda (f z) (cons (f (car z)) (.call Path .map f (cdr z))))
  }
  Zipper: (lambda (A) (.@ {(:: @Z [.Zipper]) A} Z))

  ;; : (Zipper @) <- @
  .zipper<-: (lambda (t) (cons t ($Path ($Costep (.trie-height t) 0) [])))

  ;; : @ <- (Pair @ (Path @))
  .<-zipper: (match <> ([t . path] (.make-head (car (.call Path .up t path)))))

  ;; Given a focus on a subtrie, return focuses on the next level of subtries.
  ;; If we were focusing on a node with N children, the list will be of length N.
  ;; In particular, the list will be empty if we were already focusing on a leaf,
  ;; and will be of length 2 if we were focusing on a regular branch of a binary tree.
  ;; TODO: also return a (t list -> zipper) to reconstruct the zipper from the next submaps?
  ;; : (List (Zipper @)) <- (Zipper @)
  .next:
  (match <>
    ([trie . path]
     (match (.unwrap trie)
       ((Empty) [])
       ((Leaf _) [])
       ((Branch _ left right)
        (let-match (($Path ($Costep h k) steps) path)
          (def h1 (1- h))
          (def k1 (arithmetic-shift k 1))
          [(cons left ($Path ($Costep h1 k1) (.make-branch-step right steps)))
           (cons right ($Path ($Costep h1 (1+ k1)) (.make-branch-step left steps)))]))
       ((Skip _ bits-height bits child)
        (let-match (($Path ($Costep h k) steps) path)
          (def length (1+ bits-height))
          [(cons child ($Path ($Costep (- h length) (+ (arithmetic-shift k length) bits))
                              [(SkipStep bits-height) . steps]))])))))

  .make-branch-step:
  (lambda (branch steps)
    (if (.empty? branch) (.make-skip-step 0 steps) [(BranchStep branch) . steps]))

  .make-skip-step:
  (lambda (bits-height steps)
    (if (> 0 bits-height) steps
        (match steps
          ([(SkipStep bh) . more] [(SkipStep (+ 1 bh bits-height)) . more])
          (_ [(SkipStep bits-height) . steps]))))

  ;; Refocus a Zipper until we reach a new Costep.
  ;; Algorithm:
  ;; 1. Ascend as much as needed and possible so h >= h2 and k is a prefix of k2.
  ;; 2. If fail to satisfy those conditions, create a new branch for the new key.
  ;; 3. Now h >= h2 and k is a prefix. Descend as much as possible along k2 while staying above h2.
  ;; TODO: accept any Unstep that is more general than trie, with a trie <- trunk function.
  ;; : (Zipper @) <- Costep (Zipper @)
  .refocus:
  (nest
   (let (up (.@ @ Step .up)))
   (lambda (costep zipper))
   (match costep) (($Costep h2 k2))
   (match zipper) ((cons trie ($Path ($Costep h k) steps)))
   (let (hcommon ;; height up to which to ascend: no less than the desired height
                 ;; but also no less than necessary for there being a branch to our desired key
                 ;; and no less than necessary for there being a branch to the current key
                 ;; and yet no more than necessary.
         (max h2
              (1- (integer-length
                  ;; Note that for very long keys, this bitwise-xor is already a log N operation,
                  ;; in which case maintaining an O(1) amortized cost would require us to
                  ;; take as parameter an incremental change on a zipper for the Costep,
                  ;; and return an accordingly modified zipper for the Trie.
                  ;; In practice we use 256-bit keys for Ethereum, which is borderline.
                  (bitwise-xor (arithmetic-shift k (1+ (or h -1)))
                               (arithmetic-shift k2 (1+ h2)))))))
     ;; here, h >= common >= h2, so we must descend toward the sought focus
     (nest
      (def (descend t h s))
      (if (.empty? t) (cons .empty ($Path costep (.make-skip-step (- h h2 1) s)))) ;; easiest case: done
      (let d ((t t) (h h) (s s)))
      (if (= h h2) (cons t ($Path costep s))) ;; reached the goal with a non-empty sub-trie!
      (match (.unwrap t) ;; cannot be Leaf, or else we would have had (= h h2), nor Empty, handled above
        ((Branch _ left right) ;; easy case: descending a Branch
         (let-values (((trunk branch)
                       (if (bit-set? (- h h2 1) k2) (values right left) (values left right))))
           (d trunk (1- h) (.make-branch-step branch s)))))
      ((Skip _ bits-height bits child)) ;; hard case: descending common then uncommon parts of a Skip
      (let* ((length (1+ bits-height))
             (child-height (- h length))
             (floor-height (max h2 child-height))
             (comparable-length (- h floor-height))
             (key-bits (extract-bit-field comparable-length (- floor-height h2) k2))
             (node-bits (extract-bit-field comparable-length (- floor-height child-height) bits))
             (diff-length (integer-length (bitwise-xor key-bits node-bits)))))
      (if (zero? diff-length) ;; Not so hard: if it was the same key all the way that matters.
        (d (.make-skip h (- bits-height comparable-length)
                       (extract-bit-field (- length comparable-length) 0 bits) child)
           floor-height
           (.make-skip-step (1- comparable-length) s)))
      (let* ((same-length (- comparable-length diff-length))
             (branch-node-height (- h same-length)) ;; height of the branch node if different
             (branch-height (1- branch-node-height)) ;; height of the two new branches
             (old-branch-length (- branch-height child-height))
             (old-branch (if (plus? old-branch-length)
                           (.skip branch-height (1- old-branch-length)
                                  (extract-bit-field old-branch-length 0 bits) child)
                           child))
             (steps (.make-skip-step (- branch-height h2 1)
                                     (.make-branch-step old-branch
                                                        (.make-skip-step (1- same-length) s))))))
      (cons .empty ($Path costep steps))))
   (let ascend ((t trie) (h (or h -1)) (k k) (s steps)))
   (if (>= h hcommon) (descend t h (.make-skip-step (1- (integer-length k)) s)))
   (match s
     ([step . steps]
      (match (up step (cons t ($Costep h k)))
        ((cons upt ($Costep upheight upkey)) (ascend upt upheight upkey steps)))))
   ([]) ;; At this point we're still below the desired level,
   ;; but there are no more trie steps to zip up, so k is 0 and hcommon is h2+ilength(k2),
   ;; which means we have to create additional trie nodes up to accommodate space
   ;; for the new key k2 and/or the desired h2.
   (if (zero? k2)
     (descend (.make-skip hcommon (- hcommon h 1) 0 t) hcommon []))
   (let* ((kh1 (- hcommon h2 2))
          (left (.make-skip (1- hcommon) (- hcommon h 2) (extract-bit-field (- hcommon h 1) 0 k) t))))
   (descend .empty h2 (.make-skip-step kh1 (.make-branch-step left []))))

  .leaf<-opt: (match <> ((some v) (.leaf v)) (_ .empty))
  .opt<-leaf: (lambda (t) (match (.unwrap t) ((Leaf v) (some v)) (_ #f)))

  ;; : @ <- Key (Fun (Option Value) <- (Option Value)) @
  .update/opt:
  (lambda (key f trie)
    (match (.refocus ($Costep -1 key) (.zipper<- trie))
      ((cons t path) (.<-zipper (cons (.leaf<-opt (f (.opt<-leaf t))) path)))))

  ;; Given (the data of) a skip node, and a height at which to cut it (at least 0,
  ;; and no more than the skip node's bits-height), return the two notionally
  ;; equivalent branches (one of them empty) of the lower part of the cut.
  ;; : @ <- Key (Fun (Option Value) <- (Option Value)) @
  .skip-choice:
  (lambda (height bits-height bits child cut-height)
    (let* ((h (- height (- bits-height cut-height) 1))
           (t (.make-skip h (1- cut-height) bits child)))
      (if (bit-set? cut-height bits) (values .empty t) (values t .empty))))

  ;; Describes a recursive computation over a pair of tries in great generality.
  ;; : (Fun o <-
  ;;      Key ;; Common prefix key of the both left and right trie nodes.
  ;;      @[a/Value] @[b/Value] ;; The two input tries, a and b
  ;;      recurse: o <- Key @[a/Value] @[b/Value] ;; Result from recursing into given nodes of the tries
  ;;      empty: o <- ;; Result if both tries are empty.
  ;;      leaf: o <- Key a b ;; Result from recursing over two leaf nodes at given key with given contents
  ;;      branch: o <- Key Height o o ;; recursing down two branches from given node at specified height,
  ;;                                  ;; given results for left and right tries (NOT a and b).
  ;;      skip: o <- Key o Height Height Key ;; result from simultaneously skipping some key fragment
  ;;                                         ;; on both tries
  ;;      onlya: o <- Key @[a/Value] ;; Result given that a node is only explicitly present in the a trie.
  ;;      onlyb: o <- Key @[b/Value]) ;; Result given that a node is only explicitly present in the b trie.
  .recurse/trie-pair:
  (lambda (k a b recurse: recurse empty: empty leaf: leaf branch: branch skip: skip onlya: onlya onlyb: onlyb)
    (defvalues (aa bb) (.ensure-same-height a b))
    (let r ((k k) (a aa) (b bb))
      (match (cons (.unwrap a) (.unwrap b))
        ((cons (Empty) (Empty)) (empty))
        ((cons (Empty) _) (onlyb k b))
        ((cons _ (Empty)) (onlya k a))
        ((cons (Leaf va) (Leaf vb)) (leaf k va vb))
        ((cons (Branch h la ra) (Branch _ lb rb))
         (left-to-right branch k h (recurse r k la lb) (recurse r (.right-key h k) ra rb)))
        ;; TODO: write some meta-level constraint solver that deduces the 40 lines below from the 10 above.
        ((cons (Branch h la ra) (Skip _ bits-height bits cb))
         (let* ((bh1 (1- bits-height))
                (rk (.right-key h k))
                (cb1 (.make-skip (1- h) bh1 bits cb)))
           (if (bit-set? bits-height bits)
             (left-to-right branch k h (onlya k la) (recurse r rk ra cb1))
             (left-to-right branch k h (recurse r k la cb1) (onlya rk ra)))))
        ((cons (Skip h bits-height bits ca) (Branch _ lb rb))
         (let* ((bh1 (1- bits-height))
                (rk (.right-key h k))
                (ca1 (.make-skip (1- h) bh1 bits ca)))
           (if (bit-set? bits-height bits)
             (left-to-right branch k h (onlyb k lb) (recurse r rk ca1 rb))
             (left-to-right branch k h (recurse r k lb ca1) (onlyb rk rb)))))
        ((cons (Skip h abits-height abits achild) (Skip _ bbits-height bbits bchild))
         (let* ((bits-height (min abits-height bbits-height))
                (length (1+ bits-height))
                (ahighbits (extract-bit-field length (- abits-height bits-height) abits))
                (bhighbits (extract-bit-field length (- bbits-height bits-height) bbits))
                (difflength (integer-length (bitwise-xor ahighbits bhighbits)))
                (samelength (- length difflength))
                (sameheight (- h samelength))
                (samebits (extract-bit-field samelength (- length samelength) ahighbits))
                (ksame (bitwise-ior k (arithmetic-shift samebits (1+ sameheight))))
                (adiffheight (- abits-height samelength))
                (bdiffheight (- bbits-height samelength))
                (same-result
                 (if (zero? difflength)
                   (recurse r ksame ;; at least one of the two below is actually not a skip:
                            (.make-skip sameheight adiffheight abits achild)
                            (.make-skip sameheight bdiffheight bbits bchild))
                   (let-values (((aleft aright) (.skip-choice h abits-height abits achild adiffheight))
                                ((bleft bright) (.skip-choice h bbits-height bbits bchild bdiffheight)))
                     (left-to-right
                      branch ksame sameheight
                      (recurse r k aleft bleft)
                      (recurse r (.right-key sameheight ksame) aright bright))))))
           (if (zero? samelength)
             same-result
             (skip k h (1- samelength) samebits same-result))))
        (_ (invalid '.recurse/trie-pair: k a b recurse: recurse empty: empty leaf: leaf branch: branch skip: skip onlya: onlya onlyb: onlyb)))))


  ;; : @ <- (Fun (Option Value) <- Key (Option Value) (Option Value)) @ @ ?Key
  .merge:
  (lambda (f a b (k 0))
    (.recurse/trie-pair
     k a b
     recurse: (cut <> <> <> <>)
     empty: (lambda () .empty)
     leaf: (lambda (k va vb)
             (match (f k (some va) (some vb))
               (#f .empty)
               ((some v) (.leaf v))))
     branch: (lambda (_ h l r) (.make-branch h l r))
     skip: (lambda (_ h l b c) (.make-skip h l b c))
     onlya: (lambda (k a) (.map/key/opt (lambda (k v) (f k (some v) #f)) a k))
     onlyb: (lambda (k b) (.map/key/opt (lambda (k v) (f k #f (some v))) b k))))

  ;; Compare two trees lexicographically. TODO: should we always be using the Value's .comparer function?
  ;; : Integer <- (Integer <- a b) @[a/Value] @[b/Value]
  .compare:
  (lambda (cmp a b)
    (let/cc return
      (.recurse/trie-pair
       0 a b
       recurse: (cut <> <> <> <>)
       empty: void
       leaf: (lambda (_ va vb) (def r (cmp va vb)) (or (zero? r) (return r)))
       branch: void
       skip: void
       onlya: (lambda (_ _) (return 1))
       onlyb: (lambda (_ _) (return -1)))
      0))

  ;; Are two trees equal?
  ;; : Bool <- @ @
  .=?:
  (lambda (a b)
    (let/cc return
      (when (eq? a b) (return #t))
      (.recurse/trie-pair
       0 a b
       recurse: (lambda (r k a b) (or (eq? a b) (r k a b)))
       empty: true
       leaf: (lambda (_ va vb) (or (.call Value .=? va vb) (return #f)))
       branch: true
       skip: true
       onlya: (lambda (_ _) (return #f))
       onlyb: (lambda (_ _) (return #f)))))

  ;; Split a tree in two strictly smaller trees, if possible, in a somewhat balanced way, if possible.
  ;; NB: We assume that #f, if a valid wrapped trie, is the empty trie.
  ;; : (OrFalse @) (OrFalse @) <- @
  .divide:
  (lambda (t)
    (match (.unwrap t)
      ((Empty) (values #f #f))
      ((Leaf _) (values t #f))
      ((Branch h l r) (values (.make-head l) (.make-skip h 0 1 r)))
      ((Skip h bh b c)
       (match (.unwrap c)
         ((Branch hh l r)
          (let (f (cut .make-skip h bh b <>))
            (values (f (.make-skip hh 0 0 l)) (f (.make-skip hh 0 1 r)))))
         ((Leaf _) (values t #f))
         (_ (invalid '.divide t))))))

  ;; : (Iterator (Pair Key Value)) <- @ ?Key
  .iter<-:
  (lambda (t (from 0))
    (def (next it)
      (defvalues (r ne)
        (let loop ((e (iterator-e it)))
          (match e
            ([] (values iter-end []))
            ([[k . t] . kts]
             (match (.unwrap t)
               ((Empty) (loop kts))
               ((Leaf v) (values [k . v] kts))
               ((Branch h l r) (loop [[k . l] [(.right-key h k) . r] . kts]))
               ((Skip h l b c) (loop [[(.skip-key h l b k) . c] . kts])))))))
      (set! (iterator-e it) ne)
      r)
    (when (plus? from) (let-values (((_ _ r) (.split (1- from) t))) (set! t r)))
    (make-iterator [[0 . t]] next)))

(.def (TrieSet. @ Set<-Table.)
  Table: {(:: @ Trie.) Value: Unit}
  .subset?: ;; is a a subset of b
  (lambda (a b)
    (let/cc return
      (when (eq? a b) (return #t))
      (.call Table .recurse/trie-pair
             0 a b
             recurse: (lambda (r k a b) (or (eq? a b) (r k a b)))
             empty: true
             leaf: true
             branch: true
             skip: true
             onlya: (lambda (_ _) (return #f))
             onlyb: true))))

(def (SimpleTrie Key Value)
  {(:: @ [Trie. IdWrap]) (Key) (Value)
   sexp: `(SimpleTrie ,(.@ Key sexp) ,(.@ Value sexp))})

(def (SimpleTrieSet Elt) {(:: @ [TrieSet.]) (Elt) Value: Unit})

(def NatTrieSet (SimpleTrieSet Nat))
