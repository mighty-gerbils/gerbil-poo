;;; Pure Big Endian Patricia Trees (Tries), mapping natural integers to values.
;;; See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
;;; http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps

(export #t)

(import :clan/debug)
(import
  :gerbil/gambit/bits
  :std/error :std/iter
  :clan/base :clan/option :clan/number
  :clan/poo/poo :clan/poo/mop :clan/poo/brace :clan/poo/number :clan/poo/type
  :clan/poo/fun :clan/poo/io :clan/poo/table)

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
;; The key in a Costep is the bits *above* the height, to be multiplied by 2**(height+1)
;; to get the actual key of the start of the current trie.
(defstruct $Costep (height key) transparent: #t) ; height: Integer key: Key
(defstruct $Path (costep steps) transparent: #t) ; costep: Costep steps: (List (Step t))
(defstruct $Unstep (left right skip) transparent: #t) ;; left: (Fun trunk <- Key Height trunk branch) right: (Fun trunk <- Key Height branch trunk) skip: (Fun trunk <- Key Height Height Key trunk)

;; This is basically a fold of the open recursion functor of which the (unwrapped) Trie is the fixed-point.
(.def (TrieSynth. @ [] ;; mixin for Type.
   ;; Key ;; : Type -- implicit
   Value ;; : Type
   .empty ;; : @
   .leaf ;; : @ <- Value
   .branch ;; : @ <- Height @ @ ;; height, left, right
   .skip)) ;; : @ <- Height Height Key @ ;; height, length, key, child

(.def (TrieSynthUnit @ []) ;; mixin for Unit TrieSynth.
  .empty: (void)
  .leaf: void
  .branch: void
  .skip: void)

(.def (TrieSynthCardinal @ []) ;; mixin for Nat TrieSynth.
  .empty: 0
  .leaf: (lambda _ 1)
  .branch: (lambda (_ x y) (+ x y))
  .skip: (lambda (_ _ _ child) child))

;; Method to compute the .skip method of a TrieSynth. from the .leaf, .branch and .empty methods,
;; as if the Trie didn't sport the skip node optimization.
(.def (TrieSynthComputeSkip @ [] .branch .empty) ;; mixin for TrieSynth.
  .skip: (lambda (height bits-height bits synth)
           (let loop ((bh 0) (s synth))
             (if (> bh bits-height) synth ;; the (1+ bits-height) is the number of bits
                 (loop (1+ bh)
                       (if (bit-set? bh bits)
                         (.branch height .empty synth)
                         (.branch height synth .empty)))))))

(.def (Trie. @ [Wrap. CommonTableMethod.] ;; @ <: (Wrap T)
       .wrap ;; : (Wrap t) <- t
       .unwrap ;; : t <- (Wrap t)
       .bind/wrap ;; : u <- (Wrap t) (u <- t) = (lambda (x f) (f (.unwrap x)))
       .map/wrap) ;; : (Wrap u) <- (u <- t) (Wrap t) = (lambda (f x) (.wrap (f (.unwrap x))))

  sexp: 'Trie.

  ;; The type of values stored in a Trie.
  ;; ... except that some binary methods deal with two different value types;
  ;; we then write @[a/Value] to denote a Trie where Value has been overriden with a (type variable)
  Value: Type. ;; : Type

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
  T: {(:: @ Type.) sexp: '$Trie .element?: $Trie?}
      #;(Sum ;; the wrapped type
      (Empty)
      (Leaf value: Value)
      (Branch height: Height left: T right: T)
      (Skip height: Height bits-height: Height bits: Key child: T))

;;  Step: (lambda (t)
;;          #;(Sum (BranchStep branch: t) (SkipStep bits-height: Height))
;;          {(:: @ Type.) sexp: '$Step .element?: $Step?})
;;  Costep: (Struct height: Height key: Key)
;;  Path: (lambda (t) (Struct costep: Costep steps: (List (Step t))))
;;  Unstep: (lambda (trunk branch) ;; : Type <- Type Type
;;            (Struct left: (Fun trunk <- Key Height trunk branch)
;;                    right: (Fun trunk <- Key Height branch trunk)
;;                    skip: (Fun trunk <- Key Height Height Key trunk)))

  ;; (Unstep a a) <- (Fun a <- Key Height a a) (Fun a <- Key Height Height Key a)
  .symmetric-unstep:
  (lambda (branch: branch ;; : a <- Key Height a a
      skip: skip) ;; : a <- Key Height Height Key a
    ($Unstep branch branch skip))

  ;; (Pair trunk Costep) <- (Unstep trunk branch) (Pair trunk Costep) (Step branch)
  .apply/step:
  (lambda (unstep step acc)
    (let-match ((cons trie ($Costep height key)) acc)
      (unless height
        (error "step-apply: height must be non-empty for a non-empty path"))
      (match step
        ((BranchStep branch)
         (let* ((h (1+ height)) ;; new height after un-branching
                (upkey (arithmetic-shift key -1))
                (fullkey (arithmetic-shift key h)))
           (cons
            (if (bit-set? 0 key)
              (($Unstep-right unstep) fullkey h branch trie)
              (($Unstep-left unstep) fullkey h trie branch))
            ($Costep h upkey))))
        ((SkipStep bits-height)
         (let* ((length (1+ bits-height))
                (h (+ height length)) ;; new height after un-skipping
                (bits (extract-bit-field length 0 key))
                (upkey (arithmetic-shift key (- length)))
                (fullkey (arithmetic-shift key (1+ height))))
           (cons (($Unstep-skip unstep) fullkey h bits-height bits trie) ($Costep h upkey)))))))

  ;; TODO: make this the .op method for a monad operating on a category?
  ;; : (Pair trunk Costep) <- (Unstep trunk branch) trunk (Path branch)
  .apply/path:
  (lambda (unstep t path)
    (let-match (($Path costep steps) path)
      (let-match (($Costep height _) costep)
        (def h (.trie-height t))
        (when (and h height (> h height)) (invalid '.apply/path unstep t path))
        (foldl (cut .apply/step unstep <> <>) (cons (.ensure-height height t) costep) steps))))

  ;; TODO: make these the .map method of a Path functor.
  ;; (Path a) <- (a <- b) (Path b)
  .map/path:
  (lambda (f path) ($Costep ($Path-costep path) (map (cut .map/step f <>) ($Path-steps path))))

  ;; : Height <- (Step a)
  .step-length:
  (match <>
    ((BranchStep _) 1)
    ((SkipStep bits-height) (1+ bits-height)))

  ;; TODO: make it the .element? or (with better error messages) a .validate method of the Path functor.
  ;; : Bool <- (Path a)
  .element?/path:
  (lambda (path)
    (let-match (($Path ($Costep height key) steps) path)
      (and (or (member height '(-1 #f)) (element? Height height))
           (element? Key key)
           (let c ((height height) (steps steps))
             (match steps
               ([] #t)
               ([step . steps]
                (match step
                  ;; TODO: validate the branch parameter to be of a proper parameter type.
                  ((BranchStep _) (c (1+ height) steps))
                  ((SkipStep bits-height)
                   (and (element? Height bits-height)
                        (let (new-height (+ height bits-height 1))
                          (and (element? Height new-height)
                               (c new-height steps))))))))))))

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
  .element?:
  (lambda (trie)
    (match (.unwrap trie)
      ((Empty) #t) ;; NB: In a normalized trie, Empty only happens at the toplevel:
      ;; Otherwise, a Branch with an Empty child is normalized to a Skip
      ((Leaf value) #t)
      ((Branch height left right)
       (and (element? Height height)
            (.element? left)
            (= (1- height) (.trie-height left))
            (.element? right)
            (= (1- height) (.trie-height right))))
      ((Skip height bits-height bits child)
       (and (element? Height height)
            (element? Height bits-height)
            (<= bits-height height)
            (<= 0 bits)
            (<= (integer-length bits) (1+ bits-height))
            (.element? child)
            (= (- height bits-height 1) (.trie-height child))))))

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
    ;;(unless (and (element? Height height) (equal? (1- height) (.trie-height left)) (equal? (1- height) (.trie-height right))) (error "Internal error" '.branch height left right))
    (.wrap (Branch height left right)))

  ;; : (Fun @ <- Key Height Height @)
  .skip:
  (lambda (height bits-height bits child)
    ;;(unless (and (element? Height height) (element? Height bits-height) (element? Key bits) (equal? (- height bits-height 1) (.trie-height child))) (error "Internal error" '.skip height bits-height bits child))
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
                    (bits-height2 (+ length bits-height1))
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

  ;; : @ <- Key Value @
  .acons:
  (lambda (key value trie)
    (let/cc return
      (def h (.trie-height trie))
      (unless h (return (.singleton key value))) ;; trie was empty
      (def key-height (1- (integer-length key)))
      (if (> key-height h)
        (.branch
         key-height
         (.make-skip (1- key-height) (- key-height h 2) 0 trie)
         (.make-leaf (1- key-height) key value))
        (let insert ((h h) (t trie))
          (match (.unwrap t)
            ((Leaf old) (if (equal? value old) (return trie) (.leaf value)))
            ((Branch _ left right)
             (let (h1 (1- h))
               (if (bit-set? h key)
                 (.branch h left (insert h1 right))
                 (.branch h (insert h1 left) right))))
            ((Skip _ bits-height bits child)
             (let* ((length (1+ bits-height))
                    (child-height (- h length))
                    (key-bits (extract-bit-field length (1+ child-height) key)))
               (if (= bits key-bits)
                 (.skip h bits-height bits (insert child-height child))
                 ;; new structure:
                 ;; skip for the length that is the same (if not 0),
                 ;; then a branch-node with on one side a new-branch with a new leaf,
                 ;; and on the other, the old-branch with the child of the original skip
                 (let* ((diff-length (integer-length (bitwise-xor bits key-bits)))
                        (same-length (- length diff-length))
                        (branch-node-height (- h same-length)) ;; height of the branch node
                        (branch-height (1- branch-node-height)) ;; height of the two new branches
                        (old-branch-length (- branch-height child-height))
                        (old-branch (if (zero? old-branch-length) child
                                        (.skip branch-height
                                               (1- old-branch-length)
                                               (extract-bit-field old-branch-length 0 bits)
                                               child)))
                        (new-branch (.make-leaf branch-height key value))
                        ;; Let's look whether the old branch goes right or left
                        (branch-node (if (bit-set? old-branch-length key-bits)
                                       (.make-branch branch-node-height old-branch new-branch)
                                       (.make-branch branch-node-height new-branch old-branch))))
                   (.make-skip h (1- same-length)
                               (arithmetic-shift bits (- diff-length))
                               branch-node))))))))))

  ;; : @ <- @ Key
  .remove:
  (lambda (trie key)
    (let/cc return
      (def h (.trie-height trie))
      (unless h (return trie)) ;; key is absent from empty trie; return unchanged empty trie
      (.make-head
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
              (return trie)))))))) ;; key is in skipped branch, return unchanged trie

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
        ((Leaf v) (match (f k v) (#f .empty) ((some val) (.leaf val))))
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
                       (.make-skip height 1 1 r)))
             (let-values (((_ x r) (srec k left)))
               (values (.make-skip height 1 0 left)
                       x
                       (.make-branch height r right)))))
          ((Skip height bits-height bits child)
           (let-values (((l x r) (srec (.skip-key height bits-height bits k) child)))
             (def (f child) (.make-skip height bits-height bits child))
             (values (f l) x (f r))))))))

  ;; : (Step a) <- (Fun a <- b) (Step b)
  .map/step:
  (lambda (f step)
    (match step
      ((BranchStep branch) (BranchStep (f branch)))
      ((SkipStep bits-height) step)))

  ;; Zipper: (Pair @ (Path @))

  ;; : Zipper <- @
  .zip: (lambda (t) (cons t ($Path ($Costep (.trie-height t) 0) [])))

  ;; : @ <- (Pair @ (Path @))
  .unzip:
  (let (unstep (.symmetric-unstep branch: (lambda (_ h l r) (.make-branch h l r))
                                  skip: (lambda (_ h l b c) (.make-skip h l b c))))
    (match <> ([t . path] (.make-head (car (.apply/path unstep t path))))))

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
          (let* ((h1 (1- h))
                 (k2 (arithmetic-shift k 1)))
            [(cons left ($Path ($Costep h1 k2) [(BranchStep right) . steps]))
             (cons right ($Path ($Costep h1 (1+ k2)) [(BranchStep left) . steps]))])))
       ((Skip _ bits-height bits child)
        (let-match (($Path ($Costep h k) steps) path)
          (let (length (1+ bits-height))
            [(cons child ($Path ($Costep (- h length) (+ (arithmetic-shift k length) bits))
                                [(SkipStep bits-height) . steps]))]))))))

  ;; Find the narrowest path to the key in the trie without splitting a Skip node.
  ;; : (Zipper @) <- Key @
  .find-path:
  (lambda (key trie)
    (unless (element? Key key) (error "find-path bad key" key trie))
    (def h (.trie-height trie))
    (def (p t height steps)
      (cons t ($Path ($Costep height (arithmetic-shift key (- -1 height))) steps)))
    (if (not h) (.zip trie)
        (let (kh (1- (integer-length key)))
          (if (> kh h)
            (let* ((kh1 (1- kh))
                   (left (.make-skip kh1 (- kh1 h 1) 0 trie)))
              (cons .empty
                    ($Path ($Costep kh1 1) [(BranchStep left)])))
            (let f ((h h) (t trie) (steps []))
              (match (.unwrap t)
                ((Empty) (p t h steps))
                ((Leaf _) (p t h steps))
                ((Branch h left right)
                 (let (h1 (1- h))
                   (if (bit-set? h key)
                     (f h1 right (cons (BranchStep left) steps))
                     (f h1 left (cons (BranchStep right) steps)))))
                ((Skip h bits-height bits child)
                 (let* ((length (1+ bits-height))
                        (hc (- h length)))
                   (if (= bits (extract-bit-field length (1+ hc) key))
                       (f hc child (cons (SkipStep bits-height) steps))
                       (p t h steps))))))))))

  ;; : @ <- Key (Fun (Option Value) <- (Option Value)) @
  .update/opt:
  (lambda (k f t)
    (let-match ([sub . up] (.find-path k t))
      (def o (match (.unwrap sub) ((Leaf value) (some value)) (_ #f)))
      (def u (f o))
      (if (equal? o u) t
          ;; kk is the part of the key k that's valid for the submap sub
          (let* ((kk (match ($Costep-height ($Path-costep up))
                       (#f k)
                       (-1 0)
                       (h (extract-bit-field (1+ h) 0 k))))
                 (newsub (match u
                           (#f (.remove sub kk))
                           ((some v) (.acons kk v sub)))))
          (.unzip (cons newsub up))))))

  ;; Given (the data of) a skip node, and a height at which to cut it (at least 0,
  ;; and no more than the skip node's bits-height), return the two notionally
  ;; equivalent branches (one of them empty) of the lower part of the cut.
  ;; : @ <- Key (Fun (Option Value) <- (Option Value)) @
  .skip-choice:
  (lambda (height bits-height bits child cut-height)
    (let* ((h (- height (- bits-height cut-height) 1))
           (t (.make-skip h (1- cut-height) bits child)))
      (if (bit-set? cut-height bits)
        (values .empty t)
        (values t .empty))))

  ;; Describes a recursive computation over a pair of tries in great generality.
  ;; Note that this assumes the two tries have the same height. Use ensure-same-height if needed.
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
    (match (cons (.unwrap a) (.unwrap b))
      ((cons (Empty) (Empty)) (empty))
      ((cons (Empty) _) (onlyb k b))
      ((cons _ (Empty)) (onlya k a))
      ((cons (Leaf va) (Leaf vb)) (leaf k va vb))
      ((cons (Branch h la ra) (Branch _ lb rb))
       (left-to-right branch k h (recurse k la lb) (recurse k ra rb)))
      ((cons (Branch h la ra) (Skip _ bits-height bits cb))
       (let* ((bh1 (1- bits-height))
              (rk (.right-key h k))
              (cb1 (.make-skip (1- h) bh1 bits cb)))
         (if (bit-set? bits-height bits)
           (left-to-right branch k h (onlya k la) (recurse rk ra cb1))
           (left-to-right branch k h (recurse k la cb1) (onlya rk ra)))))
      ((cons (Skip h bits-height bits ca) (Branch _ lb rb))
       (let* ((bh1 (1- bits-height))
              (rk (.right-key h k))
              (ca1 (.make-skip (1- h) bh1 bits ca)))
         (if (bit-set? bits-height bits)
           (left-to-right branch k h (onlyb k lb) (recurse rk ca1 rb))
           (left-to-right branch k h (recurse k lb ca1) (onlyb rk rb)))))
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
                 (recurse ksame ;; at least one of the two below is actually not a skip:
                          (.make-skip sameheight adiffheight abits achild)
                          (.make-skip sameheight bdiffheight bbits bchild))
                 (let-values (((aleft aright) (.skip-choice h abits-height abits achild adiffheight))
                              ((bleft bright) (.skip-choice h bbits-height bbits bchild bdiffheight)))
                   (left-to-right
                    branch ksame sameheight
                    (recurse k aleft bleft)
                    (recurse (.right-key sameheight ksame) aright bright))))))
         (if (zero? samelength)
           same-result
           (skip k h (1- samelength) samebits same-result))))))

  ;; : @ <- (Fun (Option Value) <- Key (Option Value) (Option Value)) @ @ ?Key
  .merge:
  (lambda (f a b (k 0))
    (defvalues (aa bb) (.ensure-same-height a b))
    (let m ((k k) (a aa) (b bb))
      (.recurse/trie-pair
       k a b
       recurse: m
       empty: (lambda () .empty)
       leaf: (lambda (k va vb)
               (match (f k (some va) (some vb))
                 (#f .empty)
                 ((some v) (.leaf v))))
       branch: (lambda (_ h l r) (.make-branch h l r))
       skip: (lambda (_ h l b c) (.make-skip h l b c))
       onlya: (lambda (k a) (.map/key/opt (lambda (k v) (f k (some v) #f)) a k))
       onlyb: (lambda (k b) (.map/key/opt (lambda (k v) (f k #f (some v))) b k)))))

  ;; Compare two trees lexicographically. TODO: should we always be using the Value's .comparer function?
  ;; : Integer <- (Integer <- a b) @[a/Value] @[b/Value]
  .compare:
  (lambda (cmp a b)
    (defvalues (aa bb) (.ensure-same-height a b))
    (let/cc return
      (let m ((k 0) (a aa) (b bb))
        (.recurse/trie-pair
         k a b
         recurse: m
         empty: void
         leaf: (lambda (_ va vb) (def r (cmp va vb)) (or (zero? r) (return r)))
         branch: void
         skip: void
         onlya: (lambda (_ _) (return 1))
         onlyb: (lambda (_ _) (return -1)))
        0)))

  ;; Are two trees equal?
  ;; : Bool <- @ @
  .equal?:
  (lambda (a b)
    (let/cc return
      (let e? ((k 0) (a a) (b b))
        (or (eq? a b)
            (.recurse/trie-pair
             k a b
             recurse: e?
             empty: true
             leaf: (lambda (_ va vb) (or (.call Value .equal? va vb) (return #f)))
             branch: true
             skip: true
             onlya: (lambda (_ _) (return #f))
             onlyb: (lambda (_ _) (return #f)))))))

  ;; Split a tree in two strictly smaller trees, if possible, in a somewhat balanced way, if possible.
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

  ;; Split a trie in two or more strictly smaller trees, if possible, a somewhat balanced way, if possible.
  ;; Fallback to an empty list if the trie was empty, or a singleton list of a singleton trie if a singleton.
  ;; : (List @) <- @
  .divide/list:
  (lambda (t)
    (match (.unwrap t)
      ((Empty) [])
      ((Leaf _) [t])
      ((Branch h l r) [(.make-head l) (.make-skip h 0 1 r)])
      ((Skip h bh b c)
       (match (.unwrap c)
         ((Branch hh l r)
          (let (f (cut .make-skip h bh b <>))
            [(f (.make-skip hh 0 0 l)) (f (.make-skip hh 0 1 r))]))
         ((Leaf _) [t])
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

(.def (TrieSet. @ Type.)
  Table: {(:: @ Trie.) Value: Unit}
  .subset?: ;; is a a subset of b
  (lambda (a b)
    (defvalues (aa bb) (.call Table .ensure-same-height a b))
    (let/cc return
      (let m ((e 0) (a aa) (b bb))
        (or (eq? a b)
            (.call Table .recurse/trie-pair
                   e a b
                   recurse: m
                   empty: true
                   leaf: true
                   branch: true
                   skip: true
                   onlya: (lambda (_ _) (return #f))
                   onlyb: true))))))

(def (SimpleTrie Key Value)
  {(:: @ [Trie. IdWrap]) (Key) (Value)
   sexp: `(SimpleTrie ,(.@ Key sexp) ,(.@ Value sexp))})

(def (SimpleTrieSet Elt) {(:: @ [TrieSet.]) (Elt) Value: Unit})

(def NatTrieSet (SimpleTrieSet Nat))