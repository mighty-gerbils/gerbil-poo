;;; Common declarations and methods for tables

(export #t)

(import
  :std/iter :std/misc/alist
  :clan/base :clan/list :clan/option
  ./brace ./io ./mop ./number ./poo ./type)

;; TODO: have APIs look more like LIL, less like OCaml?
;; Especially since we may (1) use similar metaprogramming for OO style (?), and
;; (2) distinguish between for-each / foldl / foldr (iterating over pairs)
;; vs for-each* / foldl* / afoldr (iterating with two separate arguments for k v)
;; (3) implement additional APIs like drop, decons, map/2, convert, etc.
;; We could also reorder the arguments between object vs typeclass styles,
;; so the object is moved first in OO style, but at whatever "usual" place in the other style.

;; NB: General methods for maps, that are not trie-specific
(.def (methods.table @ []
       Key ;; : Type
       Value ;; : Type
       .empty ;; : @
       .acons ;; : @ <- Key Value @
       .ref ;; : Value <- @ Key ?(Value <-)
       .remove ;; : @ <- @ Key
       .foldl ;; : (Fun o <- (Fun o <- Key Value o) o @)
       .foldr) ;; : (Fun o <- (Fun o <- Key Value o) o @)

  .validate:
  (if (eq? Value Any) (lambda (x (ctx '())) x)
      (lambda (x (ctx '()))
        (def c [[validate: x] . ctx])
        (.for-each (lambda (_k v) (validate Value v c)) x) ;; should we test keys, too?
        x))

  ;; : (Fun Bool <- @)
  .empty?: (lambda (t) (eqv? t .empty))

  ;; : (Fun Bool <- @ Key)
  .key?: (lambda (t key) (let/cc ret (.ref t key (lambda () (ret #f))) #t))

  ;; : (Fun (Option Value) <- @ Key)
  .ref/opt: (lambda (t key) (let/cc ret (some (.ref t key (lambda () (ret #f))))))

  ;; : Unit <- (Fun Unit <- Key Value) @
  .for-each: (lambda (f t) (.foldl (lambda (k v a) (void (f k v))) (void) t))

  ;; : Unit <- (Fun Unit <- Key Value) @
  .for-each/reverse: (lambda (f t) (.foldr (lambda (k v a) (void (f k v))) (void) t))

  ;; : @ <- Key (Option Value) @
  .acons/opt: (lambda (k ov t) (match ov (#f t) ((some v) (.acons k v t))))

  ;; : @ <- Key Value
  .singleton: (lambda (k v) (.acons k v .empty))

  ;; Merging the hard way, by iterating on one table to add its entries,
  ;; then on the other table to add its one-sided entries.
  ;; : @ <- (Fun (Option Value) <- Key (Option Value) (Option Value)) @ @
  .merge:
  (lambda (f ta tb)
    (!> (.foldl (lambda (k va m) (.acons/opt k (f k (some va) (.ref/opt tb k)) m)) .empty ta)
        (cut .foldl (lambda (k vb m) (if (.key? ta k) m (.acons/opt k (f k #f (some vb)) m))) <> tb)))

  ;; : Nat <- @
  .count: (lambda (t) (.foldl (lambda (_ _ a) (1+ a)) 0 t))

  ;; : Bool <- (Fun Bool <- Key Value) @
  .every: (lambda (pred t) (let/cc return (.for-each (lambda (k v) (unless (pred k v) (return #f))) t) #t))

  ;; : Bool <- (Fun Bool <- Key Value) @
  .any: (lambda (pred t) (let/cc return (.for-each (lambda (k v) (when (pred k v) (return #t))) t) #f))

  ;; : (List (Pair Key Value)) <- @
  .list<-: (lambda (t) (.foldr acons [] t))

  ;; : @ <- (List (Pair Key Value))
  .<-list: (lambda (l) (foldr (fun (aconskv kv t) (.acons (car kv) (cdr kv) t)) .empty l))

  ;; : (Option (Pair Key Value)) <- @
  .min-binding/opt: (lambda (t) (let/cc return (.for-each (lambda (k v) (return (some (cons k v)))) t) #f))

  ;; : (Option (Pair Key Value)) <- @
  .max-binding/opt: (lambda (t) (let/cc return (.for-each/reverse (lambda (k v) (return (some (cons k v)))) t) #f))

  ;; : (Pair Key Value) <- @
  .min-binding: (lambda (t) (option-ref (.min-binding/opt t)))

  ;; : (Pair Key Value) <- @
  .max-binding: (lambda (t) (option-ref (.max-binding/opt t)))

  ;; : (Option (Pair Key Value)) <- @
  .choose/opt: .min-binding/opt

  ;; : (Pair Key Value) <- @
  .choose: .min-binding

  ;; Linear scan for the first matching entry
  ;; BEWARE: this assumes for-each is in increasing key order.
  ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @
  .find-first/opt:
  (lambda (p t) (let/cc return
             (.for-each (lambda (k v) (when (p k v) (return (some (cons k v))))) t) #f))

  ;; Linear scan for the last matching entry
  ;; BEWARE: this assumes for-each/reverse is in decreasing key order.
  ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @
  .find-last/opt:
  (lambda (p t) (let/cc return
             (.for-each/reverse (lambda (k v) (when (p k v) (return (some (cons k v))))) t) #f))

  ;; : (Pair Key Value) <- (Fun Bool <- Key Value) @
  .find-first: (lambda (p t) (option-ref (.find-first/opt p t)))

  ;; : (Pair Key Value) <- (Fun Bool <- Key Value) @
  .find-last: (lambda (p t) (option-ref (.find-last/opt p t)))

  ;; : @ <- Key (Fun (Option Value) <- (Option Value)) @
  .update/opt:
  (lambda (k f t)
    (def o (.ref/opt t k))
    (def u (f o))
    (if (equal? o u) t
        (match u
          (#f (.remove t k))
          ((some v) (.acons k v t)))))

  ;; : @ <- Key (Fun Value <- Value) @ ?(Fun Value <-)
  .update: (lambda (k f t (default false))
             (.update/opt k (lambda (vo) (some (f (option-get/default vo default)))) t))
  ;; NB: For a handful of flat datastructures that don't have deep traversals, this could be better:
  ;;.update: (lambda (k f t (default false)) (.acons k (f (.ref t k default)) t))

  ;; : @ <- (Fun (Option Value) <- Key Value Value) @ @
  .union:
  (lambda (f a b)
    (def (g i a b)
      (if a (if b (f i (some-value a) (some-value b)) a) b))
    (.merge g a b))

  ;; : @ <- @ @
  .join: (lambda (a b) (.merge (lambda (_ a b) (or a b)) a b))

  ;; : @ <- (List @)
  .join/list: (lambda (l) (foldl .join .empty l))

  ;; : Bool <- @ @
  .=?: (lambda (a b) (let/cc return
                  (.merge (lambda (_ a b)
                            (unless (and a b (.call Value .=? (some-value a) (some-value b)))
                              (return #f)) #f)
                          a b) #t))

  ;; Split a table in two smaller trees, if possible, a somewhat balanced way, if possible.
  ;; Return two values, the first being true if the table was not empty, and the second being true
  ;; if the table had at least two elements.
  ;; ASSUMES that #f, if a valid table, is the empty table.
  ;; This default method does the stupid thing of taking the first element off.
  ;; : (OrFalse @) (OrFalse @) <- @
  .divide:
  (lambda (t)
    (match (.min-binding/opt t)
      ((some (cons k v)) (values (.singleton k v) (let (r (.remove t k)) (and (not (.empty? r)) r))))
      (#f (values #f #f))))

  ;; Split a table in two or more strictly smaller trees, if possible, a somewhat balanced way, if possible.
  ;; Fallback to an empty list if the table was empty, or a singleton list of itself if a singleton table.
  ;; This default method assumes that we have good way to divide table in two.
  ;; : (List @) <- @
  .divide/list:
  (lambda (t)
    (match (.divide t)
      ((values #f #f) [])
      ((values x #f) [x])
      ((values x y) [x y])))

  ;; : @ <- (Iterator (Pair Key Value)) ?@
  .<-iter: (lambda (s (t .empty)) (for/fold (t t) (kv s) (.acons (car kv) (cdr kv) t)))

  ;; : (Iterator (Pair Key Value)) <- @
  .iter<-: (lambda (x) (:iter (.list<- x)))

  ;; : (Lens Value <- @) <- Key
  .lens: (lambda (k) {get: (lambda (t) (.ref t k)) set: (lambda (t v) (.acons k v t))})

  .Binding: (Pair Key Value)
  .Bindings: (List .Binding)
  .json<-: (compose (.@ .Bindings .json<-) .list<-) ;; : Json <- @
  .<-json: (compose .<-list (.@ .Bindings .<-json)) ;; : @ <- Json
  .bytes<-: (compose (.@ .Bindings .bytes<-) .list<-) ;; : Bytes <- @
  .<-bytes: (compose .<-list (.@ .Bindings .<-bytes)) ;; : @ <- Bytes
  .marshal: (lambda (x port) (marshal .Bindings (.list<- x) port))
  .unmarshal: (compose .<-list (.@ .Bindings .unmarshal)))

(.def (Set<-Table. @ Type. Table sexp)
  ;; Table must be a table from Elt to Unit, i.e. (.@ Table Key) == Elt, (.@ Table Value) == Unit
  Elt: (.@ Table Key) ;; : Type
  .validate: (.@ Table .validate) ;; : @ <- Any
  .element?: (.@ Table .validate) ;; : Bool <- Any
  .empty: (.@ Table .empty)  ;; : @
  .empty?: (.@ Table .empty?) ;; : Bool <- @
  .elt?: (.@ Table .key?) ;; : Bool <- @ Elt
  .cons: (cut .call Table .acons <> (void) <>) ;; : @ <- Elt @
  .singleton: (lambda (elt) (.call Table .singleton elt (void))) ;; : @ <- Elt
  .remove: (.@ Table .remove) ;; : @ <- @ Elt
  .for-each: (lambda (f t) (.call Table .for-each (lambda (e _) (f e)) t)) ;; : Unit <- (Unit <- Elt) @
  .for-each/reverse: (lambda (f t) (.call Table .for-each/reverse (lambda (e _) (f e)) t)) ;; : Unit <- (Unit <- Elt) @
  .foldl: (lambda (f a t) (.call Table .foldl (lambda (e _ a) (f e a)) a t)) ;; : a <- (a <- Elt a)
  .foldr: (lambda (f a t) (.call Table .foldr (lambda (e _ a) (f e a)) a t)) ;; : a <- (a <- Elt a)
  .every: (lambda (f t) (.call Table .every (lambda (e _) (f e)) t)) ;; : Bool <- (Bool <- Elt) @
  .any: (lambda (f t) (.call Table .any (lambda (e _) (f e)) t)) ;; : Bool <- (Bool <- Elt) @
  .filter: (lambda (f t) (.call Table .filter (lambda (e _) (f e)) t)) ;; : @ <- (Bool <- Elt) @
  .partition: (lambda (f t) (.call Table .partition (lambda (e _) (f e)) t)) ;; : @ @ <- (Bool <- Elt) @
  .count: (.@ Table .count) ;; : Nat <- @
  .list<-: (lambda (t) (map car (.call Table .list<- t))) ;; : (List Elt) <- @
  .<-list: (lambda (l) (foldl .cons .empty l)) ;; : @ <- (List Elt)
  .sexp<-: (lambda (x) `(.call ,sexp .<-list (@list ,@(map (.@ Elt sexp<-) (.list<- x))))) ;; : Sexp <- @
  .min-elt: (lambda (t) (car (.call Table .min-binding t))) ;; : Elt <- @
  .min-elt/opt: (lambda (t) (map/option car (.call Table .min-binding/opt t))) ;; : (Option Elt) <- @
  .max-elt: (lambda (t) (car (.call Table .max-binding t))) ;; : Elt <- @
  .max-elt/opt: (lambda (t) (map/option car (.call Table .max-binding/opt t))) ;; : (Option Elt) <- @
  .choose: .min-elt ;; : Elt <- @
  .choose-opt: .min-elt/opt ;; : (Option Elt) <- @
  .split: (lambda (elt t) (defvalues (a v b) (.call Table .split elt t)) (values a (some? v) b)) ;; : @ Bool @ <- Elt @
  .find-first/opt: (lambda (f t) (map/option car (.call Table .find-first/opt (lambda (e _) (f e)) t))) ;; : (Option Elt) <- (Bool <- Elt) @
  .find-first: (lambda (f t) (option-ref (.find-first/opt f t))) ;; : Elt <- (Bool <- Elt) @
  .find-last/opt: (lambda (f t) (map/option car (.call Table .find-last/opt (lambda (e _) (f e)) t))) ;; : (Option Elt) <- (Bool <- Elt) @
  .find-last: (lambda (f t) (option-ref (.find-last/opt f t))) ;; : Elt <- (Bool <- Elt) @
  .iter<-: (lambda (t from: (from 0)) (def i (.call Table .iter<- t from: from)) ;; : (Iterator Elt) <- @ ?Elt
              (set! (iterator-next i) (compose car (iterator-next i))) i)
  .<-iter: (lambda (s (t .empty)) (for/fold (t t) (elt s) (.cons elt t))) ;; : @ <- (Iterator Elt) ?@
  .List: (List Elt)
  .json<-: (compose (.@ .List .json<-) .list<-) ;; : Json <- @
  .<-json: (compose .<-list (.@ .List .<-json)) ;; : @ <- Json
  .bytes<-: (compose (.@ .List .bytes<-) .list<-) ;; : Bytes <- @
  .<-bytes: (compose .<-list (.@ .List .<-bytes)) ;; : @ <- Bytes
  .marshal: (lambda (x port) (marshal .List (.list<- x) port))
  .unmarshal: (compose .<-list (.@ .List .unmarshal))
  ;; TODO: for union, inter, diff, compare, equal, subset,
  ;; optimize for full subtables, by caching count in wrapper?
  .union: (lambda (a b) (.call Table .merge (lambda (_ _ _) (some (void))) a b)) ;; : @ <- @ @
  .inter: (lambda (a b) (.call Table .merge (lambda (_ a b) (and a b (some (void)))) a b)) ;; : @ <- @ @
  .diff: (lambda (a b) (.call Table .merge (lambda (_ a b) (and (not b) a)))) ;; : @ <- @ @
  .compare: (lambda (a b) (.call Table .compare (lambda (_ _) 0) a b)) ;; : : Integer <- @ @
  .=?: (lambda (a b) (.call Table .=? a b)) ;; : Bool <- @ @
  .lens: (lambda (e) {get: (lambda (t) (.elt? t e)) set: (lambda (t v) (if v (.cons e t) (.remove t e)))})) ;; : (Lens Bool <- @) <- Elt
