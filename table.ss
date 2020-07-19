;;; Common declarations and methods for tables

(export #t)

(import
  :std/iter
  :clan/base :clan/list :clan/option
  :clan/poo/brace :clan/poo/io :clan/poo/mop :clan/poo/poo :clan/poo/type)

;; TODO: have interfaces more like LIL, less like OCaml?
;; Especially since we may (1) use similar metaprogramming for OO style (?), and
;; (2) distinguish between for-each / foldl / foldr (iterating over pairs)
;; vs for-each* / foldl* / afoldr (iterating with two separate arguments for k v)
;; (3) implement additional APIs like drop, decons, map/2, convert, etc.

;; NB: General methods for maps, that are not trie-specific
(.def (CommonTableMethod. @ []
       Key ;; : Type
       Value ;; : Type
       .empty ;; : @
       .acons ;; : @ <- Key Value @
       .ref ;; : Value <- @ Key ?(Value <-)
       .remove ;; : @ <- @ Key
       .foldl ;; : (Fun o <- (Fun o <- Key Value o) o @ ?Key)
       .foldr ;; : (Fun o <- (Fun o <- Key Value o) o @ ?Key)
       .merge ;; : @ <- (Fun (Option Value) <- Key (Option Value) (Option Value)) @ @
       .find-first/opt ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @
       .find-last/opt) ;; : (Option (Pair Key Value)) <- (Fun Bool <- Key Value) @

  ;; : (Fun Bool <- @ Key)
  .key?: (lambda (trie key) (let/cc ret (.ref trie key (lambda () (ret #f))) #t))

  ;; : (Fun (Option Value) <- @ Key)
  .ref/opt: (lambda (trie key) (let/cc ret (some (.ref trie key (lambda () (ret #f))))))

  ;; : Unit <- (Fun Unit <- Key Value) @
  .for-each: (lambda (f t) (.foldl (lambda (k v a) (void (f k v))) (void) t 0))

  ;; : Unit <- (Fun Unit <- Key Value) @
  .for-each/reverse: (lambda (f t) (.foldr (lambda (k v a) (void (f k v))) (void) t 0))

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
          (#t (.remove t k))
          ((some v) (.acons k v t)))))

  ;; : @ <- Key (Fun Value <- Value) @ ?(Fun Value <-)
  .update: (lambda (k f t (default false)) (.acons k (f (.ref t k default)) t))

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

  ;; : @ <- (Iterator (Pair Key Value)) ?@
  .<-iter: (lambda (s (t .empty)) (for/fold (t t) (kv s) (.acons (car kv) (cdr kv) t)))

  ;; : (Lens Value <- @) <- Key
  .lens: (lambda (k) {get: (lambda (t) (.ref t k)) set: (lambda (t v) (.acons k v t))})

  ;; : Json <- @
  .json<-: (lambda (t) (.foldr (lambda (k v l) (cons [(json<- Key k) (json<- Value v)] l)) '() t))

  ;; : @ <- Json
  .<-json: (lambda (j) (foldl (lambda (x t) (match x ([k v] (.acons (<-json Key k) (<-json Value v) t)))) .empty j)))

(.def (Set<-Table. @ Type. Table)
  ;; Table must be a table from Elt to Unit, i.e. (.@ Table Key) == Elt, (.@ Table Value) == Unit
  Elt: (.@ Table Key) ;; : Type
  .empty: (.@ Table .empty)  ;; : @
  .empty?: (.@ Table .empty?) ;; : Bool <- @
  .elt?: (.@ Table .key?) ;; : Bool <- @ Elt
  .cons: (cut .call Table acons <> (void) <>) ;; : @ <- Elt @
  .singleton: (lambda (elt) (.call Table .singleton elt (void))) ;; : @ <- Elt
  .remove: (.@ Table .remove) ;; : @ <- @ Elt
  .for-each: (lambda (f t) (.call Table .for-each (lambda (e _) (f e)) t)) ;; : Unit <- (Unit <- Elt) @
  .for-each/reverse: (lambda (f t) (.call Table .for-each/reverse (lambda (e _) (f e)) t))
  .foldl: (lambda (f a t) (.call Table .foldl (lambda (e _ a) (f e a)) a t)) ;; : a <- (a <- Elt a)
  .foldr: (lambda (f a t) (.call Table .foldr (lambda (e _ a) (f e a)) a t)) ;; : a <- (a <- Elt a)
  .every: (lambda (f t) (.call Table .every (lambda (e _) (f e)) t)) ;; : Bool <- (Bool <- Elt) @
  .any: (lambda (f t) (.call Table .any (lambda (e _) (f e)) t)) ;; : Bool <- (Bool <- Elt) @
  .filter: (lambda (f t) (.call Table .filter (lambda (e _) (f e)) t)) ;; : @ <- (Bool <- Elt) @
  .partition: (lambda (f t) (.call Table .partition (lambda (e _) (f e)) t)) ;; : @ @ <- (Bool <- Elt) @
  .count: (.@ Table .count) ;; : Nat <- @
  .list<-: (lambda (t) (map car (.call Table .list<- t))) ;; : (List Elt) <- @
  .<-list: (lambda (l) (foldl .cons .empty l)) ;; : @ <- (List Elt)
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

  ;; TODO: for union, inter, diff, compare, equal, subset,
  ;; optimize for full subtries, by caching count in wrapper?
  .union: (lambda (a b) (.call Table .merge (lambda (_ _ _) (some (void))) a b)) ;; : @ <- @ @
  .inter: (lambda (a b) (.call Table .merge (lambda (_ a b) (and a b (some (void)))) a b)) ;; : @ <- @ @
  .diff: (lambda (a b) (.call Table .merge (lambda (_ a b) (and (not b) a)))) ;; : @ <- @ @
  .compare: (lambda (a b) (.call Table .compare (lambda (_ _) 0) a b)) ;; : : Integer <- @ @
  .equal?: (lambda (a b) (.call Table .equal? a b)) ;; : Bool <- @ @
  .lens: (lambda (e) {get: (lambda (t) (.elt? t e)) set: (lambda (t v) (if v (.cons e t) (.remove t e)))})) ;; : (Lens Bool <- @) <- Elt

