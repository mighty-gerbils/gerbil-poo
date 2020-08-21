;;; Simple instance of the table interface: a wrapper for clan/pure/dict/rationaldict

(export #t)

(import
  :std/iter :std/misc/list :std/misc/alist
  :clan/base :clan/list :clan/option
  :clan/pure/dict/rationaldict
  ./brace ./io ./mop ./number ./poo ./type ./table)

(.def (RationalDict. @ [methods.table] Value)
   sexp: '(RationalDict)
   Key: Rational
   Value: Any
   .validate:
   (lambda (x (ctx '()))
     (def c [[validate: x] . ctx])
     (unless (rationaldict? x) (type-error c "Not an rationaldict"))
     (unless (eq? Value Any)
       (for-each (match <> ([_k . v] (validate Value v c))) (rationaldict->list x)))
     x)
   ;; TODO: also use rationaldict-min-key, rationaldict-max-key, rationaldict-update ?
   .empty: empty-rationaldict
   .empty?: rationaldict-empty?
   .ref: rationaldict-ref
   .key?: rationaldict-has-key?
   .acons: (lambda (k v d) (rationaldict-put d k v))
   .remove: rationaldict-remove
   ;; TODO: expose rbtree-fold in clan/pure/dict/rationaldict
   .foldl: (lambda (f seed d) (foldl (lambda (kv acc) (f (car kv) (cdr kv) acc)) seed (rationaldict->list d)))
   .foldr: (lambda (f seed d) (foldr (lambda (kv acc) (f (car kv) (cdr kv) acc)) seed (rationaldict->list d)))
   .<-list: list->rationaldict
   .list<-: rationaldict->list
   .=?: (lambda (d1 d2) (rationaldict=? d1 d2 (.@ Value .=?))))
(def (RationalDict (Value Any))
  (if (eq? Value Any) RationalDict.
      {(:: @ RationalDict.) Value sexp: `(RationalDict ,(.@ Value sexp))}))
(.def (RationalSet @ [Set<-Table.] Table: RationalDict.))
