;;; Simple instance of the table interface: a wrapper for clan/pure/dict/rationaldict

(export #t)

(import
  :std/iter :std/misc/list :std/values
  :clan/base :clan/list :clan/option
  :clan/pure/dict/rationaldict
  ./object ./brace ./io ./mop ./number ./type ./table)

(define-type (RationalDict. @ [methods.table] Value)
   Key: Rational
   Value: Any
   .validate: => (λ (super)
                   (lambda (x (ctx '()))
                     (unless (rationaldict? x) (type-error [[validate: x] . ctx] "Not an rationaldict"))
                     (super x ctx)))
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
   .sexp<-: (lambda (x) `(list->rationaldict `,(rationaldict->list x)))
   .=?: (lambda (d1 d2) (rationaldict=? d1 d2 (.@ Value .=?))))
(def (RationalDict (Value Any))
  (if (eq? Value Any) RationalDict.
      {(:: @ RationalDict.) Value sexp: `(RationalDict ,(.@ Value sexp))}))

(define-type (RationalSet @ [Set<-Table.])
  Elt: Rational
  Table: {(:: @T RationalDict.) Key: Elt Value: Unit}
  .list<-: rationaldict-keys
  .min-elt: (compose first-value rationaldict-min-key)
  .max-elt: (compose first-value rationaldict-max-key))
