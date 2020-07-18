;;; Typeclasses for Functional Programming

(export #t)

(import
  :std/error :std/iter
  (only-in :clan/list acons)
  :clan/option
  :clan/poo/poo :clan/poo/mop :clan/poo/brace :clan/poo/number :clan/poo/type)

(.def (Functor. @ Type.
       .tap ;; : Type <- Type
       .ap ;; : (forall a (Fun (@ a) <- a))
       .map)) ;; : (forall a b (Fun (Fun (@ a) <- (@ b)) (Fun a <- b)))

(.def (Identity @ Functor.)
  .tap: identity
  .ap: identity
  .map: identity
  .unap: identity
  .Log: Unit)

(.def (Wrap. @ Type.
       T ;; : Type.
       Wrapper) ;; : Functor.
  .wrap: (.@ Wrapper .ap)
  .unwrap: (.@ Wrapper .unap))

(.def (IdWrap @ Wrap.)
  Wrapper: Identity)
