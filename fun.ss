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

(.def (Identity @ Functor.) ;; also a monad
  .tap: identity
  .ap: identity
  .map: identity
  .unap: identity ;; this functor also has an inverse, itself
  .Log: Unit
  .bind: (lambda (x f) (f x)))

(.def (Wrap. @ Type.
       T ;; : Type.
       Wrapper) ;; : Functor.
  .wrap: (.@ Wrapper .ap) ;; : (Wrap t) <- t
  .unwrap: (.@ Wrapper .unap) ;; : t <- (Wrap t)
  .bind/wrap: (.@ Wrapper .bind) ;; : u <- (Wrap t) (u <- t) = (lambda (x f) (f (.unwrap x)))
  .map/wrap: (.@ Wrapper .map)) ;; : (Wrap u) <- (u <- t) (Wrap t) = (lambda (f x) (.wrap (f (.unwrap x))))

(.def (IdWrap @ Wrap.)
  Wrapper: Identity)
