;;; Typeclasses for Functional Programming

(export #t)

(import
  :std/error :std/iter
  (only-in :clan/list acons)
  :clan/option
  ./poo ./mop ./brace ./number ./type ./io)

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

(.def (methods.io<-wrap @ [] T .wrap .unwrap)
  .marshal: (lambda (v port) (marshal T (.unwrap v) port))
  .unmarshal: (lambda (port) (.wrap (unmarshal T port)))
  .bytes<-: (lambda (v) (bytes<- T (.unwrap v)))
  .<-bytes: (lambda (b) (.wrap (<-bytes T b)))
  .json<-: (lambda (v) (json<- T (.unwrap v)))
  .<-json: (lambda (b) (.wrap (<-json T b))))

(.def (Wrap. @ [methods.io<-wrap]
       T ;; : Type.
       Wrapper) ;; : Functor.
  .wrap: (.@ Wrapper .ap) ;; : (Wrap t) <- t
  .unwrap: (.@ Wrapper .unap) ;; : t <- (Wrap t)
  .bind/wrap: (.@ Wrapper .bind) ;; : u <- (Wrap t) (u <- t) = (lambda (x f) (f (.unwrap x)))
  .map/wrap: (.@ Wrapper .map)) ;; : (Wrap u) <- (u <- t) (Wrap t) = (lambda (f x) (.wrap (f (.unwrap x))))

(.def (IdWrap @ Wrap.)
  Wrapper: Identity)


;; Dependent variant of Functor, taking explicit type parameters at runtime
(.def (Functor^. @ []
  .tap ;; : Type <- Type
  .ap^ ;; :  (Fun (@ a) <- (forall a <: Type) a)
  .map^)) ;; : (Fun (@ a) <- (forall a <: Type) (forall b <: Type) (Fun a <- b) (@ b))

(.def (Wrap^. @ [methods.io<-wrap]
       T ;; : Type.
       Wrapper^) ;; : Functor^.
  .wrap: (cut .call Wrapper^ .ap^ T <>) ;; : @ <- T
  .unwrap: (cut .call Wrapper^ .unap^ T <>)) ;; : T <- @
