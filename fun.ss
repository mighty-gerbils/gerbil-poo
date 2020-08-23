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

(.def (Wrapper. @ []
       .ap ;; : (Wrap t) <- t
       .unap) ;; : t <- (Wrap t)
  .bind: (lambda (x f) (f (.unap x))) ;; : u <- (Wrap t) (u <- t)
  .map: (lambda (f x) (.ap (f (.unap x))))) ;; : (Wrap u) <- (u <- t) (Wrap t)

(.def (Wrap. @ [methods.io<-wrap]
       T ;; : Type.
       Wrapper) ;; : Functor.
  .wrap: (.@ Wrapper .ap)
  .unwrap: (.@ Wrapper .unap)
  .bind/wrap: (.@ Wrapper .bind)
  .map/wrap: (.@ Wrapper .map))

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
