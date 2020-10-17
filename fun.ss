;;; Typeclasses for Functional Programming

(export #t)

(import
  :std/error :std/iter
  (only-in :clan/list acons)
  :clan/option
  ./poo ./mop ./brace ./number ./type ./io)

(.def (Category. @ Type. ;; The Category is identified to the type of its objects/nodes/states/points
  ;; @ : Type ;; objects of the category, points of the space, states of the computation…
  Arrow ;; : Type ;; (homo)morphisms of the category, transformations, state transitions with effects…
  domain ;; : @ <- Arrow ;; start node of an arrow
  codomain ;; : @ <- Arrow ;; end node of an arrow
  compose ;; : Arrow <- Arrow Arrow ;; given arrows A<-B and B<-C, return an arrow A<-C
  ;; logical constraint: associativity law for compose
  identity ;; : Arrow <- @ ;; given a node A, an identity node A<-A
  ;; left- and right- identity laws for compose
  ;; also equality predicate and laws for that?
  ))

(.def (Functor. @ Type.
  Domain Codomain ;; functor C<-D from D to C
  .ap ;; : Codomain <- Domain
  .map)) ;; : Codomain.Arrow <- Domain.Arrow

(.def (ParametricFunctor. @ [Functor.] ;; BlindParametric
  .tap ;; : Type <- Type ;; computes the Codomain from the Domain
  .ap ;; : (forall a (Fun (.tap a) <- a)) ;; the code does NOT depend on the input type!
  .map)) ;; : (forall a b (Fun Fun (.tap a) <- (.tap b)) (Fun a <- b))) ;; the code does NOT depend on the input type!

(.def (Identity @ ParametricFunctor.) ;; also a monad
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
