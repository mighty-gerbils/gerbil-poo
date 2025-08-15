;;; Typeclasses for Functional Programming

(export #t)

(import
  :std/error :std/iter
  (only-in :clan/list acons)
  :clan/option
  ./object ./mop ./brace ./number ./type ./io)

(define-type (Category. @ Type. ;; The Category is identified to the type of its objects/nodes/states/points
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

(define-type (Functor. @ Type.
  Domain Codomain ;; functor C<-D from D to C
  .ap ;; : Codomain <- Domain
  .map)) ;; : Codomain.Arrow <- Domain.Arrow

(define-type (ParametricFunctor. @ [Functor.] ;; BlindParametric
  .tap ;; : Type <- Type ;; computes the Codomain from the Domain
  .ap ;; : (forall a (Fun (.tap a) <- a)) ;; the code does NOT depend on the input type!
  .map)) ;; : (forall a b (Fun Fun (.tap a) <- (.tap b)) (Fun a <- b))) ;; the code does NOT depend on the input type!

(define-type (Identity @ ParametricFunctor.) ;; also a monad
  .tap: identity
  .ap: identity
  .map: identity
  .unap: identity ;; this functor also has an inverse, itself
  .Log: Unit
  .bind: (lambda (x f) (f x)))

(define-type (methods.io<-wrap @ [] T .wrap .unwrap)
  .marshal: (lambda (v port) (marshal T (.unwrap v) port))
  .unmarshal: (lambda (port) (.wrap (unmarshal T port)))
  .bytes<-: (lambda (v) (bytes<- T (.unwrap v)))
  .<-bytes: (lambda (b) (.wrap (<-bytes T b)))
  .json<-: (lambda (v) (json<- T (.unwrap v)))
  .<-json: (lambda (b) (.wrap (<-json T b))))

(define-type (Wrapper. @ []
       .ap ;; : (Wrap t) <- t
       .unap) ;; : t <- (Wrap t)
  .bind: (lambda (x f) (f (.unap x))) ;; : (Wrap t) <- (Wrap u) ((Wrap t) <- u)
  .map: (lambda (f x) (.ap (f (.unap x))))) ;; : (Wrap t) <- (t <- u) (Wrap u)

(define-type (Wrap. @ [methods.io<-wrap Type.] ;; type of wrapped item
       T ;; : Type. ;; unwrapped
       Wrapper ;; : Functor. ;; @ = (Wrapper T)
       .wrap?) ;; : Bool <- Any
  .element?: (lambda (x) (and (.wrap? x) (.call T .element? (.unwrap x))))
  .wrap: (.@ Wrapper .ap)
  .unwrap: (.@ Wrapper .unap)
  .bind/wrap: (.@ Wrapper .bind)
  .map/wrap: (.@ Wrapper .map))

;; TODO: pass-through methods for marshalling, etc.
(define-type (IdWrap @ Wrap. T)
  Wrapper: Identity
  )

;; Dependent variant of Functor, taking explicit type parameters at runtime
(define-type (Functor^. @ []
  .tap ;; : Type <- Type
  .ap^ ;; :  (Fun (@ a) <- (forall a <: Type) a)
  .map^)) ;; : (Fun (@ a) <- (forall a <: Type) (forall b <: Type) (Fun a <- b) (@ b))

(define-type (Wrap^. @ [methods.io<-wrap]
       T ;; : Type.
       Wrapper^) ;; : Functor^.
  .wrap: (cut .call Wrapper^ .ap^ T <>) ;; : @ <- T
  .unwrap: (cut .call Wrapper^ .unap^ T <>)) ;; : T <- @
