(export polynomial-test)

(import
  :gerbil/gambit
  :std/assert :std/format
  :std/iter
  :std/misc/repr
  :std/sort
  :std/srfi/13
  :std/sugar :std/test
  :clan/assert :clan/base :clan/debug
  ../object ../mop ../number ../type ../brace ../fq ../polynomial)


(defrule (fqcheck (F <-n n<- + * inv expt) checks ...)
  (let ()
    (def (<-n n) (.call F .<-n n))
    (def (n<- a) (.call F .n<- a))
    (def (+ a b) (n<- (.call F .add (<-n a) (<-n b))))
    (def (* a b) (n<- (.call F .mul (<-n a) (<-n b))))
    ;;(def (/ a b) (n<- (.call F .div (<-n a) (<-n b))))
    (def (inv a) (n<- (.call F .inv (<-n a))))
    (def (expt a b) (n<- (.call F .expt (<-n a) b)))
    checks ...))
(defrule (f2check (F <-n n<- + * inv expt) checks ...)
  (begin
    (DBG "Testing F_2^n directly" 'n (.@ F .n))
    (fqcheck (F <-n n<- + * inv expt) checks ...)
    (DBG "Testing F_2^n via generic F_q" 'n (.@ F .n))
    (fqcheck ((F_q<-F_2^n F) <-n n<- + * inv expt) checks ...)))

(def polynomial-test
  (test-suite "test suite for clan/poo/polynomial"
    (test-case "test .add"
      (check (.call Polynomial. .add #(1 -2 1) #(1 2 3 4)) => #(2 0 4 4))
      (check (.call Polynomial. .add #(1 2 3) #(4 5 6)) => #(5 7 9))
      (check (.call Polynomial. .add #(1 -2 1) #(1 2 -1)) => #(2))
      (check (.call Polynomial. .add #(1 2 3) #(-1 -2 -3)) => #()))
    (test-case "test .sub"
      (check (.call Polynomial. .sub #(1 2 3 4) #(-1 2 -1)) => #(2 0 4 4))
      (check (.call Polynomial. .sub #(1 -2 1) #(-1 -2 -3 -4)) => #(2 0 4 4))
      (check (.call Polynomial. .sub #(1 2 3) #(4 5 6)) => #(-3 -3 -3))
      (check (.call Polynomial. .sub #(1 -2 1) #(1 2 1)) => #(0 -4))
      (check (.call Polynomial. .sub #(1 2 3) #(1 2 3)) => #()))
    (test-case "test .=?"
      (check (.call Polynomial. .=? #(1 2 3 4) #(5 6 7 8)) => #f)
      (check (.call Polynomial. .=? #(1 2 3 4) #(1 2 3 4)) => #t)
      (check (.call Polynomial. .=? #(1 2 3 4) #(1 2 3)) => #f)
      (check (.call Polynomial. .=? #() #()) => #t)
      (check (.call Polynomial. .=? #(1 2 3) #()) => #f))
    (test-case "test .degree"
      (check (.call Polynomial. .degree #(1 2 3 4)) => 3)
      (check (.call Polynomial. .degree #()) => -inf.0)
      (check (.call Polynomial. .degree #(1)) => 0))
    (test-case "test .mul"
      (check (.call Polynomial. .mul #(1 -2 1) #(1 2 3 4)) => #(1 0 0 0 -5 4))
      (check (.call Polynomial. .mul #(1 2 3) #(4 5 6)) => #(4 13 28 27 18))
      (check (.call Polynomial. .mul #(1 -2 1) #(1 2 -1)) => #(1 0 -4 4 -1))
      (check (.call Polynomial. .mul #(1 2 3) #(-1 -2 -3)) => #(-1 -4 -10 -12 -9)))
    (test-case "test .differentiate"
      (check (.call Polynomial. .differentiate #(1 -2 1)) => #(-2 2))
      (check (.call Polynomial. .differentiate #(4 13 28 27 18)) => #(13 56 81 72))
      (check (.call Polynomial. .differentiate #(1 0 -4 4 -1)) => #(0 -8 12 -4)))
    (test-case "test .division"
      (check (values->list (.call Polynomial. .division #(1 2 1) #(1 1))) => [#(1 1) #()])
      (check (values->list (.call Polynomial. .division #(1 0 -4 4 -1) #(1 -2 1))) => [#(1 2 -1) #()])
      (check (values->list (.call Polynomial. .division #(6 3 -4 4 -1) #(1 2 -1))) =>
             [#(1 -2 1) #(5 3)]))
    (test-case "test .apply"
      (check (.call Polynomial. .apply #(1 -2 1) 1) => 0)
      (check (.call Polynomial. .apply #(1 -2 1) 3) => 4))))
