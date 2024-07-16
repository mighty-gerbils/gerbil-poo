(export fq-test)

(import
  :gerbil/gambit
  :std/assert :std/format
  :std/iter
  :std/misc/repr
  :std/sort
  :std/srfi/13
  :std/sugar :std/test
  :clan/assert :clan/base :clan/debug
  ../object ../mop ../number ../type ../brace ../fq)


(def F_3^2 {(:: @ [F_q.]) .p: 3 .n: 2 .xn: #(1 1)})
(def F_2^3 {(:: @ [F_2^n.]) .n: 3 .xn: #(1 1 0)})

(def (F_q<-F_2^n F)
  {(:: @ [F_q.] .<-n) .p: 2 .n: (.@ F .n) .xn: (.<-n (.@ F .xn))})
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

(def fq-test
  (test-suite "test suite for clan/poo/fq"
    (test-case "test vector-andmap utility"
      (check (vector-andmap false) => #t)
      (check (vector-andmap odd? #(1 3 5 9 13)) => #t)
      (check (vector-andmap odd? #(1 3 8)) => #f)
      (check (vector-andmap = #(1 3 8) #(1 3 8)) => #t)
      (check (vector-andmap = #(1 3 8) #(1 5 8)) => #f))
    (test-case "test mul-expt<-mul"
      (check (mul-expt<-mul + 5 10 7) => 75)
      (check (mul-expt<-mul * 42 42 22) => (expt 42 23))
      (check (mul-expt<-mul * 42 42 22 8) => (expt 42 23)))
    (test-case "simple tests for F_2^8"
      (f2check (F_2^8 <-n n<- + * inv expt)
        (check (+ #x57 0) => #x57)
        (check (+ #x57 0) => #x57)
        (check (* #x83 1) => #x83)
        (check (* #x83 0) => #x0)
        (check (* #x02 02) => #x04)
        (check (* #x04 #x04) => #x10)
        (check (* #x10 #x10) => #x1b)
        (check (+ #x57 #x83) => #xD4)
        (check (* #x57 #x83) => #xC1)
        (check (* #x53 #xCA) => #x01)
        (check (expt #x02 0) => #x01)
        (check (expt #x02 1) => #x02)
        (check (expt #x02 2) => #x04)
        (check (expt #x02 3) => #x08)
        (check (expt #x02 4) => #x10)
        (check (expt #x02 255) => #x01)
        (check (expt #x53 254) => #xCA)
        (check (inv #x53) => #xCA)
        (check (inv #xCA) => #x53)))
    (test-case "simple tests for F_3^2"
      (check (.@ F_3^2 .p) => 3)
      (fqcheck (F_3^2 <-n n<- + * inv expt)
        (for (i (iota 9))
          (DBG foo: i (<-n i) (n<- (<-n i)))
          (check (n<- (<-n i)) => i))
        (check (for/collect (i (iota 9))
                 (for/collect (j (iota 9))
                   (+ i j)))
               => '((0 1 2 3 4 5 6 7 8)
                    (1 2 0 4 5 3 7 8 6)
                    (2 0 1 5 3 4 8 6 7)
                    (3 4 5 6 7 8 0 1 2)
                    (4 5 3 7 8 6 1 2 0)
                    (5 3 4 8 6 7 2 0 1)
                    (6 7 8 0 1 2 3 4 5)
                    (7 8 6 1 2 0 4 5 3)
                    (8 6 7 2 0 1 5 3 4)))
        (check (for/collect (i (iota 9))
                 (for/collect (j (iota 9))
                   (* i j)))
               => '((0 0 0 0 0 0 0 0 0)
                    (0 1 2 3 4 5 6 7 8)
                    (0 2 1 6 8 7 3 5 4)
                    (0 3 6 4 7 1 8 2 5)
                    (0 4 8 7 2 3 5 6 1)
                    (0 5 7 1 3 8 2 4 6)
                    (0 6 3 8 5 2 4 1 7)
                    (0 7 5 2 6 4 1 8 3)
                    (0 8 4 5 1 6 7 3 2)))))
    ;; Beware: the test vectors below have not been independently checked!
    (test-case "simple tests for F_2^64"
      (f2check (F_2^64 <-n n<- + * inv expt)
        (check (+ #x123456789ABCDEF0 0) => #x123456789ABCDEF0)
        (check (+ #x123456789ABCDEF0 #x0FEDCBA987654321) => #x1dd99dd11dd99dd1)
        (check (* #x123456789ABCDEF0 #x0FEDCBA987654321) => #x48827ab55d976fa0)))
    (test-case "simple tests for F_2^256"
      (f2check (F_2^256 <-n n<- + * inv expt)
        (check (+ #x123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0
                  #x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321)
               => #X1DD99DD11DD99DD11DD99DD11DD99DD11DD99DD11DD99DD11DD99DD11DD99DD1)
        (check (* #x123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0
                  #x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321)
               => #x9F6499CE926995CB976C91C69A619DC39F6499CE926995CB976C91C69A6191AF)))))
