;; Simple polynomials as arrays of terms of a given field

(export #t)

(import
  :std/error
  :std/iter
  :std/misc/number
  :std/sugar
  :std/values
  :clan/base
  :std/debug/DBG
  (only-in :std/srfi/133 vector-map vector-index-right)
  ./object ./mop ./brace ./number ./type ./zn)

(defrule (let0 (x init) body ...) (let ((x init)) body ... x))

;; (Univariate) Polynomials
(define-type (Polynomial. @ [expt<-mul.] .expt .mul-expt)
  .Ring: ;; commutative ring (or field, etc.) that the polynomial is over
  Real ;; as a useful default, use Real numbers
  .element?:
  (lambda (x) (and (vector? x) (vector-every (.@ .Ring .element?) x)))
  .zero:
  #()
  .one:
  (vector (.@ .Ring .one))
  .intscale: ;; multiply by a integer scalar
  (lambda (k P) (let (intscale (.@ .Ring .intscale)) (vector-map (cut intscale k <>) P)))
  .scale: ;; multiply by a scalar, being an element of the Ring
  (lambda (k P) (let (mul (.@ .Ring .mul)) (vector-map (cut mul k <>) P)))
  .normalize:
  (lambda (P) ;; strip leading terms that are zeros
    (let* ((= (.@ .Ring .=?))
           (O (.@ .Ring .zero))
           (d (vector-index-right (lambda (x) (not (= O x))) P))) ;; degree of the polynomial
      (cond
       ((equal? d (1- (vector-length P))) P)
       (d (subvector P 0 (1+ d)))
       (else #()))))
  .add:
  (lambda (P Q)
    (let retry ((P P) (Q Q))
      (if (< (vector-length P) (vector-length Q))
        (retry Q P)
        (let ((S (vector-copy P))
              (add (.@ .Ring .add)))
          (for (i (iota (vector-length Q)))
            (vector-set! S i (add (vector-ref P i) (vector-ref Q i))))
          (.normalize S)))))
  .sub:
  (lambda (P Q)
    (let (sub (.@ .Ring .sub))
      (if (>= (vector-length P) (vector-length Q))
        (let (D (vector-copy P))
          (for (i (iota (vector-length Q)))
            (vector-set! D i (sub (vector-ref P i) (vector-ref Q i))))
          (.normalize D))
        (let (D (make-vector (vector-length Q)))
          (for (i (iota (vector-length P)))
            (vector-set! D i (sub (vector-ref P i) (vector-ref Q i))))
          (for (i (iota (- (vector-length Q) (vector-length P)) (vector-length P)))
            (vector-set! D i (sub (.@ .Ring .zero) (vector-ref Q i))))
          (.normalize D)))))
  .neg:
  (lambda (P) (vector-map (.@ .Ring .neg) P))
  .=?:
  (lambda (a b)
    (and (= (vector-length a) (vector-length b))
         (vector-every (.@ .Ring .=?) a b)))
  .degree:
  (lambda (P) (let (l (vector-length P)) (if (zero? l) -inf.0 (1- l))))
  .mul:
  (lambda (P Q) ;; simple multiplication in O(n^2)
    (if (or (equal? P #()) (equal? Q #()))
      #()
      (let* ((degP (.degree P))
             (degQ (.degree Q))
             (PQ (make-vector (+ 1 degP degQ) (.@ .Ring .zero)))
             (add (.@ .Ring .add))
             (mul (.@ .Ring .mul)))
      ;; for i across indexes
      (for (i (iota (1+ degP)))
        (for (j (iota (1+ degQ)))
          (let (k (+ i j))
            (vector-set! PQ k (add (vector-ref PQ k) (mul (vector-ref P i) (vector-ref Q j)))))))
      PQ)))
  .differentiate:
  (lambda (P)
    (let (l (1- (vector-length P)))
      (if (positive? l)
        (let (dP (make-vector l))
          (for (i (iota l 1))
            (vector-set! dP (1- i)
                         (.call .Ring .intscale i (vector-ref P i))))
          (.normalize dP))
        #())))
  .division: ;; Euclidian division, assuming the Ring is a division ring
  (lambda (P Q) ;; return q & r such that P = q*Q+r, deg(r) < deg(Q)
    (if (equal? Q #()) (error "Cannot divide polynomial by zero")
        (let ((degP (.degree P))
              (degQ (.degree Q)))
          (if (< degP degQ)
            (values #() P)
            (let ((q (make-vector (- degP degQ -1) (.@ .Ring .zero)))
                  (r (vector-copy P))
                  (div (.@ .Ring .div))
                  (sub (.@ .Ring .sub))
                  (mul (.@ .Ring .mul))
                  (=? (.@ .Ring .=?))
                  (O (.@ .Ring .zero)))
              ;; Recursively compute coefficients of the quotient and rest
              (let loop ((i (- degP degQ)))
                (let ((c (div (vector-ref r (+ i degQ)) (vector-ref Q degQ))))
                  (vector-set! q i c)
                  (for (j (iota degQ)) ;; could be (1+ degQ), but we leave the top term unzeroed
                    (let (k (+ j i))
                      (vector-set! r k (sub (vector-ref r k) (mul c (vector-ref Q j)))))))
                (if (positive? i)
                  (loop (1- i))
                  ;; Compute the degree of the rest and strip the tail of the vector
                  (let lp2 ((dr (1- degQ)))
                    (cond
                     ((negative? dr) (values q #()))
                     ((=? O (vector-ref r dr)) (lp2 (1- dr)))
                     (else (values q (subvector r 0 (1+ dr)))))))))))))
  .apply:
  (lambda (P x)
    (def l (vector-length P))
    (match l
      (0 (.@ .Ring .zero))
      (1 (vector-ref P 0))
      (else (let ((add (.@ .Ring .add))
                  (mul (.@ .Ring .mul)))
              (let loop ((r (vector-ref P 0)) (i 1) (xi x))
                (let ((s (add r (mul (vector-ref P i) xi)))
                      (j (1+ i)))
                (if (= j l) s (loop s j (mul x xi)))))))))


;; Given a list of points xs of length N, return a function that given a list ys computes
;; the unique polynomial of degree N-1 or less that goes through the points at specified coordinates
(def (lagrange-interpolation Poly xs)
  (if (null? xs)
    (lambda (ys) (check-argument (null? ys) "more ys than xs" [xs ys]) #())
    (let* ((Rone (.@ Poly .Ring .one))
           (Rneg (.@ Poly .Ring .neg))
           (Rinv (.@ Poly .Ring .inv))
           (add (.@ Poly .add))
           (mul (.@ Poly .mul))
           (scale (.@ Poly .scale))
           (division (.@ Poly .division))
           (apply (.@ Poly .apply))
           (X-xs (map (lambda (x) (vector (Rneg x) Rone)) xs))
           (PP (foldl mul (car X-xs) (cdr X-xs))) ;; product of all terms X-x
           (LL (map (lambda (x X-x)
                      (let* ((P (first-value (division PP X-x)))
                             (y (apply P x)))
                        (scale (Rinv y) P)))
                    xs X-xs)))
      (lambda (ys) (foldl add #() (map scale ys LL))))))
