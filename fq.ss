;; Arithmetics on Finite Field 𝐹_q, a.k.a. GF(q),
;; where q=p^n, p is prime, n is a positive natural number.
;;
;; All elements verify x^q=x. Non-zero elements verify x^(q-1)=1--they are all roots of unity.
;; Some are primitive roots, i.e. they do not verify x^d=1 for d < q-1 (and d divides q-1).
;; Now for d|q-1, d<q-1, m*q=q-1, we have x^(d*m)=1
;;
;; NB: not safe for side-channels, do not use to compute with cryptographic secrets!
;; NB: also not particularly optimized at this point, but should get the project off the ground.

(export #t)

(import
  :std/error
  :std/iter
  :std/misc/number
  :clan/base
  (only-in :std/srfi/133 vector-map)
  ./object ./mop ./brace ./number ./type)


;; TODO: refactor into two things, one for generic Galois extension, another specialized for GL(q).
;; Operations assume numbers are in canonical normalized forms
(define-type (F_q. @ [expt<-mul-inv.] .expt .mul-expt)
  .q: (expt .p .n)
  .p: undefined
  .n: undefined ;; Note: we assume 1 < n, or the construction is pointless
  .xn: undefined ;; value of x^n; can also be also a polynomial such that F_q is Z/pZ[x]/(x^n-xn)
  ;; chosen to have few non-zero coefficients, preferably small ones (1 or -1),
  ;; and to be the minimal polynomial of a primitive element
  ;; typically, x^n-xn is the Conway polynomial for that field and dimension.
  .Z/pZ: (Z/ .p)
  ;; Primitive element, generator of the multiplicative group, zero of poly above,
  .x: (.<-n .p) ;; NB: only defined for n>1
  ;; (let (X (.new)) (vector-set! X 1 1) X)
  .element?: (lambda (x) (and (vector? x)
                         (= (vector-length x) .n)
                         (vector-every (.@ .Z/pZ .element?) x)))
  .new: (lambda () (make-vector .n 0))
  .zero: (.new)
  .one: (let (I (.new))
          (vector-set! I 0 1)
          I)
  .intscale: (lambda (m a) ;; multiply by an integer scalar
               (vector-map (cut .call .Z/pZ .intscale m <>) a))
  .scale: (lambda (m a) ;; multiply by a scalar, being an element of Z/pZ
            (vector-map (cut .call .Z/pZ .mul m <>) a))
  .add: (lambda (a b) (vector-map (.@ .Z/pZ .add) a b))
  .sub: (lambda (a b) (vector-map (.@ .Z/pZ .sub) a b))
  .neg: (lambda (a) (vector-map (.@ .Z/pZ .neg) a))
  .=?: (lambda (a b)
         (vector-every = a b))
  .mul: (lambda (m a)
          (let ((result (.new))
                (a (vector-copy a))
                (add (.@ .Z/pZ .add))
                (mul (.@ .Z/pZ .mul))
                (n-1 (1- .n)))

            ;; for i across indexes
            (for (i (iota .n))
              ;; add in the ith multiplier time the ith coefficient
              (let (c (vector-ref m i))
                (for (j (iota .n))
                  (vector-set! result j
                               (add (vector-ref result j)
                                    (mul c (vector-ref a j))))))
              ;; adjust the multiplier
              (let (c (vector-ref a n-1))
                (for (j (iota .n n-1 -1))
                  (vector-set! a j (add (if (zero? j) 0 (vector-ref a (1- j)))
                                        (mul c (vector-ref .xn j)))))))
            result))
  .inv: (lambda (a)
          (check-argument (not (.=? .zero a)) "Zero has no inverse" a)
          (.expt a (- .q 2)))
  .div: (lambda (a b)
          (check-argument (not (.=? .zero b)) "Cannot divide by zero" [a b])
          (.mul-expt a b (- .q 2)))
  .n<-: ;; Convert to packed bignum from working representation
  (lambda (v)
    (let loop ((s 0) (i .n))
      (if (zero? i) s
          (let (j (1- i))
            (loop (+ (* s .p) (vector-ref v j)) j)))))
  .<-n: ;; Convert to working representation from packed bignum
  (lambda (n)
    (let (v (.new))
      (let loop ((n n) (i 0))
        (if (>= i .n) v
            (let-values (((qq rr) (floor/ n .p)))
              (vector-set! v i rr)
              (loop qq (1+ i))))))))

(def (F_q p n xn)
  {(:: @ [F_q.]) .p: p .n: n .xn: xn})

;; For p=2, n>=1, represent elements with integers rather than vectors of bits
(define-type (F_2^n. @ [F_q.] .n .xn .expt .mul-expt)
  .p: 2
  .element?: (lambda (x) (and (exact-integer? x)
                         (not (negative? x))
                         (< (integer-length x) .n)))
  .new: (lambda () 0)
  .zero: 0
  .one: 1
  .intscale: (lambda (m a) (if (even? m) 0 a)) ;; multiply by a scalar, being an integer
  .scale: .intscale ;; multiply by a scalar, being an element of Z/2Z
  .add: bitwise-xor
  .sub: bitwise-xor
  .neg: identity
  .=?: =
  .mulx: (lambda (a)
           (if (bit-set? (1- .n) a)
             (bitwise-xor .xn (replace-bit-field (1- .n) 1 a 0))
             (arithmetic-shift a 1)))
  .mul:
  (lambda (m a)
    (if (zero? m)
      0
      (let loop ((result 0)
                 (i 0)
                 (a a))
        (let* ((c? (bit-set? i m))
               (r (if c? (bitwise-xor result a) result)) ;; TODO: make it constant-time?
               (j (1+ i)))
          (if (< j .n)
            (loop r j (.mulx a))
            r)))))
  .n<-: identity ;; Convert to packed bignum from working representation
  .<-n: identity) ;; Convert to working representation from packed bignum

;; For 𝐹_2^8 we use the same irreducible polynomial as AES / Rijndael: 𝑝(𝑥)=𝑥^8+𝑥^4+𝑥^3+𝑥+1
(define-type (F_2^8 @ [F_2^n.])
  .n: 8 .xn: 27)

;; For 𝐹_2^64 we use the NIST SP800-38D (GCM) Polynomial 𝑝(𝑥)=𝑥^64+𝑥^4+𝑥^3+𝑥+1
(define-type (F_2^64 @ [F_2^n.])
  .n: 64 .xn: 27)

;; For 𝐹_2^256, we use 𝑝(𝑥)=𝑥^256+𝑥^10+𝑥^5+𝑥^2+1 https://math.stackexchange.com/questions/4815536/what-is-the-conway-polynomial-for-gf2256
(define-type (F_2^256 @ [F_2^n.])
  .n: 256 .xn: 1061)
