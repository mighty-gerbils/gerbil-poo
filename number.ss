;;-*- Gerbil -*-
;;; Number Type descriptors for POO and its MOP

;; TODO: a better type hierarchy
;; TODO: clear contracts for + vs add (overflow check vs modular?), etc.
(export #t)

(import
  (only-in :std/srfi/141 floor-quotient)
  (only-in :std/error check-argument)
  (only-in :std/misc/bytes big u8vector-double-ref u8vector-double-set!
           sint->u8vector u8vector->sint uint->u8vector u8vector->uint)
  (only-in :std/misc/hash hash-ensure-ref)
  (only-in :std/misc/number uint? uint-below? n-bits->n-u8 div-mod
           uint-of-length? sint-of-length? normalize-sint normalize-uint)
  (only-in :clan/base λ compose number-comparer)
  (only-in :clan/io write-varuint read-varuint write-varint read-varint)
  (only-in ./mop define-type Type.)
  (only-in ./brace @method)
  (only-in ./io methods.marshal<-fixed-length-bytes))

;; TODO: basic interface for arithmetics, with proper type signatures.
(define-type (Number @ Type. .validate)
  .element?: number?
  .sexp<-: identity
  .add: +
  .sub: -
  .neg: -
  .mul: *
  .div: /
  .inv: /
  .zero: 0
  .one: 1
  .<-string: (lambda (x) (.validate (string->number x)))
  .string<-: number->string
  .=?: (cut = <> <>))

(define-type (Real @ Number)
  .element?: real?
  ;; function taking two entries a, b.
  ;; -- If a = b then returns 0;
  ;; -- If a > b then returns 1
  ;; -- If a < b then returns -1
  ;; (-- If the numbers are not comparable, returns #f)
  .comparer: number-comparer
  .non-negative?: (cut <= 0 <>)
  .sign: (cut number-comparer <> 0)
  .<-json: identity
  .json<-: identity
  .<: <
  .<=: <=
  .>: >
  .>=: >=
  .intscale: *
  .max: max
  .min: min)

(define-type (Integer @ [Real] .string<- .<-string)
  .element?: exact-integer?
  .json<-: (lambda (x) (if (<= (integer-length x) 53) x (.string<- x)))
  .<-json: (lambda (x) (if (exact-integer? x) x (.<-string x)))
  .marshal: write-varint
  .unmarshal: read-varint
  .bytes<-: sint->u8vector ;; note: encoding shorter (and thus different) from the marshalling
  .<-bytes: u8vector->sint
  .div: floor-quotient
  .inv: (lambda (x) (case x ((1 -1) x) (else (error "Cannot invert integer" x))))
  .mod: modulo
  .logand: bitwise-and
  .logor: bitwise-ior
  .logxor: bitwise-xor
  .lognot: bitwise-not ;; more bitwise operations, see Gambit
  .shift-left: arithmetic-shift
  .shift-right: (λ (x n) (arithmetic-shift x (- n)))
  ;; extract-bit-field test-bit-field? integer-length integer-length<? ...
  .succ: 1+
  .pred: 1-)

(define-type (UInt @ Integer)
  .marshal: write-varuint
  .unmarshal: read-varuint
  .bytes<-: uint->u8vector ;; note: encoding shorter (and thus different) from the marshalling
  .<-bytes: u8vector->uint
  .sub: (lambda (x y) (if (>= x y) (- x y) (error "Overflow" '.sub x y)))
  .neg: (lambda (x) (if (zero? x) 0 (error "Overflow" '.neg x)))
  .pred: (lambda (x) (if (zero? x) (error "Overflow" '.pred x) (1- x)))
  .non-negative?: true)

;; IntegerRange between .most-negative and .most-positive included
(define-type (IntegerRange. @ Integer .most-negative .most-positive)
  .element?:
   (match (vector .most-negative .most-positive)
     ((vector #f #f) exact-integer?)
     ((vector _ #f) (λ (x) (and (exact-integer? x) (<= .most-negative x))))
     ((vector #f _) (λ (x) (and (exact-integer? x) (<= x .most-positive))))
     ((vector _ _) (λ (x) (and (exact-integer? x) (<= .most-negative x .most-positive))))))
(def (IntegerRange min: (.most-negative #f) max: (.most-positive #f))
  (check-argument (or (not .most-negative) (exact-integer? .most-negative))
                  "integer or false" .most-negative)
  (check-argument (or (not .most-positive) (exact-integer? .most-positive))
                  "integer or false" .most-positive)
  {(:: @ IntegerRange.) (.most-negative) (.most-positive)
   sexp: `(IntegerRange ,@(if .most-negative `(min: ,.most-negative) '())
                        ,@(if .most-positive `(max: ,.most-positive) '()))})

(def (unary-pre-op-check op check info x)
  (if (check x) (op x)
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x)))

(def (binary-pre-op-check op check info x y)
  (if (check x y) (op x y)
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x y)))

(def (unary-post-op-check op check info x)
  (def y (op x))
  (if (check y) y
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x)))

(def (binary-post-op-check op check info x y)
  (def z (op x y))
  (if (check z) z
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x y)))

;; Modular Arithmetics on Integer Ring Z/nZ
(define-type (Z/. @ [methods.marshal<-fixed-length-bytes UInt] n .validate)
  .element?: (cut uint-below? <> n)
  .length-in-bits: (integer-length .most-positive)
  .length-in-bytes: (n-bits->n-u8 .length-in-bits)
  .bytes<-: (cut uint->u8vector <> big .length-in-bytes)
  .<-bytes: (compose .validate u8vector->uint)
  .normalize: (λ (x) (modulo x n))
  .most-positive: (- n 1)
  .most-negative: 0
  .add-carry?: (λ (x y) (<= n (+ x y)))
  .add-negative-overflow?: false
  .sub-carry?: false
  .sub-negative-overflow?: (λ (x y) (< x y))
  .add: (λ (x y) (def z (+ x y)) (if (<= z .most-positive) z (- z n)))
  .sub: (λ (x y) (def z (- x y)) (if (negative? z) (+ z n) z))
  .neg: (λ (x) (if (zero? x) 0 (- n x)))
  .mul: (λ (x y) (.normalize (* x y)))
  .div: (lambda (x y) (.normalize (div-mod x y n)))
  .inv: (lambda (x) (.div 1 x))
  .intscale: .mul
  .succ: (λ (x) (if (= x .most-positive) 0 (1+ x)))
  .pred: (λ (x) (if (zero? x) .most-positive (1- x)))
  ;; function taking two entries a, b.
  ;; -- If a = b then returns 0;
  ;; -- If a > b then returns 1
  ;; -- If a < b then returns -1
  ;; (-- If the numbers are not comparable, returns #f)
  .comparer: number-comparer
  ;; TODO: range-checked arithmetics
  ;; add-valid? sum? mul-valid? product?
  .max: max
  .min: min)
(def (Z/ n) {(:: @ Z/.) (n) sexp: `(Z/ ,n)})



(define-type (UIntN. @ Z/. .length-in-bits .length-in-bytes .most-positive .validate)
  n: (arithmetic-shift 1 .length-in-bits)
  .element?: (cut uint-of-length? <> .length-in-bits)
  .bytes<-: (cut uint->u8vector <> big .length-in-bytes)
  .<-bytes: (compose .validate u8vector->uint)
  .normalize: (cut normalize-uint <> .length-in-bits)) ;; maybe faster? (λ (x) (bitwise-and x .most-positive))
(def UIntN<-length-in-bits (make-hash-table))
(def (UIntN .length-in-bits)
  (hash-ensure-ref UIntN<-length-in-bits .length-in-bits
                   (lambda () {(:: @ UIntN.) (.length-in-bits) sexp: `(UIntN ,.length-in-bits)})))
(define-type (UInt256 @ (UIntN 256)))

(define-type (IntN. @ Z/. .length-in-bits .length-in-bytes)
  n: (arithmetic-shift 1 .length-in-bits)
  .most-positive: (1- (arithmetic-shift 1 (1- .length-in-bits)))
  .most-negative: (- (arithmetic-shift 1 (1- .length-in-bits)))
  .element?: (cut sint-of-length? <> .length-in-bits)
  .bytes<-: (cut sint->u8vector <> big .length-in-bytes)
  .<-bytes: (cut u8vector->sint <> big .length-in-bytes)
  .normalize: (cut normalize-sint <> .length-in-bits)
  ;; Addition overflow occurs iff we add two operands of same sign and get result of opposite sign
  .add-overflow?: (λ (x y) (let (n (+ x y)) (not (= n (.normalize n))))) ;; TODO: simplify
  ;; Subtraction overflow occurs iff we subtract two operands of opposite sign and
  ;; get result of sign opposite the first operand, same as second operand.
  .sub-overflow?: (λ (x y) (let (n (- x y)) (not (= n (.normalize n))))) ;; TODO: simplify
  .succ: (λ (x) (if (= x .most-positive) .most-negative (1+ x)))
  .pred: (λ (x) (if (= x .most-negative) .most-positive (1- x))))

(def IntN<-length-in-bits (make-hash-table))
(def (IntN .length-in-bits)
  (hash-ensure-ref IntN<-length-in-bits .length-in-bits
                   (lambda () {(:: @ IntN.) (.length-in-bits) sexp: `(IntN ,.length-in-bits)})))
(define-type (JsInt @ (IntN 54))) ; From -2**53 to 2**53-1 included.

(def (bytes<-double d)
  (def bytes (make-u8vector 8))
  (u8vector-double-set! bytes 0 d big)
  bytes)

(def (double<-bytes bytes)
  (u8vector-double-ref bytes 0 big))

(define-type (Float @ [methods.marshal<-fixed-length-bytes Real] .validate)
  .element?: flonum?
  .length-in-bytes: 8
  .json<-: identity
  .<-json: .validate
  .bytes<-: bytes<-double
  .<-bytes: double<-bytes)

;; Compute a*x^e where multiplication is given by mul and e is a natural number,
;; by dichotomizing the exponent e.
;; Note: not safe vs side-channel leak for cryptographic use unless mul is, and n is specified,
;; and the suggested modification is made (and even then, be sure to use a constant-time if)
(def (mul-expt<-mul mul a x e (n (integer-length e)))
  (cond
   ((positive? n)
    (let loop ((i 0) (a a) (x x))
      (let* ((bit? (bit-set? i e))
             (j (1+ i))
             (aa (if bit? (mul a x) a))) ;; for constant-time, try (mul a (if bits? x 1))
        (if (> n j)
          (loop j aa (mul x x))
          aa))))
   ((zero? n)
    a)
   (else
    (error "exponent n must be a non-negative integer"))))

;; Compute a*x^e where multiplication is given by mul, inversion by inv, and e is a relative integer,
;; by dichotomizing the exponent e.
(def (mul-expt<-mul-inv mul inv a x e)
  (if (negative? e)
    (mul-expt<-mul mul a (inv x) (- e))
    (mul-expt<-mul mul a x e)))

;; natural integers as exponents
(define-type (expt<-mul. @ [Type.] .one .inv .mul)
  .mul-expt: (lambda (a x e) (mul-expt<-mul .mul a x e))
  .expt: (lambda (x e) (.mul-expt .one x e)))

;; relative integers as exponents
(define-type (expt<-mul-inv. @ [Type.] .one .inv .mul)
  .mul-expt: (lambda (a x e) (mul-expt<-mul-inv .mul .inv a x e))
  .expt: (lambda (x e) (.mul-expt .one x e)))
