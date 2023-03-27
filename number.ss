;;-*- Gerbil -*-
;;; Number Type descriptors for POO and its MOP

;; TODO: a better type hierarchy
;; TODO: clear contracts for + vs add (overflow check vs modular?), etc.
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact :gerbil/gambit/ports :scheme/base
  :std/assert :std/iter
  :std/misc/bytes :std/misc/hash
  :std/srfi/1
  :std/sugar
  :clan/base :clan/io :clan/number
  ./object ./mop ./brace ./io)

;; TODO: basic interface for arithmetics, with proper type signatures.
(define-type (Number @ Type. .validate)
  .element?: number?
  .sexp<-: identity
  .add: +
  .sub: -
  .mul: *
  .div: /
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
  .max: max
  .min: min)

(define-type (Integer @ [Real] .string<- .<-string)
  .element?: exact-integer?
  .json<-: (lambda (x) (if (<= (integer-length x) 53) x (.string<- x)))
  .<-json: (lambda (x) (if (exact-integer? x) x (.<-string x)))
  .marshal: write-varint
  .unmarshal: read-varint
  .bytes<-: bytes<-sint ;; note: encoding shorter (and thus different) from the marshalling
  .<-bytes: sint<-bytes
  .div: floor-quotient
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

(define-type (Nat @ Integer)
  .marshal: write-varnat
  .unmarshal: read-varnat
  .bytes<-: bytes<-nat ;; note: encoding shorter (and thus different) from the marshalling
  .<-bytes: nat<-bytes
  .sub: (lambda (x y) (if (>= x y) (- x y) (error "Overflow" - x y)))
  .pred: (lambda (x) (if (zero? x) (error "Overflow" .pred x) (1- x)))
  .non-negative?: true)

;; IntegerRange between .most-negative and .most-positive included
(.def (IntegerRange. @ Integer .most-negative .most-positive)
  sexp: `(IntegerRange ,@(if .most-negative `(min: ,.most-negative) '())
                       ,@(if .most-positive `(max: ,.most-positive) '()))
  .element?:
   (match (vector .most-negative .most-positive)
     ((vector #f #f) exact-integer?)
     ((vector _ #f) (λ (x) (and (exact-integer? x) (<= .most-negative x))))
     ((vector #f _) (λ (x) (and (exact-integer? x) (<= x .most-positive))))
     ((vector _ _) (λ (x) (and (exact-integer? x) (<= .most-negative x .most-positive))))))
(def (IntegerRange min: (.most-negative #f) max: (.most-positive #f))
  (assert! (or (not .most-negative) (exact-integer? .most-negative)))
  (assert! (or (not .most-positive) (exact-integer? .most-positive)))
  {(:: @ IntegerRange.) (.most-negative) (.most-positive)})

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

;; Interface for Z/nZ
(.def (Z/. @ [methods.marshal<-fixed-length-bytes Nat] n .validate)
  sexp: `(Z/ ,n)
  .element?: (nat-under? n)
  .length-in-bits: (integer-length .most-positive)
  .length-in-bytes: (n-bytes<-n-bits .length-in-bits)
  .bytes<-: (cut bytes<-nat <> .length-in-bytes)
  .<-bytes: (compose .validate nat<-bytes)
  .normalize: (λ (x) (modulo x n))
  .most-positive: (- n 1)
  .most-negative: 0
  .add-carry?: (λ (x y) (<= n (+ x y)))
  .add-negative-overflow?: false
  .sub-carry?: false
  .sub-negative-overflow?: (λ (x y) (< x y))
  .add: (λ (x y) (def z (+ x y)) (if (<= z .most-positive) z (- z n)))
  .sub: (λ (x y) (def z (- x y)) (if (<= 0 z) z (+ z n)))
  .mul: (λ (x y) (.normalize (* x y)))
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
(def (Z/ n) {(:: @ Z/.) (n)})

(.def (UInt. @ Z/. .length-in-bits .length-in-bytes .most-positive)
  sexp: `(UInt ,.length-in-bits)
  n: (arithmetic-shift 1 .length-in-bits)
  .element?: (lambda (x) (and (nat? x) (<= (integer-length x) .length-in-bits)))
  .bytes<-: (cut bytes<-nat <> .length-in-bytes)
  .<-bytes: nat<-bytes
  .normalize: (cut normalize-uint <> .length-in-bits)) ;; maybe faster? (λ (x) (bitwise-and x .most-positive))
(def UInt<-length-in-bits (make-hash-table))
(def (UInt .length-in-bits)
  (hash-ensure-ref UInt<-length-in-bits .length-in-bits (lambda () {(:: @ UInt.) (.length-in-bits)})))
(define-type (UInt256 @ (UInt 256)))

(.def (Int. @ Z/. .length-in-bits .length-in-bytes)
  sexp: `(Int ,.length-in-bits)
  n: (arithmetic-shift 1 .length-in-bits)
  .most-positive: (1- (arithmetic-shift 1 (1- .length-in-bits)))
  .most-negative: (- (arithmetic-shift 1 (1- .length-in-bits)))
  .element?: (lambda (x) (and (exact-integer? x) (< (integer-length x) .length-in-bits)))
  .bytes<-: (cut bytes<-sint <> .length-in-bytes)
  .<-bytes: (cut sint<-bytes <> .length-in-bytes)
  .normalize: (cut normalize-sint <> .length-in-bits)
  ;; Addition overflow occurs iff we add two operands of same sign and get result of opposite sign
  .add-overflow?: (λ (x y) (let (n (+ x y)) (not (= n (.normalize n))))) ;; TODO: simplify
  ;; Subtraction overflow occurs iff we subtract two operands of opposite sign and
  ;; get result of sign opposite the first operand, same as second operand.
  .sub-overflow?: (λ (x y) (let (n (- x y)) (not (= n (.normalize n))))) ;; TODO: simplify
  .add: (λ (x y) (.normalize (+ x y)))
  .sub: (λ (x y) (.normalize (- x y)))
  .mul: (λ (x y) (.normalize (* x y)))
  .succ: (λ (x) (if (= x .most-positive) .most-negative (1+ x)))
  .pred: (λ (x) (if (= x .most-negative) .most-positive (1- x))))

(def Int<-length-in-bits (make-hash-table))
(def (Int .length-in-bits)
  (hash-ensure-ref Int<-length-in-bits .length-in-bits (lambda () {(:: @ Int.) (.length-in-bits)})))
(define-type (JsInt @ (Int 54))) ; From -2**53 to 2**53-1 included.

(def (bytes<-double d)
  (def bytes (make-bytes 8))
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
