;;-*- Gerbil -*-
;;; Number Type descriptors for POO and its MOP

;; TODO: a better type hierarchy
;; TODO: clear contracts for + vs add (overflow check vs modular?), etc.
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact :gerbil/gambit/ports :scheme/base
  :std/iter :std/misc/bytes :std/misc/hash :std/srfi/1 :std/sugar
  :clan/base :clan/io :clan/number
  ./object ./mop ./brace ./io)

;; TODO: basic interface for arithmetics, with proper type signatures.
(.def (Number @ Type.)
  sexp: 'Number
  .element?: number?
  .sexp<-: identity
  .add: +
  .sub: -
  .mul: *
  .div: /
  .zero: 0
  .one: 1
  .<-string: (lambda (x) (validate @ (string->number x)))
  .string<-: number->string
  .=?: (cut = <> <>))

(.def (Real @ Number)
  sexp: 'Real
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

(.def (Integer @ [methods.bytes<-marshal Real]
                 .string<- .<-string)
  sexp: 'Integer
  .element?: exact-integer?
  .json<-: (lambda (x) (if (<= (integer-length x) 53) x (.string<- x)))
  .<-json: (lambda (x) (if (exact-integer? x) x (.<-string x)))
  .div: floor-quotient
  .mod: modulo
  .logand: bitwise-and
  .logor: bitwise-ior
  .logxor: bitwise-xor
  .lognot: bitwise-not ;; more bitwise operations, see Gambit
  .shift-left: arithmetic-shift
  .shift-right: (λ (x n) (arithmetic-shift x (- n)))
  .marshal: write-varint
  .unmarshal: read-varint
  ;; extract-bit-field test-bit-field? integer-length integer-length<? ...
  .succ: 1+
  .pred: 1-)

(.def (Nat @ Integer)
  sexp: 'Nat
  .marshal: write-varnat
  .unmarshal: read-varnat
  .sub: (lambda (x y) (if (>= x y) (- x y) (error "Overflow" - x y)))
  .pred: (lambda (x) (if (zero? x) (error "Overflow" .pred x) (1- x)))
  .non-negative?: true)

(.def (IntegerRange. @ Integer minimum maximum)
  sexp: `(IntegerRange ,@(if minimum `(min: ,minimum) '())
                       ,@(if max `(max: ,maximum) '()))
  .element?:
   (match (vector minimum maximum)
     ((vector #f #f) exact-integer?)
     ((vector _ #f) (λ (x) (and (exact-integer? x) (<= minimum x))))
     ((vector #f _) (λ (x) (and (exact-integer? x) (<= x maximum))))
     ((vector _ _) (λ (x) (and (exact-integer? x) (<= minimum x maximum))))))
(def (IntegerRange min: (minimum #f) max: (maximum #f))
  (assert! (or (not minimum) (exact-integer? minimum)))
  (assert! (or (not maximum) (exact-integer? maximum)))
  {(:: @ IntegerRange.) (minimum) (maximum)})

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

(.def (Z/. @ Integer n .validate)
  sexp: `(Z/ ,n)
  .element?: (nat-under? n)
  .length-in-bits: (integer-length .maxint)
  .length-in-bytes: (n-bytes<-n-bits .length-in-bits)
  .bytes<-: (cut bytes<-nat <> .length-in-bytes)
  .<-bytes: (compose .validate nat<-bytes)
  .marshal: (λ (n out) (write-integer-bytes n .length-in-bytes out))
  .unmarshal: (λ (in) (read-integer-bytes .length-in-bytes in))
  .normalize: (λ (x) (modulo x n))
  .maxint: (- n 1)
  .add-carry?: (λ (x y) (<= n (+ x y)))
  .add-negative-overflow?: false
  .sub-carry?: false
  .sub-negative-overflow?: (λ (x y) (< x y))
  .add: (λ (x y) (def z (+ x y)) (if (<= z .maxint) z (- z n)))
  .sub: (λ (x y) (def z (- x y)) (if (<= 0 z) z (+ z n)))
  .mul: (λ (x y) (.normalize (* x y)))
  .succ: (λ (x) (if (= x .maxint) 0 (1+ x)))
  .pred: (λ (x) (if (zero? x) .maxint (1- x)))
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

(.def (UInt. @ Z/. .length-in-bits .length-in-bytes .maxint)
  sexp: `(UInt ,.length-in-bits)
  n: (arithmetic-shift 1 .length-in-bits)
  .element?: (lambda (x) (and (nat? x) (<= (integer-length x) .length-in-bits)))
  .bytes<-: (cut bytes<-nat <> .length-in-bytes)
  .<-bytes: nat<-bytes
  .normalize: (λ (x) (bitwise-and x .maxint)))
(def UInt<-length-in-bits (make-hash-table))
(def (UInt .length-in-bits)
  (hash-ensure-ref UInt<-length-in-bits .length-in-bits (lambda () {(:: @ UInt.) (.length-in-bits)})))

(.def (JsInt @ [methods.marshal<-fixed-length-bytes Integer] .validate)
  sexp: 'JsInt
  .element?: (λ (x) (and (exact-integer? x) (<= .most-negative x .most-positive)))
  .most-positive: (1- (expt 2 53))
  .most-negative: (- (expt 2 53)) ;; make it an even SInt53
  .length-in-bytes: 7
  .json<-: identity
  .<-json: .validate
  .bytes<-: (λ (n) (def bytes (make-bytes 7))
               (u8vector-sint-set! bytes 0 n big 7)
               bytes)
  .<-bytes: (λ (bytes) (.validate (u8vector-sint-ref bytes 0 big 7))))

(def (bytes<-double d)
  (def bytes (make-bytes 8))
  (u8vector-double-set! bytes 0 d big)
  bytes)

(def (double<-bytes bytes)
  (u8vector-double-ref bytes 0 big))

(.def (Float @ [methods.marshal<-bytes Real] .validate)
  sexp: 'Float
  .element?: flonum?
  .length-in-bytes: 8
  .json<-: identity
  .<-json: .validate
  .bytes<-: bytes<-double
  .<-bytes: double<-bytes)
