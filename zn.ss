;; Modular Arithmetics on Integer Ring Z/nZ
;; Note: this is all quite inefficient as it is.
;; It would require some partial evaluation to become efficient.

(import
  :std/misc/number
  :clan/base
  ./object ./mop ./brace ./number ./type)

;; TODO: a better type hierarchy
;; TODO: clear contracts for + vs add (overflow check vs modular?), etc.
(export #t)

;; Operations assume numbers are in canonical normalized forms
(define-type (Z/nZ. @ [IntegerRange.])
  .n: undefined
  .normalize: (lambda (a) (modulo a .n))
  .add: (lambda (a b)
          (let (s (+ a b))
            (if (< s .n) s (- s .n))))
  .sub: (lambda (a b)
          (let (s (- a b))
            (if (negative? s) (+ s .n) s)))
  .mul: (lambda (a b) (.normalize (* a b)))
  .div: (lambda (a b) (.normalize (div-mod a b .n))))

(def (Z/nZ n)
  {(:: @ Z/nZ.) .n: n})
