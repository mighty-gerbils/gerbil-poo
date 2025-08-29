;;-*- Gerbil -*-
;;; Brace syntax for POO

(export @method @@method)

(import
  (prefix-in (only-in :gerbil/core @method) @)
  (only-in ./object .o .o/ctx))

;; {args ...} -> (@method args ...) -> (.o args ...)
;; except that for macro-scope it's -> (.o/ctx #,stx args ...)
(defsyntax-for-match @method
  (lambda (stx)
    (syntax-case stx () ; match pattern
      ((_ args ...) #'(.o args ...))))
  (lambda (stx)
    (syntax-case stx () ; expr
      ((_ args ...)
       (with-syntax ((ctx stx)) #'(.o/ctx ctx args ...))))))
