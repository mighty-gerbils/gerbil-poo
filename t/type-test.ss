(export
  type-test)

(import
  :gerbil/gambit/ports
  :std/format :std/sort :std/srfi/13 :std/sugar :std/test
  :clan/assert :clan/base
  ../poo ../mop ../number ../type)

(defrule (check-rep parse unparse rep obj)
  (begin ;;let ((rep rep) (obj obj))
    (check-equal? (parse rep) obj)
    (check-equal? (unparse obj) rep)))

(def type-test
  (test-suite "test suite for clan/poo/type"
    (test-case "simple tests"
      (def MyRange (IntegerRange min: 100 max: 200))
      (map (λ-match ([type element] (assert! (element? type element))))
           [[MyRange 123]
            [MyRange 100]
            [MyRange 200]])
      (map (λ-match ([type element] (assert! (not (element? type element)))))
           [[MyRange 99]
            [MyRange 201]]))
    (test-case "tuple test"
      (def UInt8 (UInt 8))
      (def t (Tuple UInt8 UInt8 UInt8))
      (check-rep (.@ t .<-json) (.@ t .json<-) [5 8 13] #(5 8 13))
      (check-rep (.@ t .<-bytes) (.@ t .bytes<-) #u8(#x15 #x22 #x37) #(21 34 55)))
    (test-case "function tests"
      (def (f x y) (values 1 x y))
      (check-equal? (values->list ((validate (Fun Number String Symbol <- String Symbol) f) "a" 'b))
                    '(1 "a" b))
      (check-exception ((validate (Fun Any <- String) f) 2 3) true)
      (check-exception ((validate (Fun Any <- Any Any) f) 2 3) true)
      (check-exception ((validate (Fun String Number Number <- Any Any) f) 2 3) true))))
