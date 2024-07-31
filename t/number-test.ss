(export number-test)

(import
  :gerbil/gambit
  :std/assert :std/format
  :std/iter
  :std/misc/repr
  :std/sort
  :std/srfi/13
  :std/sugar :std/test
  :clan/assert :clan/base :clan/debug
  ../object ../mop ../number ../type ../brace)

(def number-test
  (test-suite "test suite for clan/poo/number"
    (test-case "simple tests for Z/nZ"
      (def Z/3Z (Z/ 3))
      (check
       (for/collect (i (iota 3))
         (for/collect (j (iota 3))
           (.call Z/3Z .add i j)))
       => [[0 1 2][1 2 0][2 0 1]])
      (check
       (for/collect (i (iota 3))
         (for/collect (j (iota 3))
           (.call Z/3Z .mul i j)))
       => [[0 0 0][0 1 2][0 2 1]])
      (check
       (for/collect (i (iota 3))
         (for/collect (j (iota 2 1))
           (.call Z/3Z .div i j)))
       => [[0 0][1 2][2 1]]))
    (test-case "test mul-expt<-mul"
      (check (mul-expt<-mul + 5 10 7) => 75)
      (check (mul-expt<-mul * 42 42 22) => (expt 42 23))
      (check (mul-expt<-mul * 42 42 22 8) => (expt 42 23)))))