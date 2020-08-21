(export #t)

(import
  :std/test
  :clan/t/test-support
  ../mop ../rationaldict
  ./table-testing)

(def T (RationalDict Any))

(def rationaldict-test
  (test-suite "test suite for clan/poo/rationaldict"
    (init-test-random-source!)
    (table-tests T)))
