(export #t)

(import
  :std/sugar :std/test
  :clan/base :clan/testing
  ../object ../mop ../number ../type ../trie
  ./table-testing)

(def (simple-tests T)
  (table-test-case T "manual tests ported from legilogic_lib"
    (test-case "Simple constructors"
      (check-equal? (E .empty) (Empty))
      (check-equal? (F .singleton 0 "a") (Leaf "a"))
      (check-equal? (F .singleton 1 "b") (Skip 0 0 1 (Leaf "b")))
      (check-equal? (F .<-list '((0 . "a") (1 . "b"))) (Branch 0 (Leaf "a") (Leaf "b")))
      (check-equal? (F .<-list '((0 . "a") (1 . "b") (2 . "c") (3 . "d")))
                    (Branch 1 (Branch 0 (Leaf "a") (Leaf "b"))
                              (Branch 0 (Leaf "c") (Leaf "d"))))
      (check-equal? (F .<-list '((0 . "a") (1 . "b") (2 . "c")))
                    (Branch 1 (Branch 0 (Leaf "a") (Leaf "b"))
                            (Skip 0 0 0 (Leaf "c"))))
      (check-equal? (F .<-list '((0 . "a") (1 . "b") (3 . "d")))
                    (Branch 1 (Branch 0 (Leaf "a") (Leaf "b"))
                            (Skip 0 0 1 (Leaf "d"))))
      (check-equal? (F .<-list '((0 . "a") (3 . "d")))
                    (Branch 1 (Skip 0 0 0 (Leaf "a"))
                              (Skip 0 0 1 (Leaf "d"))))
      (check-equal? (F .<-list '((1 . "b") (3 . "d")))
                    (Branch 1 (Skip 0 0 1 (Leaf "b"))
                              (Skip 0 0 1 (Leaf "d")))))
    (test-case "zipping"
      (defrule (check-leaf-focus k v t)
        (begin
          (F .validate t)
          (let-match ((cons focus path) (F .refocus ($Costep -1 k) (F .zipper<- t)))
            (check-equal? focus (F .leaf<-opt (F .ref/opt t k)))
            (check-equal? (F .ref t k false) v)
            (validate (E Path) path)
            (check-equal? ($Path-costep path) ($Costep -1 k))
            (check-equal? (F .<-zipper (cons focus path)) t))))
      (check-leaf-focus 100 "one hundred" (F .singleton 100 "one hundred"))
      (check-leaf-focus 101 "needle" (F .<-list '((100 . "hey") (101 . "needle")
                                                  (102 . "hay") (103 . "haAAy"))))
      (check-leaf-focus 0 #f (E .empty))
      (check-leaf-focus 1 #f (E .empty))
      (check-leaf-focus 3 "c" (F .<-list '((1 . "a") (3 . "c"))))
      (def t0 (F .<-list '((0 . "a") (1 . "b"))))
      (def t1 (F .singleton 0 "veni, vidi"))
      (def t2 (F .acons 2 "veni, vidi" t0))
      (check-leaf-focus 2 #f t0)
      (check-equal? (F .acons 2 "veni, vidi" t0) t2)
      (check-equal? (F .update 2 (lambda _ "veni, vidi") t0) t2))))

(def T (SimpleTrie UInt String))

;;; The G functions make the eval work, which ensures the traces have usable function names.
(def (G x) (hash-get (object-instance T) x))
(def (G-set! x y) (hash-put! (object-instance T) x y))
(def (traceT)
  (for-each (lambda (k) (def v (.ref T k)) (when (procedure? v) (eval `(trace! (G ',k)))))
            (.all-slots T)))
;;(traceT)

(def-table-test-accessors T)

(def trie-test
  (test-suite "test suite for clan/poo/trie"
    (init-test-random-source!)
    (table-tests T)
    (simple-tests T)))
