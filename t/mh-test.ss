;;; Experiments for multiple-inheritance, to be merged into POO at some point.

(export #t)

(import
  :std/srfi/1
  :clan/poo/poo :clan/poo/brace
  #|./poo ./brace|#)

;; (Listof (NonEmptyListof X)) <- (Listof X) (Listof (NonEmptyListof X))
(def (cons-if-not-empty l ls)
  (if (null? l) ls (cons l ls)))

;; (Listof (NonEmptyListof X)) <- (Listof (Listof X))
(def (remove-empties ls)
  (foldr cons-if-not-empty '() ls))

;; (Listof (NonEmptyListof X)) <- X (Listof (NonEmptyListof X))
(def (remove-next next tails)
  (foldr (lambda (l acc)
           (match l
             ([x] (if (equal? x next) acc [l . acc]))
             ([x . r] [(if (equal? x next) r l) . acc])))
         '() tails))

;; Given a get-supers function, compute the precedence list without any caching.
;; ((NonEmptyListof X) <- X) <- ((Listof X) <- X)
(def (get-precedence-list<-get-supers get-supers)
  (def (gpl x) (compute-precedence-list x get-supers: get-supers get-precedence-list: gpl))
  gpl)

;; Compute precedence list according to C3 linearization algorithm
;;   http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=35C3856CFE46B32E3047826297384694?doi=10.1.1.19.3910&rep=rep1&type=pdf
;; Note that the precedence list of x always starts with x and is therefore non-empty.
;; : (NonEmptyListof A) <- A \
;;     get-supers:((Listof A) <- A) \
;;     get-precedence-list:(A <- (NonEmptyListof A))
(def (compute-precedence-list
      x
      get-supers: get-supers
      get-precedence-list: (get-precedence-list (get-precedence-list<-get-supers get-supers)))
  (def supers (get-supers x))
  (def super-precedence-lists (map get-precedence-list supers))
  ;; : X <- (NonEmptyListof (NonEmptyListof X))
  (def (c3-select-next tails)
    ;; : Bool <- X
    (def (candidate? c) (every (lambda (tail) (not (member c (cdr tail)))) tails))
    (def select-loop (match <>
                       ([[c . _] . more] (if (candidate? c) c (select-loop more)))
                       ([] (error "Inconsistent precedence graph" x))))
    (select-loop tails))
  ;; : (NonEmptyListof X) <- (NonEmptyListof X) (Listof (NonEmptyListof X))
  (let loop ((rhead [x]) (tails (remove-empties [super-precedence-lists ... supers])))
    (match tails
      ([] (reverse rhead))
      ([x] (append (reverse rhead) x))
      (_ (let (next (c3-select-next tails))
           (loop [next . rhead] (remove-next next tails)))))))

(import
  :std/test)

(def mh-test
  (test-suite "test suite for clan/poo/mh"
    (test-case "simple tests of C3 linearization"
      (def c3-example
        {O: '()
         A: '(O)  B: '(O)  C: '(O)  D: '(O)  E: '(O)
         K1: '(A B C)  K2: '(D B E)  K3: '(D A)
         Z: '(K1 K2 K3)})
      (def (get-supers x) (.ref c3-example x))
      (def (cpl x) (compute-precedence-list x get-supers: get-supers))
      (check-equal? (cpl 'O) '(O))
      (check-equal? (cpl 'A) '(A O))
      (check-equal? (cpl 'K1) '(K1 A B C O))
      (check-equal? (cpl 'Z) '(Z K1 K2 K3 D A B C E O)))))

;;(run-tests! mh-test)
