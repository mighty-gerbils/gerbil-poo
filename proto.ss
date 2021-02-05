;;; Prototypes, in the most universal sense.
(export #t)

(import :clan/base)

;; If we could define types with constraints, it would be:
;; (deftype (Proto A B) (A ← A B) st: (<: A B))

;; instantiate-function-proto : A ← (Proto A B) B st: (<: A B Function))
(def (instantiate-proto p b) (def a (p (lambda i (apply a i)) b)) a)

;; identity-proto : (Proto A A)
(def (identity-proto _ a) a)

;; compose-proto : (Proto A C) ← (Proto A B) (Proto B C)
(def (compose-proto p q) (lambda (a b) (p a (q a b))))

;; compose-proto* : (Proto (A_ 0) (A_ (Card I))) ← (IndexedList I i: Proto (A_ i) (A_ (i+1)))
(def (compose-proto* l)
  (match l
    ([] identity-proto)
    ([p] p)
    ([p q] (compose-proto p q))
    ([p . r] (lambda (a b) (p a ((compose-proto* r) a b))))))

;; We can define a bottom / zero element as a universal base function
;; that is member of all function types:
#;(: proto-bottom (Fun O ... ← I ...))
(define (proto-bottom . x) (apply undefined x))

;; Then, we instantiate the combination of a bunch of prototypes in one go,
;; with proto-bottom at the bottom.
#;(: instance (Fun (A_ 0) ← (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))...))
(define (instantiate-protos . prototypes)
  (instantiate-proto (compose-proto* prototypes) proto-bottom))
