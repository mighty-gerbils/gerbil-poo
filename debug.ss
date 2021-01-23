(export #t)
(import
  :gerbil/gambit/ports
  :std/format :std/sugar
  :clan/base :clan/debug
  ./poo ./mop ./io ./type ./brace)

;; A bit like DBG, but with types
(defrules DDT ()
  ((_ tag-expr)
   (DDT-helper tag-expr '() '() '() #f #f #f))
  ((_ tag-expr forms ... type expr)
   (let ((tagval tag-expr)
         (thunk (λ () expr)))
     (if tagval
       (DDT/internal tagval () () 'expr type thunk forms ...)
       (thunk)))))

(defrules DDT/internal ()
  ((DDT/internal tagval (dbg-type ...) (dbg-expr ...) expr type thunk)
   (DDT-helper tagval '(dbg-expr ...) (list dbg-type ...) (list (lambda () dbg-expr) ...) expr type thunk))
  ((DDT/internal tagval (dbg-type ...) (dbg-expr ...) expr type thunk typeN exprN more ...)
   (DDT/internal tagval (dbg-type ... typeN) (dbg-expr ... exprN) expr type thunk more ...)))

;; NB: fprintf uses the current-error-port and calls force-output
(def (DDT-helper tag dbg-exprs dbg-types dbg-thunks expr type thunk)
  (letrec
      ((f (λ (fmt . args)
            (force-output (current-output-port)) ;; avoid out-of-order issues due to stdout buffering
            (apply fprintf (current-error-port) fmt args)
            (force-output (current-error-port))))
       (v (λ (t x)
            (cond
             ((not t) (f " ~a~%" x))
             ((procedure? t)
              (f " ~a~%" (try (t x) (catch (_) (format "[CONVERSION ERROR] ~r" x)))))
             ((element? Type t)
              (if (element? t x)
                (f " ~s~%" (sexp<- t x))
                (f " [TYPE ERROR: not a ~s] ~r~%" (.@ t sexp) x)))
             (else
              (error "Invalid type or DDT printing specifier" t x)))))
       (x (λ (expr type thunk)
            (f "  ~s =>" expr)
            (call-with-values thunk (λ x (let (vx (apply values x)) (v type vx) vx))))))
    (if tag
      (begin
        (unless (void? tag) (f "~a~%" tag))
        (for-each x dbg-exprs dbg-types dbg-thunks)
        (if thunk (x expr type thunk) (void)))
      (if thunk (thunk) (void)))))

;; Method to inherit from another object and trace its procedure-valued methods.
;; Example usage: { foo: => (trace-inherited-method `(.@ ,(.@ @ sexp) foo)) }
(def (trace-inherited-slot name)
  (lambda (self super-prototypes slot-name base)
    (def inherited (compute-slot self super-prototypes slot-name base))
    (if (procedure? inherited) (traced-function `(.@ ,name ,slot-name) inherited) inherited)))

;; Create a variant of a poo that traces all its methods.
(def (trace-poo poo (name (.@ poo sexp)))
  (def wrapped {(:: @ poo)})
  (for-each (lambda (slot) (.putslot! wrapped slot (trace-inherited-slot name)))
            (.all-slots poo))
  wrapped)
