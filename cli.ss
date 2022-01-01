(export #t)

(import
  :gerbil/gambit/ports
  :std/generic :std/getopt :std/sugar
  :clan/cli :clan/hash :clan/list :clan/multicall :clan/path-config
  ./object ./brace)

(defmethod (getopt-spec (x object))
  (flatten-pair-tree
   (cond
    ((.has? x .type .getopt-spec) ((.@ x .type .getopt-spec) x))
    ((.has? x getopt-spec) (.@ x getopt-spec))
    (else (error "No getopt-spec" x)))))

(defmethod (call-with-processed-command-line (x object) (command-line <t>) (function <t>))
  (def process-opts
    (cond
     ((.has? x .type .process-opts) ((.@ x .type .process-opts) x))
     ((.has? x process-opts) (.@ x process-opts))
     (else (error "No getopt-spec" x))))
  (def gopt (apply getopt (getopt-spec x)))
  (def h (getopt-parse gopt command-line))
  (pair-tree-for-each! process-opts (cut <> h))
  (call-with-getopt-parse gopt h function))

(def options/base {getopt-spec: ? [] process-opts: ? []})

(def (make-options getopt-spec_ (process-opts_ []) (super options/base))
  {(:: @ super)
   getopt-spec: => (cut cons <> getopt-spec_)
   process-opts: => (cut cons <> process-opts_)})

(def options/backtrace (make-options getopt-spec/backtrace process-opts/backtrace))

(def options/path-config-root
  (make-options [(option 'path-config-root "--path-config-root"
                         help: "Directory under which to configure all runtime paths")]
                [(lambda (opt) (awhen (it (hash-removed opt 'path-config-root))
                            (set-path-config-root! it)))]))

(def options/help
  {(:: @ [options/base])
   getopt-spec: => (cut cons <> (flag 'help "-h" "--help" help: "Show help")) ;; or should it be -? or both?
   process-opts: => (cut cons <>
                         (lambda (opt) (when (hash-get opt 'help)
                                    (let (gopt (apply getopt (flatten-pair-tree getopt-spec)))
                                      (getopt-display-help gopt (current-program-string))
                                      (force-output)
                                      (exit 0)))))})
