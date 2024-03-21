(export #t)

(import
  (only-in :std/generic defmethod)
  (only-in :std/cli/getopt getopt getopt-parse getopt-display-help flag option
           ->getopt-spec call-with-processed-command-line call-with-getopt-parse)
  (only-in :std/cli/multicall current-program-string)
  (only-in :std/misc/list flatten)
  (only-in :std/sugar awhen)
  (only-in :clan/cli getopt-spec/backtrace process-opts/backtrace)
  (only-in :clan/hash hash-removed)
  (only-in :clan/list pair-tree-for-each!)
  (only-in :clan/path-config set-path-config-root!)
  (only-in ./object .has? .@ object)
  (only-in ./brace @method))

(defmethod (->getopt-spec (x object))
  (cond
   ((.has? x .type .getopt-spec) (->getopt-spec ((.@ x .type .getopt-spec) x)))
   ((.has? x getopt-spec) (->getopt-spec (.@ x getopt-spec)))
   (else (error "No getopt-spec" x))))

(defmethod (call-with-processed-command-line (x object) (command-line :t) (function :t))
  (def process-opts
    (cond
     ((.has? x .type .process-opts) ((.@ x .type .process-opts) x))
     ((.has? x process-opts) (.@ x process-opts))
     (else (error "No getopt-spec" x))))
  (def gopt (apply getopt (->getopt-spec x)))
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
                                    (let (gopt (apply getopt (flatten getopt-spec)))
                                      (getopt-display-help gopt (current-program-string))
                                      (force-output)
                                      (exit 0)))))})
