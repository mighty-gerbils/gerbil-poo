(export #t)

(import :std/getopt :clan/cli ./object ./brace)

(def options/base {(getopt-spec ? []) (process-opts ? [])})

(def (make-options getopt-spec_ process-opts_ (super options/base))
  {(:: @ super)
   getopt-spec: => (cut append (reverse getopt-spec_) <>)
   process-opts: => (cut append (reverse process-opts_) <>)})

(def options/backtrace (make-options getopt-spec/backtrace process-opts/backtrace))

(def (process-options options arguments)
  (def gopt (apply getopt (.@ options getopt-spec)))
  (def opt (getopt-parse gopt arguments))
  (for-each (cut <> opt) (reverse (.@ options process-opts)))
  opt)
