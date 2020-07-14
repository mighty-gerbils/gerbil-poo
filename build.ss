#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the build file for Gerbil-poo. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils

(import
  :std/build-script :std/srfi/1
  :clan/filesystem :clan/path :clan/versioning)

(def here (path-parent (this-source-file)))
(current-directory here)

(def (build-spec)
  ((cut lset-difference equal? <> '("build.ss" "unit-tests.ss"))
   (filter (cut path-extension-is? <> ".ss") (directory-files "."))))

(def (main . args)
  (when (match args ([] #t) (["compile" . _] #t) (_ #f))
    (update-version-from-git name: "Gerbil-poo" deps: '("clan")))
  (defbuild-script ;; defines an inner "main"
    (build-spec)
    ;;verbose: 9
    )
  (apply main args))
