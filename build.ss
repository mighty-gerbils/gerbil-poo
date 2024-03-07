#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the build file for Gerbil-poo. Invoke it using
;;   ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile"), e.g.
;;   ./build.ss compile --O -t -g
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils

(import :std/make :clan/base :clan/building)

(def (spec)
  (!> (all-gerbil-modules)
      (cut apply add-build-options <> "io" (include-gambit-sharp))
      (cut cons "t/table-testing" <>)))

(init-build-environment!
 name: "Gerbil-poo"
 deps: '("clan")
 spec: spec)
