#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss

(import :gerbil/expander :clan/testing)
(init-test-environment!)
(import-module ':clan/poo/version #t #t) ;; import-module rather than import, to work even uncompiled
