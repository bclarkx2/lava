#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert (test-interpret-class "int4-files/1" "A") 15)
(assert (test-interpret-class "int4-files/2" "A") 12)
(assert (test-interpret-class "int4-files/3" "A") 125)
(assert (test-interpret-class "int4-files/4" "A") 36)
(assert (test-interpret-class "int4-files/5" "A") 54)
(assert (test-interpret-class "int4-files/6" "A") 110)
(assert (test-interpret-class "int4-files/7" "C") 26)
(assert (test-interpret-class "int4-files/8" "Square") 117)
(assert (test-interpret-class "int4-files/9" "Square") 32)
(assert (test-interpret-class "int4-files/10" "List") 15)
(assert (test-interpret-class "int4-files/11" "List") 123456)
(assert (test-interpret-class "int4-files/12" "List") 5285)
(assert (test-interpret-class "int4-files/13" "C") -716)
