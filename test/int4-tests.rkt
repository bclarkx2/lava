#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert (test-interpret-class "int4-files/1" 'A)
        15
        "Test 1")

(assert (test-interpret-class "int4-files/2" 'A)
        12
        "Test 2")

(assert (test-interpret-class "int4-files/3" 'A)
        125
        "Test 3")

(assert (test-interpret-class "int4-files/4" 'A)
        36
        "Test 4")

(assert (test-interpret-class "int4-files/5" 'A)
        54
        "Test 5")

(assert (test-interpret-class "int4-files/6" 'A)
        110
        "Test 6")

;;; (assert 26 (test-interpret-class "int4-files/7" 'C) "Test 7")
;;; (assert 117 (test-interpret-class "int4-files/8" 'Square))
;;; (assert 32 (test-interpret-class "int4-files/9" 'Square))
;;; (assert 15 (test-interpret-class "int4-files/10" 'List))
;;; (assert 123456 (test-interpret-class "int4-files/11" 'List))
;;; (assert 5285 (test-interpret-class "int4-files/12" 'List))
;;; (assert -716 (test-interpret-class "int4-files/13" 'C))
