#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert 15 (test-interpret-class "int4-files/1" 'A) "Test 1")
(assert 12 (test-interpret-class "int4-files/2" 'A) "Test 2")
(assert 125 (test-interpret-class "int4-files/3" 'A) "Test 3")
(assert 36 (test-interpret-class "int4-files/4" 'A) "Test 4")
(assert 54 (test-interpret-class "int4-files/5" 'A) "Test 5")
(assert 110 (test-interpret-class "int4-files/6" 'A) "Test 6")
(assert 26 (test-interpret-class "int4-files/7" 'C) "Test 7")
;;; (assert 117 (test-interpret-class "int4-files/8" 'Square))
;;; (assert 32 (test-interpret-class "int4-files/9" 'Square))
;;; (assert 15 (test-interpret-class "int4-files/10" 'List))
;;; (assert 123456 (test-interpret-class "int4-files/11" 'List))
;;; (assert 5285 (test-interpret-class "int4-files/12" 'List))
;;; (assert -716 (test-interpret-class "int4-files/13" 'C))
