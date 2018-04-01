#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert (test-interpret "int2-files/1") 20)
(assert (test-interpret "int2-files/2") 164)
(assert (test-interpret "int2-files/3") 32)
(assert (test-interpret "int2-files/4") 2)
(assert-interpret-err "int2-files/5" 'illegal-var-dereferencing)
(assert (test-interpret "int2-files/6") 25)
(assert (test-interpret "int2-files/7") 21)
(assert (test-interpret "int2-files/8") 6)
(assert (test-interpret "int2-files/9") -1)
(assert (test-interpret "int2-files/10") 789)
(assert-interpret-err "int2-files/11" 'illegal-var-dereferencing)
(assert-interpret-err "int2-files/12" 'illegal-var-dereferencing)
(assert-interpret-err "int2-files/13" 'illegal-break)
(assert (test-interpret "int2-files/14") 12)
(assert (test-interpret "int2-files/15") 125)
(assert (test-interpret "int2-files/16") 110)
(assert (test-interpret "int2-files/17") 2000400)
(assert (test-interpret "int2-files/18") 101)
(assert-interpret-err "int2-files/19" 'illegal-throw)
(assert (test-interpret "int2-files/20") 21)
