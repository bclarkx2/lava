#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert (test-interpret "int3-files/1") 10)
(assert (test-interpret "int3-files/2") 14)
(assert (test-interpret "int3-files/3") 45)
(assert (test-interpret "int3-files/4") 55)
(assert (test-interpret "int3-files/5") 1)
(assert (test-interpret "int3-files/6") 115)
(assert (test-interpret "int3-files/7") 'true)
(assert (test-interpret "int3-files/8") 20)
(assert (test-interpret "int3-files/9") 24)
(assert (test-interpret "int3-files/10") 2)
(assert (test-interpret "int3-files/11") 35)
(assert-interpret-err "int3-files/12" 'parameter-mismatch)
(assert (test-interpret "int3-files/13") 90)
(assert (test-interpret "int3-files/14") 69 "collatz sequence")
(assert (test-interpret "int3-files/15") 87)
(assert (test-interpret "int3-files/16") 64)
(assert-interpret-err "int3-files/17" 'assign-before-declare)
(assert (test-interpret "int3-files/18") 125)
(assert (test-interpret "int3-files/19") 100)
(assert (test-interpret "int3-files/20") 2000400)
