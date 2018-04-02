#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; tests

(assert (test-interpret "given-files/1") 150 "1")
(assert (test-interpret "given-files/2") -4 "2")
(assert (test-interpret "given-files/3") 10 "3")
(assert (test-interpret "given-files/4") 16 "4")
(assert (test-interpret "given-files/5") 220 "5")
(assert (test-interpret "given-files/6") 5 "6")
(assert (test-interpret "given-files/7") 6 "7")
(assert (test-interpret "given-files/8") 10 "8")
(assert (test-interpret "given-files/9") 5 "9")
(assert (test-interpret "given-files/10") -39 "10")
(assert-interpret-err "given-files/11" 'assign-before-declare "11")
(assert-interpret-err "given-files/12" 'illegal-var-dereferencing "12")
(assert-interpret-err "given-files/13" 'illegal-var-dereferencing "13")
(assert-interpret-err "given-files/14" 'illegal-var-use "14")
(assert (test-interpret "given-files/15") 'true "15")
(assert (test-interpret "given-files/16") 100 "16")
(assert (test-interpret "given-files/17") 'false "17")
(assert (test-interpret "given-files/18") 'true "18")
(assert (test-interpret "given-files/19") 128 "19")
(assert (test-interpret "given-files/20") 12 "20")
