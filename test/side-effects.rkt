
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert (test-interpret "given-files/21") 30 "21")
(assert (test-interpret "given-files/22") 11 "22")
(assert (test-interpret "given-files/23") 1106 "23")
(assert (test-interpret "given-files/24") 12 "24")
(assert (test-interpret "given-files/25") 16 "25")
(assert (test-interpret "given-files/26") 72 "26")
(assert (test-interpret "given-files/27") 21 "27")
(assert (test-interpret "given-files/28")  164 "28")
