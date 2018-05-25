
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert (test-interpret-class "catch-continue" "A")
        4
        "catch-continue")

(assert (test-interpret-class "try-return" "A")
        10
        "try-return")

(assert (test-interpret-class "try-continue" "A")
        4
        "try-continue")

(assert (test-interpret-class "double-try-in-catch" "A")
        11100
        "double-try-in-catch")
