
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert (test-interpret-class "chaining-functions" "A")
        6
        "chaining-functions")

(assert (test-interpret-class "chaining-fields" "A")
        4
        "chaining-fields")

(assert (test-interpret-class "function-then-field" "A")
        2
        "function-then-field")

(assert (test-interpret-class "deep-method-chaining" "A")
        'true
        "deep-method-chaining")

(assert (test-interpret-class "builder" "A")
        6
        "builder")

(assert (test-interpret-class "throw-in-different-class" "A")
        3
        "throw-in-different-class")

(assert (test-interpret-class "side-effect" "A")
        1
        "side-effect")
