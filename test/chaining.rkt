
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert (test-interpret-class "chaining/chaining-functions" 'A)
        6
        "chaining-functions")

(assert (test-interpret-class "chaining/chaining-fields" 'A)
        4
        "chaining-fields")

(assert (test-interpret-class "chaining/function-then-field" 'A)
        2
        "function-then-field")

(assert (test-interpret-class "chaining/deep-method-chaining" 'A)
        'true
        "deep-method-chaining")

(assert (test-interpret-class "chaining/builder" 'A)
        6
        "builder")
