
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert (test-interpret-class "parent-class-method" "B")
        11
        "parent-class-method")

(assert (test-interpret-class "override" "B")
        10
        "override")

(assert (test-interpret-class "implicit-this" "B")
        100
        "implicit-this")

(assert (test-interpret-class "parent-class-field" "B")
        1
        "parent-class-field")

(assert (test-interpret-class "field-duplication" "B")
        3
        "field-duplication")

(assert (test-interpret-class "field-static-binding" "B")
        2
        "field-static-binding")
