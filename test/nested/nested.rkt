
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../../src/interpreter.rkt")
(require "../common.rkt")

;;; tests

(assert (test-interpret-class "another-class-field-nested" "A")
        7
        "another-class-field-nested")

(assert (test-interpret-class "this-nested" "A")
        4
        "this-nested")

(assert (test-interpret-class "this-side-effect" "A")
        3
        "this-side-effect")

(assert (test-interpret-class "this-side-eff-no-ret" "A")
        8
        "this-side-eff-no-ret")