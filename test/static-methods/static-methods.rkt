
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert (test-interpret-class "basic-static-method" "A")
        3
        "basic-static-method")

