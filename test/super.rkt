
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert (test-interpret-class "super/basic-super" 'B)
        6
        "basic-super")

(assert (test-interpret-class "super/super-with-override" 'B)
        42
        "super-with-override")

(assert (test-interpret-class "super/super-field" 'B)
        3
        "super-field")
