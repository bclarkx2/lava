
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert (test-interpret-class "polymorphism/parent-class-method" 'B)
        11)

(assert (test-interpret-class "polymorphism/override" 'B)
        10)

(assert (test-interpret-class "polymorphism/implicit-this" 'B)
        100)
