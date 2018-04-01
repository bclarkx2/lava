#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(assert (test-interpret "binding-files/reverse-declare-define-order") 4)

(assert-interpret-err "binding-files/declare-before-use"
                      'illegal-var-dereferencing)

(assert-interpret-err "binding-files/redefine"
                      'illegal-var-use)
