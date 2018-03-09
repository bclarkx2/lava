#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(define test-1-1
 (lambda ()
  (assert 4 (interpret-raise "binding-files/reverse-declare-define-order"))))

(define test-1-2
 (lambda ()
  (assert-interpret-err
   "binding-files/declare-before-use"
   'illegal-var-dereferencing)))

(define test-1-3
 (lambda ()
  (assert-interpret-err 
   "binding-files/redefine"
   'illegal-var-use)))
