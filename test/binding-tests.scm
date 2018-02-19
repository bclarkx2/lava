#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.scm")

;;; tests

(define test-1-1
 (lambda ()
  (equal? 4 (interpret "binding-files/reverse-declare-define-order"))))

