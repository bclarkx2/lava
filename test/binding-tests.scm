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

(define test-1-2
 (lambda ()
  (with-handlers ([(lambda (msg) (equal? msg 'ILLEGAL-USE))
                   (lambda (msg) msg)])
    (interpret "binding-files/declare-before-use"))))
