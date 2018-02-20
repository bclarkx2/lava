#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.scm")

(define assert-err
 (lambda (file err)
  (with-handlers ([(lambda (msg) (equal? msg err))
                   (lambda (msg) #t)])
    (interpret file))))


;;; tests

(define test-1-1
 (lambda ()
  (equal? 4 (interpret "binding-files/reverse-declare-define-order"))))

(define test-1-2
 (lambda ()
  (assert-err
   "binding-files/declare-before-use"
   'illegal-var-dereferencing)))
  
