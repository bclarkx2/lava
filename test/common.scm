
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.scm")

;;; utilities
(define assert-err
 (lambda (file err)
  (with-handlers ([(lambda (msg) (equal? msg err))
                   (lambda (msg) #t)])
    (interpret file))))

(define print-results
 (lambda (success expected result)
  (if success
   (writeln success)
   (begin 
    (writeln success)
    (writeln expected)
    (writeln result)))))
  
(define assert
 (lambda (result expected)
   (print-results (equal? result expected)
                  expected result)))
