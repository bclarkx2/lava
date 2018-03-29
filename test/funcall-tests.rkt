    
;;; #lang racket
;;; (provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; test cases

;; normal functions
(define test-1-1
 (lambda ()
    (begin 
     (set-formal-params! (lambda (e) '(a b)))
     (set-func-body! (lambda (e) '(return (+ a b))))
     (set-func-env! (lambda (e s) (state-empty)))
     (assert
      (value-func '(funcall add 2 3) (state-empty))
      5))))
