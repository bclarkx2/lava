    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;; asserts

(define assert-funcall
 (lambda (formal body env stmt cur-state val)
  (begin 
   (set-formal-params! (lambda (e) formal))
   (set-func-body! (lambda (e) body))
   (set-func-env! (lambda (e s) env))
   (assert
    (value-func stmt cur-state)
   val))))  



;;; test cases

;; normal functions
(define test-1-1
 (lambda ()
    (begin 
     (set-formal-params! (lambda (e) '(a b)))
     (set-func-body! (lambda (e) '((return (+ a b)))))
     (set-func-env! (lambda (e s) (state-empty)))
     (assert
      (value-func '(funcall add 2 3) (state-empty))
      5))))

; niladic function
(define test-1-2
 (lambda ()
  (assert-funcall
   '()
   '((return 10))
   (state-empty)
   '(funcall nil_func)
   (state-empty)
   10)))

; return param with global var
(define test-1-3
 (lambda ()
  (assert-funcall
   '(y)
   '((return y))
   '(((x) (10)))
   '(funcall ret 4)
   '((() ()) ((x) (10)))
   4)))

; return param with global var and other
(define test-1-4
 (lambda ()
  (assert-funcall
   '(y)
   '((return y))
   '(((x) (10)))
   '(funcall ret 4)
   '(((y) (8)) ((x) (10)))
   4)))

; global var conflict
(define test-1-5
 (lambda ()
  (assert-funcall
   '(y)
   '((var x 100) (return (* x y)))
   '(((x) (10)))
   '(funcall mult 4)
   '((() ()) ((x) (10)))
   400)))

;;; (begin 
;;;    (set-formal-params! (lambda (e) '(y)))
;;;    (set-func-body! (lambda (e) '((var x 100) (return (* x y)))))
;;;    (set-func-env! (lambda (e s) '(((x) (10)))))
;;;    (value-func '(funcall mult 4) '((() ()) ((x) (10)))))
