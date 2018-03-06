    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.scm")
(require "common.scm")


;;; test cases

(define test-1-1
 (lambda ()
  (assert-state
   '((var x 10)
     (try 
      (begin
       (= x 20))
      ()
      ()))
   '((x) (20)))))

(define test-1-2
 (lambda ()
  (assert-state
   '((var x 10)
     (try 
      (= x 20)
      ()
      (finally (= x 30))))
   '((x) (30)))))

(define test-1-3
 (lambda ()
   (assert-state
    '((var x 10)
      (try 
       (begin
        (= x 20)
        (throw 3))
       (catch (e)
        (= x e))
       ()))
   '((x) (3)))))

(define test-1-4
 (lambda ()
   (assert-state
    '((var x 10)
      (try 
       (begin
        (= x 20)
        (throw 30))
      (catch (e)
       (= x e))
      (finally
       (= x 40))))
    '((x) (40)))))
