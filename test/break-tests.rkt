    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; test cases

(define test-1-1
 (lambda ()
  (assert-state
   '((var x 1)
     (begin
      (= x 2)))
   '(((x) (2))))))

(define test-1-2
 (lambda ()
  (assert-state
   '((var x 1)
     (begin
      (= x 2)
      (= x 3)))
   '(((x) (3))))))

(define test-1-3
 (lambda ()
  (assert-state
   '((var x 10)
     (while (> x 0)
      (begin
       (= x (- x 1))
       (break))))
   '(((x) (9))))))

(define test-1-4
 (lambda ()
  (assert-state
   '((var x 10)
     (while (> x 0)
      (begin
       (= x (- x 1))
       (continue)
       (break))))
   '(((x) (0))))))

(define test-1-5
 (lambda ()
  (assert-state
   '((var x 10)
     (while (> x 0)
       (begin
        (= x (- x 1))
        (if (< x 6) (break) (continue)))))
   '(((x) (5))))))

(define test-1-6
 (lambda ()
  (assert-state-err
   '((var x 10)
     (break))
   'illegal-break)))
