    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; test cases

(assert-state
   '((var x 1)
     (begin
      (= x 2)))
   '(((x) (2))))

(assert-state
   '((var x 1)
     (begin
      (= x 2)
      (= x 3)))
   '(((x) (3))))

(assert-state
   '((var x 10)
     (while (> x 0)
      (begin
       (= x (- x 1))
       (break))))
   '(((x) (9))))

(assert-state
   '((var x 10)
     (while (> x 0)
      (begin
       (= x (- x 1))
       (continue)
       (break))))
   '(((x) (0))))

(assert-state
   '((var x 10)
     (while (> x 0)
       (begin
        (= x (- x 1))
        (if (< x 6) (break) (continue)))))
   '(((x) (5))))

(assert-state-err
   '((var x 10)
     (break))
   'illegal-break)
