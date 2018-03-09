#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")

;;; tests

;; keyword

(define test-1-1
 (lambda ()
  (equal? 'if (keyword '(if (== 1 2) (= x 3) (= x 4))))))

(define test-1-2
 (lambda ()
  (equal? 'while (keyword '(while (== 1 2) (= x (+ x 1)))))))

(define test-1-3
 (lambda ()
  (equal? '= (keyword '(= x 2)))))

(define test-1-4
 (lambda ()
  (equal? 'var (keyword '(var x)))))

(define test-1-5
 (lambda ()
  (equal? 'var (keyword '(var x (+ 1 2))))))

(define test-1-6
 (lambda ()
  (equal? 'return (keyword '(return x)))))
