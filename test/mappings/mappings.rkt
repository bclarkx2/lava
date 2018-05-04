#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")


;; keyword

(check-equal? 'if (keyword '(if (== 1 2) (= x 3) (= x 4))))
(check-equal? 'while (keyword '(while (== 1 2) (= x (+ x 1)))))
(check-equal? '= (keyword '(= x 2)))
(check-equal? 'var (keyword '(var x)))
(check-equal? 'var (keyword '(var x (+ 1 2))))
(check-equal? 'return (keyword '(return x)))
