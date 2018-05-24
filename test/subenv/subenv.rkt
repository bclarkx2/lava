
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert '(((a) (b)) ((c) (d)) ((e) (f)))
        (subenvironment 3 '(((a) (b)) ((c) (d)) ((e) (f)))))

(assert '(((c) (d)) ((e) (f)))
        (subenvironment 2 '(((a) (b)) ((c) (d)) ((e) (f)))))

(assert '(((e) (f)))
        (subenvironment 1 '(((a) (b)) ((c) (d)) ((e) (f)))))
