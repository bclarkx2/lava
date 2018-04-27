
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

; instance fields
(assert (instance-field-names '())
        '())

(assert (instance-field-names '((var x)))
        '(x))

(assert (instance-field-names '((var y 10)))
        '(y))

(assert (instance-field-names '((var x) (var y)))
        '(x y))

(assert (instance-field-names '((var x) (function f ()) (var y)))
        '(x y))

; static functions
(assert (caar (static-functions '()))
        '())

(assert (caar (static-functions '((var x))))
        '())

(assert (caar (static-functions '((static-function f ()
                                    ((return 2)))
                                  (var x))))
        '(f)
        "single static")

(assert (caar (static-functions '((static-function f ()
                                    ((return 2)))
                                  (static-function g ()
                                    ((return 4)))
                                  (var x))))
        '(f g))

(assert (caar (static-functions '((static-function f ()
                                    ((return 2)))
                                  (var y)
                                  (static-function g ()
                                    ((return 4)))
                                  (var x)))) 
        '(f g))

(assert (caar (instance-functions '((function f ()
                                      ((return 2)))
                                    (var y)
                                    (function g ()
                                      ((return 4)))
                                    (var x))))
        '(f g))

(assert (state-class '(class A () ((var x)
                                    (static-function main ()
                                       ((return 2)))
                                    (function f ()
                                       ((return 2)))))
                     (state-empty)
                     default-brk
                     default-cont
                     default-return
                     default-throw)
        '())
