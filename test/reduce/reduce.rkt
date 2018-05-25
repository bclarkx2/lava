
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

; line returns 2
; x -> 2
; count = 1
; total count = 1
(assert (test-interpret-class "immediate-break" "C")
        -102
        "immediate-break")

; line returns 1
; x -> 3
; count = 4
; total count = 5
(assert (test-interpret-class "2" "C")
        -503
        "2")

; line returns 5
; x -> 8
; count = 1
; total count = 6
(assert (test-interpret-class "3" "C")
        -608
        "3")

; line throws 2
; x -> 8
; count = 1
; total count = 7
(assert (test-interpret-class "4" "C")
        -716
        "4")
