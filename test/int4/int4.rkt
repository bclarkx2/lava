#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../../src/interpreter.rkt")
(require "../common.rkt")

;;; tests

(assert (test-interpret-class "1" "A")
        15
        "Test 1")

(assert (test-interpret-class "2" "A")
        12
        "Test 2")

(assert (test-interpret-class "3" "A")
        125
        "Test 3")

(assert (test-interpret-class "4" "A")
        36
        "Test 4")

(assert (test-interpret-class "5" "A")
        54
        "Test 5")

(assert (test-interpret-class "6" "A")
        110
        "Test 6")

(assert (test-interpret-class "7" "C")
        26
        "Test 7")

(assert (test-interpret-class "8" "Square")
        117
        "Test 8")

(assert (test-interpret-class "9" "Square")
        32
        "Test 9")

(assert (test-interpret-class "10" "List")
        15
        "Test 10")

(assert (test-interpret-class "11" "List")
        123456
        "Test 11")

(assert (test-interpret-class "12" "List")
        5285
        "Test 12")

(assert (test-interpret-class "13" "C")
        -716
        "Test 13")

