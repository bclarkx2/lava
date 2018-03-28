#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; tests

(define test-1-1
 (lambda ()
  (assert (interpret-raise "int3-files/1") 10)))

(define test-1-2
 (lambda ()
  (assert (interpret-raise "int3-files/2") 14)))

(define test-1-3
 (lambda ()
  (assert (interpret-raise "int3-files/3") 45)))

(define test-1-4
 (lambda ()
  (assert (interpret-raise "int3-files/4") 55)))

(define test-1-5
 (lambda ()
  (assert (interpret-raise "int3-files/5") 1)))

(define test-1-6
 (lambda ()
  (assert (interpret-raise "int3-files/6") 115)))

(define test-1-7
 (lambda ()
  (assert (interpret-raise "int3-files/7") 'true)))

(define test-1-8
 (lambda ()
  (assert (interpret-raise "int3-files/8") 20)))

(define test-1-9
 (lambda ()
  (assert (interpret-raise "int3-files/9") 24)))

(define test-1-10
 (lambda ()
  (assert (interpret-raise "int3-files/10") 2)))

(define test-1-11
 (lambda ()
  (assert (interpret-raise "int3-files/11") 35)))

(define test-1-12
 (lambda ()
  (assert-interpret-err
  "int3-files/12"
  'mismatched-parameters)))

(define test-1-13
 (lambda ()
  (assert (interpret-raise "int3-files/13") 90)))


(define test-1-14
 (lambda ()
  (assert (interpret-raise "int3-files/14") 69)))

(define test-1-15
 (lambda ()
  (assert (interpret-raise "int3-files/15") 87)))

(define test-1-16
 (lambda ()
  (assert (interpret-raise "int3-files/16") 64)))

(define test-1-17
 (lambda ()
  (assert-interpret-err
  "int3-files/17"
  'SOME-ERROR)))

(define test-1-18
 (lambda ()
  (assert (interpret-raise "int3-files/18") 125)))

(define test-1-19
 (lambda ()
  (assert (interpret-raise "int3-files/19") 100)))


(define test-1-20
 (lambda ()
  (assert (interpret-raise "int3-files/20") 2000400)))
