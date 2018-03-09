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
  (assert (interpret-raise "int2-files/1") 20)))

(define test-1-2
 (lambda ()
  (assert (interpret-raise "int2-files/2") 164)))

(define test-1-3
 (lambda ()
  (assert (interpret-raise "int2-files/3") 32)))

(define test-1-4
 (lambda ()
  (assert (interpret-raise "int2-files/4") 2)))

(define test-1-5
 (lambda () 
  (assert-interpret-err
    "int2-files/5"
    'illegal-var-dereferencing)))

(define test-1-6
 (lambda ()
  (assert (interpret-raise "int2-files/6") 25)))

(define test-1-7
 (lambda ()
  (assert (interpret-raise "int2-files/7") 21)))

(define test-1-8
 (lambda ()
  (assert (interpret-raise "int2-files/8") 6)))

(define test-1-9
 (lambda ()
  (assert (interpret-raise "int2-files/9") -1)))

(define test-1-10
 (lambda ()
  (assert (interpret-raise "int2-files/10") 789)))

(define test-1-11
 (lambda ()
  (assert-interpret-err
  "int2-files/11"
  'illegal-var-dereferencing)))

(define test-1-12
 (lambda ()
  (assert-interpret-err
  "int2-files/12"
  'illegal-var-dereferencing)))

(define test-1-13
 (lambda ()
  (assert-interpret-err
  "int2-files/13"
  'illegal-break)))

(define test-1-14
 (lambda ()
  (assert (interpret-raise "int2-files/14") 12)))

(define test-1-15
 (lambda ()
  (assert (interpret-raise "int2-files/15") 125)))

(define test-1-16
 (lambda ()
  (assert (interpret-raise "int2-files/16") 110)))

(define test-1-17
 (lambda ()
  (assert (interpret-raise "int2-files/17") 2000400)))

(define test-1-18
 (lambda ()
  (assert (interpret-raise "int2-files/18") 101)))

(define test-1-19
 (lambda ()
  (assert-interpret-err
	"int2-files/19"
	'illegal-throw)))

(define test-1-20
	(lambda ()
		(assert (interpret-raise "int2-files/20") 21)))
