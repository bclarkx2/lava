#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; helpers


;;; tests

(define test-1-1
 (lambda ()
  (assert (test-interpret "given-files/1") 150)))

(define test-1-2
 (lambda ()
  (assert (test-interpret "given-files/2") -4)))

(define test-1-3
 (lambda ()
  (assert (test-interpret "given-files/3") 10)))

(define test-1-4
 (lambda ()
  (assert (test-interpret "given-files/4") 16)))

(define test-1-5
 (lambda ()
  (assert (test-interpret "given-files/5") 220)))

(define test-1-6
 (lambda ()
  (assert (test-interpret "given-files/6") 5)))

(define test-1-7
 (lambda ()
  (assert (test-interpret "given-files/7") 6)))

(define test-1-8
 (lambda ()
  (assert (test-interpret "given-files/8") 10)))

(define test-1-9
 (lambda ()
  (assert (test-interpret "given-files/9") 5)))

(define test-1-10
 (lambda ()
  (assert (test-interpret "given-files/10") -39)))

(define test-1-11
 (lambda ()
  (assert-interpret-err
   "given-files/11"
   'assign-before-declare)))

(define test-1-12
 (lambda ()
  (assert-interpret-err
   "given-files/12"
   'illegal-var-dereferencing)))

(define test-1-13
 (lambda ()
  (assert-interpret-err
   "given-files/13"
   'illegal-var-dereferencing)))

(define test-1-14
 (lambda ()
  (assert-interpret-err
   "given-files/14"
   'illegal-var-use)))

(define test-1-15
 (lambda ()
  (assert (test-interpret "given-files/15") 'true)))

(define test-1-16
 (lambda ()
  (assert (test-interpret "given-files/16") 100)))

(define test-1-17
 (lambda ()
  (assert (test-interpret "given-files/17") 'false)))

(define test-1-18
 (lambda ()
  (assert (test-interpret "given-files/18") 'true)))

(define test-1-19
 (lambda ()
  (assert (test-interpret "given-files/19") 128)))

(define test-1-20
 (lambda ()
  (assert (test-interpret "given-files/20") 12)))

(define test-1-21
 (lambda ()
  (assert (test-interpret "given-files/21") 30)))

(define test-1-22
 (lambda ()
  (assert (test-interpret "given-files/22") 11)))

(define test-1-23
 (lambda ()
  (assert (test-interpret "given-files/23") 1106)))

(define test-1-24
 (lambda ()
  (assert (test-interpret "given-files/24") 12)))

(define test-1-25
 (lambda ()
  (assert (test-interpret "given-files/25") 16)))

(define test-1-26
 (lambda ()
  (assert (test-interpret "given-files/26") 72)))

(define test-1-27
 (lambda ()
  (assert (test-interpret "given-files/27") 21)))

(define test-1-28
 (lambda ()
  (assert (test-interpret "given-files/28") 164)))
