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
  (assert (test-interpret "int3-files/1") 10)))

(define test-2-1
 (lambda ()
  (assert (test-interpret "int3-files/2") 14)))

(define test-3-1
 (lambda ()
  (assert (test-interpret "int3-files/3") 45)))

(define test-4-1
 (lambda ()
  (assert (test-interpret "int3-files/4") 55)))

(define test-5-1
 (lambda ()
  (assert (test-interpret "int3-files/5") 1)))

(define test-6-1
 (lambda ()
  (assert (test-interpret "int3-files/6") 115)))

(define test-7-1
 (lambda ()
  (assert (test-interpret "int3-files/7") 'true)))

(define test-8-1
 (lambda ()
  (assert (test-interpret "int3-files/8") 20)))

(define test-9-1
 (lambda ()
  (assert (test-interpret "int3-files/9") 24)))

(define test-10-1
 (lambda ()
  (assert (test-interpret "int3-files/10") 2)))

(define test-11-1
 (lambda ()
  (assert (test-interpret "int3-files/11") 35)))

(define test-12-1
 (lambda ()
  (assert-interpret-err
  "int3-files/12"
  'parameter-mismatch)))

(define test-13-1
 (lambda ()
  (assert (test-interpret "int3-files/13") 90)))


(define test-14-1
 (lambda ()
  (assert (test-interpret "int3-files/14") 69)))

(define test-15-1
 (lambda ()
  (assert (test-interpret "int3-files/15") 87)))

(define test-16-1
 (lambda ()
  (assert (test-interpret "int3-files/16") 64)))

(define test-17-1
 (lambda ()
  (assert-interpret-err
  "int3-files/17"
  'assign-before-declare)))

(define test-18-1
 (lambda ()
  (assert (test-interpret "int3-files/18") 125)))

(define test-19-1
 (lambda ()
  (assert (test-interpret "int3-files/19") 100)))


(define test-20-1
 (lambda ()
  (assert (test-interpret "int3-files/20") 2000400)))
