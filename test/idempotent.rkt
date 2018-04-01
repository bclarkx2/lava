
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")

;;; test cases

(assert
 (test-interpret-text
"var a = 3;
function foo () {
  a = a + 1;
  return 2;
}
function main () {
  var b = foo();
  return a;
}")
 4)
