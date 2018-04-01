
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")
(require "common.rkt")

;;; test cases

(define test-1-1
 (lambda ()
  (assert
   (test-interpret-text
"var x = 14;
var y = 3 * x - 7;
function gcd(a,b) {
  if (a < b) {
    var temp = a;
    a = b;
    b = temp;
  }
  var r = a % b;
  while (r != 0) {
    a = b;
    b = r;
    r = a % b;
  }
  return b;
}
function main () {
  return gcd(x,y);
}")
    7)))

(define test-1-2
 (lambda ()
  (assert
   (test-interpret-text
"var a = 3;
function foo () {
  a = 4;
  return 2;
}
function main () {
  foo();
  return a;
}")
   4)))

(define test-1-3
 (lambda ()
  (assert-err-test-interpret-text
"var a = foo();
function foo () {
  return 2;
}
function main () {
  return a;
}" 'illegal-var-dereferencing)))

(define test-1-4
 (lambda ()
  (assert
   (test-interpret-text
"function foo () {
  try {
    throw 6;
  }
  finally {
    throw 10;
  }
}
function bar () {
  try {
    foo();
  }
  catch (e) {
    return e;
  }
}
function main () {
  return foo();
}")
   10)))
