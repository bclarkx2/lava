    
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
  (assert-state
   '((var x 10)
     (try 
      (begin
       (= x 20))
      ()
      ()))
   '((x) (20)))))

(define test-1-2
 (lambda ()
  (assert-state
   '((var x 10)
     (try 
      (= x 20)
      ()
      (finally (= x 30))))
   '((x) (30)))))

(define test-1-3
 (lambda ()
   (assert-state
    '((var x 10)
      (try 
       (begin
        (= x 20)
        (throw 3))
       (catch (e)
        (= x e))
       ()))
   '((x) (3)))))

(define test-1-4
 (lambda ()
   (assert-state
    '((var x 10)
      (try 
       (begin 
        (= x 20)
        (throw 30))
      (catch (e)
       (= x e))
      (finally
       (= x 40))))
    '((x) (40)))))

; return in try w/ finally
(define test-1-5
 (lambda ()
  (assert-state
   '((var x 10)
     (try
      (begin
       (= x 20
       (return x)))
     ()  
     (finally
      (= x 30))))
   '((x) (30)))))

; try-finally inside while loop; break inside try
(define test-2-1
 (lambda ()
  (assert-state
   '((var x 10)
     (while (> x 0)
     (begin
      (= x (- x 1))
      (try
       (break)
      ()  
      (finally
       (= x 99))))))
   '((x) (99)))))

; try-finally in while; break in catch
(define test-2-2
 (lambda ()
  (assert-state
   '((var x 10)
     (while (> x 0)
     (begin
      (= x (- x 1))
      (try
       (throw 20)
      (catch (e)
       (begin
        (= x e)
        (break)))
      (finally
       (= x 99))))))
   '((x) (99)))))

; throw in catch block
(define test-3-1
 (lambda ()
  (assert-state-err
   '((var x 10)
     (try
      (throw 20)
     (catch (e)
      (throw 30))
     (finally
      (= x 99))))
   'illegal-throw)))

; throw in finally block
(define test-3-2
 (lambda ()
  (assert-state-err
   '((var x 10)
     (try
      (= x 20)
     (catch (e)
      (= x 30))
     (finally
      (throw 40))))
   'illegal-throw)))
