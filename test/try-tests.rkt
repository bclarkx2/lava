    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert-state
 '((var x 10)
   (try 
    (begin
     (= x 20))
    ()
    ()))
 '(((x) (20))))

(assert-state
 '((var x 10)
   (try 
    (= x 20)
    ()
    (finally (= x 30))))
 '(((x) (30))))

 (assert-state
  '((var x 10)
    (try 
     (begin
      (= x 20)
      (throw 3))
     (catch (e)
      (= x e))
     ()))
 '(((x) (3))))

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
  '(((x) (40))))

; return in try w/ finally
(assert-state
 '((var x 10)
   (try
    (begin
     (= x 20
     (return x)))
   ()  
   (finally
    (= x 30))))
 '(((x) (30))))

; try-finally inside while loop; break inside try
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
 '(((x) (99))))

; try-finally in while; break in catch
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
 '(((x) (99))))

; throw in catch block
(assert-state-err
 '((var x 10)
   (try
    (throw 20)
   (catch (e)
    (throw 30))
   (finally
    (= x 99))))
 'illegal-throw)

; throw in finally block
(assert-state-err
 '((var x 10)
   (try
    (= x 20)
   (catch (e)
    (= x 30))
   (finally
    (throw 40))))
 'illegal-throw)
