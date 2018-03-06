    
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.scm")

;;; helpers

(define assert-err
 (lambda (file err)
  (with-handlers ([(lambda (msg) (equal? msg err))
                   (lambda (msg) #t)])
    (interpret file))))


;;; test cases

(define test-1-1
 (lambda ()
  (equal?
   (call/cc
    (lambda (return)
     (state '(begin (= x 2))
           '((x) (1))
           default-brk
           default-cont
           return)))
   '((x) (2)))))

(define test-1-2
 (lambda ()
  (equal?
   (call/cc
    (lambda (return)
     (state '(begin (= x 2) (= x 3))
           '((x) (1))
           default-brk
           default-cont
           return)))
   '((x) (3)))))

(define test-1-3
 (lambda ()
  (equal?
   (call/cc (lambda (return)
    (state '((var x 10)
             (while (> x 0)
              (begin
               (= x (- x 1))
               (break))))
           (state-empty)
           default-brk
           default-cont
           return))) 
   '((x) (9)))))

(define test-1-4
 (lambda ()
  (equal?
   (call/cc (lambda (return)
    (state '((var x 10)
             (while (> x 0)
              (begin
               (= x (- x 1))
               (continue)
               (break))))
           (state-empty)
           default-brk
           default-cont
           return)))
   '((x) (0)))))

(define test-1-5
 (lambda ()
  (equal?
   (call/cc (lambda (return)
    (state '((var x 10)
             (while (> x 0)
               (begin
                (= x (- x 1))
                (if (< x 6) (break) (continue)))))
           (state-empty)
           default-brk
           default-cont
           return)))
   '((x) (5)))))
