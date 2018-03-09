
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require "../src/interpreter.rkt")


;;; utilities

(define print-results
 (lambda (success expected result)
  (if success
   (writeln success)
   (begin 
    (writeln success)
    (writeln expected)
    (writeln result)))))

(define assert
 (lambda (result expected)
   (print-results (equal? result expected)
                  expected result)))

(define unbox-all
  (lambda (s)
    (cond
      ((null? s) s)
      ((equal? s (state-empty)) s)
      ((equal? s (layer-empty)) s)
      ((is-state? s)
        (cons (unbox-all (car s)) (unbox-all (cdr s))))
      (else
        (list (cons (car (variables s)) (car (unbox-all (list (cdr (variables s)) (cdr (var-values s))))))
              (cons (unbox (car (var-values s))) (cadr (unbox-all (list (cdr (variables s)) (cdr (var-values s)))))))))))
                       

;;; Asserts

(define assert-interpret-err
  (lambda (file err)
    (with-handlers ([(lambda (msg) (equal? msg err))
                     (lambda (msg) #t)])
     (interpret-raise file))))

(define assert-state-err
 (lambda (stmt-tree err)
  (with-handlers ([(lambda (msg) (equal? msg err))
                   (lambda (msg) #t)])
   (assert-state stmt-tree err))))

(define assert-state
 (lambda (stmt-tree expected)
  (assert
   (unbox-all (call/cc (lambda (return)
    (state stmt-tree
           (state-empty)
           default-brk
           default-cont
           return
           default-throw))))
   expected)))
