
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require racket/pretty)
(require "../src/interpreter.rkt")


;;; utilities

(define print-results
 (lambda (success expected result)
  (if success
   (pretty-print success)
   (begin 
    (pretty-print success)
    (pretty-print expected)
    (pretty-print result)))))

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
        (list (cons (car (layer-variables s)) (car (unbox-all (list (cdr (layer-variables s)) (cdr (layer-values s))))))
              (cons (unbox (car (layer-values s))) (cadr (unbox-all (list (cdr (layer-variables s)) (cdr (layer-values s)))))))))))
                       

(define test-interpret
 (lambda (filename)
   (with-handlers ([(lambda (msg) 'no-problem)
                    (lambda (msg) msg)])
    (interpret-raise filename))))

;;; Asserts

(define assert-interpret-err
  (lambda (file err)
    (with-handlers ([(lambda (msg) (equal? msg err))
                     (lambda (msg) #t)])
     (test-interpret file))))

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
