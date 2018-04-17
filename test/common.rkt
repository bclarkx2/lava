
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require racket/pretty)
(require "../src/interpreter.rkt")
(require "../src/functionParser.rkt")
(require rackunit)

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
 (lambda (result expected [msg ""])
  (check-equal? result expected msg)))

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

(define test-interpret-class
 (lambda (filename classname)
  (with-handlers ([(lambda (msg) 'no-problem)
                    (lambda (msg) msg)])
    (interpret-raise filename classname))))
                       
(define test-interpret
 (lambda (filename)
   (with-handlers ([(lambda (msg) 'no-problem)
                    (lambda (msg) msg)])
    (interpret-raise filename))))

(define test-interpret-text
 (lambda (text)
  (begin0
   (test-interpret
    (let ([out (open-output-file ".data" #:exists 'replace)])
     (begin (display text out)
            (close-output-port out)
            ".data")))
   (delete-file ".data"))))

(define assert-err-test-interpret-text
 (lambda (text err)
  (assert err
          (test-interpret-text text))))
  
(define calc-state
 (lambda (stmt-tree)
  (with-handlers ([(lambda (msg) 'no-problem)
                   (lambda (msg) msg)])
   (unbox-all (call/cc (lambda (return)
     (state stmt-tree
            (state-empty)
            default-brk
            default-cont
            return
            default-throw)))))))

;;; Asserts

(define assert-interpret-err
  (lambda (file err [msg ""])
    (assert err (test-interpret file) msg)))

(define assert-state-err
 (lambda (stmt-tree err)
   (assert (calc-state stmt-tree) err)))

(define assert-state
 (lambda (stmt-tree expected)
  (assert
   (calc-state stmt-tree)
   expected)))
