;;; Interpreter project
;; Brian Clark
;; Danny Miles
;; Kaius Reed

#lang racket
(provide (all-defined-out))
(require "simpleParser.scm")

(define interpret
 (lambda (filename)
  (value (parser filename) (state.empty))))


;;; Value

(define value
 (lambda (stmt-list s)
   (cond
     ((null? stmt-list) '())
     ((list? (car stmt-list))
      (if (null? (value (car stmt-list) s))
          (value (cdr stmt-list) (state (car stmt-list) s))
          (value (car stmt-list) s)))
     (else
      (value.evaluate stmt-list s)))))
     
 ;Mathematical Operators
(define value.int
  (lambda (e s)
    (cond
      ((number? e) e)
      ((eq? '+ (operator e)) (+ (value.int(operand1 e s) s) (value.int(operand2 e s) s)))
      ((and (eq? '- (operator e)) (unary? e))
          (- 0 (value.int(operand1 e s) s)))
      ((eq? '- (operator e)) (- (value.int(operand1 e s) s) (value.int(operand2 e s) s)))
      ((eq? '* (operator e)) (* (value.int(operand1 e s) s) (value.int(operand2 e s) s)))
      ((eq? '/ (operator e)) (quotient (value.int(operand1 e s) s) (value.int(operand2 e s) s)))
      ((eq? '% (operator e)) (remainder (value.int(operand1 e s) s) (value.int(operand2 e s) s)))
      (else (error 'badop "Undefined operator")))))

;Comparison and Boolean Operations
(define value.bool
  (lambda (e s)
    (cond
      ((boolean? e) e)
      ((number? e) e)
      ((eq? 'true e) #t)
      ((eq? 'false e) #f)
      ((eq? '== (operator e)) (eq? (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '!= (operator e)) (not (eq? (value.bool(operand1 e s) s) (value.bool(operand2 e s) s))))
      ((eq? '> (operator e)) (> (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '< (operator e)) (< (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '>= (operator e)) (>= (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '<= (operator e)) (<= (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '&& (operator e)) (and (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '|| (operator e)) (or (value.bool(operand1 e s) s) (value.bool(operand2 e s) s)))
      ((eq? '! (operator e)) (not (value.bool(operand1 e s) s)))
      (else (error 'badop "Undefined operator")))))

(define value.evaluate
  (lambda (e s)
    (cond
      ((or  (eq? '+ (operator e)) (eq? '- (operator e)) (eq? '* (operator e)) (eq? '/ (operator e)) (eq? '% (operator e))) (value.int e s)
      ((or (eq? '&& (operator e)) (eq? '|| (operator e))) (value.bool e s)))
      (else '()))))


;abstractions for value
(define operator
  (lambda (e)
    (car e)))

;add lookup function here - if variable, lookup, else return atom
(define operand1
  (lambda (lis s)
    (cond
      ((list? (cadr lis)) (value (cadr lis) s))
      ((if (or (number? (cadr lis)) (eq? 'true (cadr lis)) (eq? 'false (cadr lis)))
           (cadr lis)
           (state.lookup (cadr lis) s))))))

;add lookup function here
(define operand2
  (lambda (lis s)
    (cond
      ((list? (caddr lis)) (value (caddr lis) s))
      ((if (or (number? (caddr lis)) (eq? 'true (caddr lis)) (eq? 'false (caddr lis)))
           (caddr lis)
           (state.lookup (caddr lis) s))))))
    

(define unary?
  (lambda (lis)
    (if (pair? (cddr lis))#f
        #t)))


;;; Bindings

(define state.empty (lambda () '(() ())))

(define state.add-binding
 (lambda (var value s)
   (if (null? (state.lookup var s))
       (list (cons var (car s)) (cons value (cadr s)))
       (state.add-binding var value (state.remove-binding var s)))))
     

(define state.remove-binding
 (lambda (var s)
   (cond
     ((null? s) '(() ()))
     ((eq? var (car (variables s))) (remaining s))
     (else (list
            (cons (car (variables s)) (car (state.remove-binding var (remaining s))))
            (cons (car (var-values s)) (cadr (state.remove-binding var (remaining s)))))))))
       

(define state.lookup
  (lambda (var s)
    (cond
      ((null? (variables s)) '())
      ((eq? var (car (variables s))) (car (var-values s)))
      (else (state.lookup var (remaining s))))))

(define variables
  (lambda (vartable) (car vartable)))

(define var-values
  (lambda (vartable) (cadr vartable)))

(define remaining
  (lambda (vartable)
    (list (cdr (car vartable)) (cdr (car (cdr vartable))))))

;;; State Mappings

(define state
 (lambda (stmt s)
  (cond

   ; null and return statements do not alter state
   ((null? stmt) s)
   ((eq? (keyword stmt) 'return) s)

   ; remaining operations delegated to helpers
   ((eq? (keyword stmt) '=) (state.assign stmt s))
   ((eq? (keyword stmt) 'if) (state.if stmt s))
   ((eq? (keyword stmt) 'var) (state.var stmt s))
   ((eq? (keyword stmt) 'while) (state.while stmt s))

   (else s)
  )
 )
)

(define keyword
 (lambda (stmt) (car stmt)))


;; Assignment

(define state.assign
 (lambda (stmt s)
  (state.add-binding
   (varname stmt)
   (value stmt s)
   (state.remove-binding (varname stmt) s))))

(define varname
 (lambda (stmt) (cadr stmt)))


;; If

(define state.if
 (lambda (stmt s)
  (if (value (condition stmt))
   (state (stmt1 stmt) s)
   (state (stmt2 stmt) s))))

(define condition (lambda (stmt) (cadr stmt)))
(define stmt1 (lambda (stmt) (caddr stmt)))
(define stmt2 (lambda (stmt) (cadddr stmt)))


;; Var

(define state.var
 (lambda (stmt s)
  (if (has-initialization stmt)
   (state.add-binding
    (varname stmt) (value (car (initialization stmt))) s)
   (state.add-binding
    (varname stmt) '() s))))

(define has-initialization
 (lambda (stmt) (not (null? (initialization stmt)))))

(define initialization
 (lambda (stmt) (cddr stmt)))


;; While

(define state.while
 (lambda (stmt s) s
  (if (value (condition stmt))
   (state stmt
    (state (loopbody stmt)
     (state (condition stmt)
      s)))
   (state (condition stmt) s))))

(define loopbody
 (lambda (stmt) (caddr stmt)))
