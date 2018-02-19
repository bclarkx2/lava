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


;;; General helpers

(define in?
 (lambda (a lis)
  (cond
   ((null? lis) #f)
   ((eq? a (car lis)) #t)
   (else (in? a (cdr lis))))))


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
      ((number? (car e)) (car e))
      ((and (eq? '- (operator e)) (unary? e))
          (- 0 (value.evaluate(operand1 e s) s)))
      ((eq? '+ (operator e)) (compute + e s))
      ((eq? '- (operator e)) (compute - e s))
      ((eq? '* (operator e)) (compute * e s))
      ((eq? '/ (operator e)) (compute quotient e s))
      ((eq? '% (operator e)) (compute remainder e s))
      (else (error 'badop "Undefined int operator")))))

;Comparison and Boolean Operations
(define value.bool
  (lambda (e s)
    (cond
      ((eq? '== (operator e)) (compute eq? e s))
      ((eq? '!= (operator e)) (compute (lambda (a b) (not (eq? a b))) e s))
      ((eq? '> (operator e)) (compute > e s))
      ((eq? '< (operator e)) (compute < e s))
      ((eq? '>= (operator e)) (compute >= e s))
      ((eq? '<= (operator e)) (compute <= e s))
      ((eq? '&& (operator e)) (compute (lambda (a b) (and a b)) e s))
      ((eq? '|| (operator e)) (compute (lambda (a b) (or a b)) e s))
      ((eq? '! (operator e)) (not (value.evaluate(operand1 e s) s)))
      (else (error 'badop "Undefined bool operator")))))

(define value.evaluate
  (lambda (e s)
    (if (list? e)
     (cond
      ((null? (cdr e)) (value.evaluate (car e) s))
      ((interpreter-keyword? (operator e)) (value.statement e s))
      ((int-operator? (operator e)) (value.int e s))
      ((bool-operator? (operator e)) (value.bool e s))
      (else (error 'badop "Undefined operator")))
     (cond
      ((number? e) e)
      ((boolean? e) e)
      ((eq? 'true e) #t)
      ((eq? 'false e) #f)
      (else (state.lookup e s))))))

(define value.statement
  (lambda (stmt s)
    (cond
      ((eq? 'return (keyword stmt)) (value.return stmt s))
      ((eq? 'if (keyword stmt)) (value.if stmt s))
      (else '()))))

;has a value if the chosen clause has a return statement
(define value.if
  (lambda (stmt s)
    (if (value.evaluate (condition stmt) s)
        (value (stmt1 stmt) s)
        (value (stmt2 stmt) s))))
    

(define value.return
 (lambda (e s)
  (cond 
   ((eq? (value.evaluate (cdr e) s) #t) 'true)
   ((eq? (value.evaluate (cdr e) s) #f) 'false)
   (else (value.evaluate (cdr e) s)))))


;abstractions for value
(define operator
  (lambda (e)
    (car e)))

(define int-operator?
 (lambda (op)
  (in? op '(+ - * / %))))

(define bool-operator?
 (lambda (op)
  (in? op '(== != > < >= <= && || !))))

;add lookup function here - if variable, lookup, else return atom
(define operand1
  (lambda (lis s)
    (cond
      ((list? (cadr lis)) (value (cadr lis) s))
      ((if (or (number? (cadr lis)) (eq? 'true (cadr lis)) (eq? 'false (cadr lis)))
           (cadr lis)
           (state.lookup (cadr lis) s))))))

(define operand2
  (lambda (lis s)
    (cond
      ((list? (caddr lis)) (value (caddr lis) s))
      ((if (or (number? (caddr lis)) (eq? 'true (caddr lis)) (eq? 'false (caddr lis)))
           (caddr lis)
           (state.lookup (caddr lis) s))))))
    
(define compute
 (lambda (op e s)
  (op (value.evaluate(operand1 e s) s)
      (value.evaluate(operand2 e s) s))))

(define unary?
  (lambda (lis)
    (if (pair? (cddr lis))#f
        #t)))

;if the car of the lis is a program-defined keyword, we must execute the command
(define interpreter-keyword?
  (lambda (atom)
   (in? atom '(var = if while return))))


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

   (else s))))

(define keyword
 (lambda (stmt) (car stmt)))


;; Assignment

(define state.assign
 (lambda (stmt s)
  (state.add-binding
   (varname stmt)
   (value.evaluate (varexpr stmt) s)  ;value of the expression
   (state.remove-binding (varname stmt) s))))

(define varname
 (lambda (stmt) (cadr stmt)))

(define varexpr
 (lambda (stmt) (caddr stmt)))
  


;; If

(define state.if
 (lambda (stmt s)
  (if (value.evaluate (condition stmt) s)
   (state (stmt1 stmt) s)
   (state (stmt2 stmt) s))))

(define condition (lambda (stmt) (cadr stmt)))
(define stmt1 (lambda (stmt) (caddr stmt)))
(define stmt2
 (lambda (stmt)
  (if (null? (cdddr stmt))
    '()
    (cadddr stmt))))


;; Var

(define state.var
 (lambda (stmt s)
  (if (has-initialization stmt)
   (state.add-binding
    (varname stmt) (value.evaluate (car (initialization stmt)) s) s)
   (state.add-binding
    (varname stmt) '() s))))

(define has-initialization
 (lambda (stmt) (not (null? (initialization stmt)))))

(define initialization
 (lambda (stmt) (cddr stmt)))


;; While

(define state.while
 (lambda (stmt s) s
  (if (value.evaluate (condition stmt) s)
   (state stmt
    (state (loopbody stmt)
     (state (condition stmt)
      s)))
   (state (condition stmt) s))))

(define loopbody
 (lambda (stmt) (caddr stmt)))
