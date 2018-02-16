;;; Interpreter project
;; Brian Clark
;; Danny Miles
;; Kaius Reed

#lang racket
(provide (all-defined-out))
(require "simpleParser.scm")

(define interpret
 (lambda (filename)
  'hey
 )
)



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
      (value.evaluate stmt-list)))))
     
 ;Mathematical Operators
(define value.int
  (lambda (e)
    (cond
      ((number? e) e)
      ((eq? '+ (operator e)) (+ (value.int(operand1 e)) (value.int(operand2 e))))
      ((and (eq? '- (operator e)) (unary? e))
          (- 0 (value.int(operand1 e))))
      ((eq? '- (operator e)) (- (value.int(operand1 e)) (value.int(operand2 e))))
      ((eq? '* (operator e)) (* (value.int(operand1 e)) (value.int(operand2 e))))
      ((eq? '/ (operator e)) (quotient (value.int(operand1 e)) (value.int(operand2 e))))
      ((eq? '% (operator e)) (remainder (value.int(operand1 e)) (value.int(operand2 e))))
      (else (error 'badop "Undefined operator")))))

;Comparison and Boolean Operations
(define value.bool
  (lambda (e)
    (cond
      ((boolean? e) e)
      ((number? e) e)
      ((eq? 'true e) #t)
      ((eq? 'false e) #f)
      ((eq? '== (operator e)) (eq? (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '!= (operator e)) (not (eq? (value.bool(operand1 e)) (value.bool(operand2 e)))))
      ((eq? '> (operator e)) (> (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '< (operator e)) (< (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '>= (operator e)) (>= (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '<= (operator e)) (<= (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '&& (operator e)) (and (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '|| (operator e)) (or (value.bool(operand1 e)) (value.bool(operand2 e))))
      ((eq? '! (operator e)) (not (value.bool(operand1 e))))
      (else (error 'badop "Undefined operator")))))

(define value.evaluate
  (lambda (e)
    (if (or  (eq? '+ (operator e)) (eq? '- (operator e)) (eq? '* (operator e)) (eq? '/ (operator e)) (eq? '% (operator e)))
        (value.int e)
        (value.bool e))))


;abstractions for value
(define operator
  (lambda (e)
    (car e)))

;add lookup function here - if variable, lookup, else return atom
(define operand1
  (lambda (lis)
    (if (or (number? (cadr lis)) (eq? 'true (cadr lis)) (eq? 'false (cadr lis)))
        (cadr lis)
    (state.lookup (cadr lis)))))

;add lookup function here
(define operand2
  (lambda (lis)
    (if (or (number? (caddr lis)) (eq? 'true (caddr lis)) (eq? 'false (caddr lis)))
        (caddr lis)
    (state.lookup (caddr lis)))))
    

(define unary?
  (lambda (lis)
    (if (pair? (cddr lis))#f
        #t)))


;;; Bindings

(define state.add-binding
 (lambda (var value s)
   (if (null? (state.lookup var s))
       (cons (list var value) s)
       (state.add-binding var value (state.remove-binding var s)))))
     

(define state.remove-binding
 (lambda (var s)
   (cond
     ((null? s) '())
     ((eq? var (variable (mapping s))) (remaining s))
     (else (cons (mapping s) (state.remove-binding var (remaining s)))))))
       

(define state.lookup
  (lambda (var s)
    (cond
      ((null? s) '())
      ((eq? var (variable (mapping s))) (val (mapping s)))
      (else (state.lookup var (remaining s))))))

(define mapping
  (lambda (vartable) (car vartable)))

(define variable
  (lambda (mapping) (car mapping)))

(define val
  (lambda (mapping) (cadr mapping)))

(define remaining
  (lambda (vartable) (cdr vartable)))

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
    (varname stmt) (value (initialization stmt)) s)
   (state.add-binding
    (varname stmt) '() s))))

(define has-initialization
 (lambda (stmt) (not (null? (initialization stmt)))))

(define initialization
 (lambda (stmt) (caddr stmt)))


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
