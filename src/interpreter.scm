;;; Interpreter project
;; Brian Clark
;; Danny Miles
;; Kaius Reed

#lang racket
(provide (all-defined-out))
(require "simpleParser.scm")

(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (state (parser filename) (state-empty) default-brk default-cont return default-throw)))))


;;; General helpers

(define in?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((eq? a (car lis)) #t)
      (else (in? a (cdr lis))))))


;;; Value

 
;Mathematical Operators
(define value-int
  (lambda (e s)
    (cond
      ((number? e) e)
      ((number? (car e)) (car e))
      ((and (eq? '- (operator e)) (unary? e))
       (- 0 (value-evaluate(operand1 e s) s)))
      ((eq? '+ (operator e)) (compute + e s))
      ((eq? '- (operator e)) (compute - e s))
      ((eq? '* (operator e)) (compute * e s))
      ((eq? '/ (operator e)) (compute quotient e s))
      ((eq? '% (operator e)) (compute remainder e s))
      (else (error 'badop "Undefined int operator")))))

;Comparison and Boolean Operations
(define value-bool
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
      ((eq? '! (operator e)) (not (value-evaluate(operand1 e s) s)))
      (else (error 'badop "Undefined bool operator")))))

(define value-evaluate
  (lambda (e s)
    (if (list? e)
        (cond
          ((null? (cdr e)) (value-evaluate (car e) s))
          ((int-operator? (operator e)) (value-int e s))
          ((bool-operator? (operator e)) (value-bool e s))
          (else (error 'badop "Undefined operator")))
        (cond
          ((number? e) e)
          ((boolean? e) e)
          ((eq? 'true e) #t)
          ((eq? 'false e) #f)
          (else (state-lookup e s))))))


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
      ((list? (cadr lis)) (value-evaluate (cadr lis) s))
      ((or (number? (cadr lis))
           (eq? 'true (cadr lis))
           (eq? 'false (cadr lis)))
           (cadr lis))
      (else (state-lookup (cadr lis) s)))))

(define operand2
  (lambda (lis s)
    (cond
      ((list? (caddr lis)) (value-evaluate (caddr lis) s))
      ((or (number? (caddr lis))
           (eq? 'true (caddr lis))
           (eq? 'false (caddr lis)))
           (caddr lis))
      (else (state-lookup (caddr lis) s)))))
    
(define compute
  (lambda (op e s)
    (op (value-evaluate (operand1 e s) s)
        (value-evaluate (operand2 e s) s))))

(define unary?
  (lambda (lis)
    (if (pair? (cddr lis))#f
        #t)))

;if the car of the lis is a program-defined keyword, we must execute the command
(define interpreter-keyword?
  (lambda (atom)
    (in? atom '(var = if while return begin break continue try))))


;;; Bindings

(define state-empty (lambda () '((() ()))))
(define layer-empty (lambda () '(() ())))
(define null-value (lambda () '()))

;**IMPORTANT:  Add, Remove, and Lookup operate within the context of the topmost layer in the list
;Variables, Var-values, and Remaining return the variable, values, and remaining lists respectively for the topmost layer only
(define state-add-binding
  (lambda (var value s)
    (cons (list (cons var (variables (top-layer s))) (cons (box value) (var-values (top-layer s)))) (cdr s))))

(define state-set-binding
  (lambda (var newValue s)
    (cond
      ((equal? s (state-empty)) (state-empty))
      ((null? s) (null-value))
      ((in? var (variables (top-layer s))) (cons (change-binding var newValue (top-layer s)) (cdr s)))
      (else (cons (top-layer s) (state-set-binding var newValue (cdr s)))))))

(define change-binding
  (lambda (var newValue layer)
    (cond
      ((equal? layer (layer-empty)) (layer-empty))
      ((eq? var (car (variables layer))) (begin (set-box! (car (var-values layer)) newValue) layer))
      (else (list (cons (car (variables layer)) (car (change-binding var newValue (list (cdr (variables layer)) (cdr (var-values layer))))))
                  (cons (car (var-values layer)) (cadr (change-binding var newValue (list (cdr (variables layer)) (cdr (var-values layer)))))))))))
                       

(define state-lookup
  (lambda (var lis)
    (cond
      ((null? lis) (raise 'illegal-var-dereferencing))
      ((equal? lis (state-empty)) (raise 'illegal-var-deferencing))
      ((equal? lis (layer-empty)) '())
      ((is-state lis)
       (if (null? (state-lookup var (top-layer lis)))
           (state-lookup var (cdr lis))
           (state-lookup var (top-layer lis))))
      (else
       (if (eq? var (car (variables lis)))
           (unbox (car (var-values lis)))
           (state-lookup var (list (cdr (variables lis)) (cdr (var-values lis)))))))))

(define is-declared
  (lambda (var lis)
    (cond
      ((null? lis) #f)
      ((equal? lis (layer-empty)) #f)
      ((is-state lis) (or (is-declared var (top-layer lis)) (is-declared var (cdr lis))))
      ((eq? var (car (variables lis))) #t)
      (else (is-declared var (list (cdr (variables lis)) (cdr (var-values lis))))))))
       
     

(define is-state
  (lambda (s)
    (and (not (null? (car s))) (list? (caar s)))))
        
(define state-add-layer
  (lambda (state)
    (cons '(() ()) state)))

(define state-remove-layer
  (lambda (state)
    (cdr state)))

(define top-layer
  (lambda (state)
    (car state)))

(define variables
  (lambda (layer) (car layer)))

(define var-values
  (lambda (layer) (cadr layer)))

(define remaining
  (lambda (state)
    (list (cdr (variables state)) (cdr (var-values state)))))

;;; State gotos

(define default-brk (lambda (x) (raise 'illegal-break)))
(define default-cont (lambda (x) (raise 'illegal-cont)))
(define default-throw (lambda (x y) (raise 'illegal-throw)))


;;; State Mappings

(define top-level-state
 (lambda (stmt-lst)
  (call/cc
   (lambda (return)
    (state stmt-lst
           (state-empty)
           default-brk
           default-cont
           return)))))
  

(define state
  (lambda (stmt s brk cont return throw)
    (cond

      ; null and return statements do not alter state
      ((null? stmt) s)
      ((eq? (keyword stmt) 'return) (handle-return stmt s return))

      ; may be a list of statements
      ((list? (keyword stmt)) (state-list stmt s brk cont return throw))

      ; remaining operations delegated to helpers
      ((eq? (keyword stmt) '=) (state-assign stmt s))
      ((eq? (keyword stmt) 'if) (state-if stmt s brk cont return throw))
      ((eq? (keyword stmt) 'var) (state-var stmt s))
      ((eq? (keyword stmt) 'while) (state-while stmt s brk cont return throw))
      ((eq? (keyword stmt) 'begin) (state-block stmt s brk cont return throw))
      ((eq? (keyword stmt) 'try) (state-try stmt s brk cont return throw))

      ; goto keywords
      ((eq? (keyword stmt) 'break) (brk s))
      ((eq? (keyword stmt) 'continue) (cont s))
      ((eq? (keyword stmt) 'throw) (handle-throw stmt s throw))

      (else s))))

(define keyword
  (lambda (stmt) (car stmt)))

(define handle-throw
 (lambda (stmt s throw)
  (throw s (value-evaluate (cdr stmt) s))))

(define handle-return
  (lambda (stmt s return)
    (cond 
      ((eq? (value-evaluate (cdr stmt) s) #t) (return 'true))
      ((eq? (value-evaluate (cdr stmt) s) #f) (return 'false))
      (else (return (value-evaluate (cdr stmt) s))))))

;; Statement list

(define state-list
 (lambda (stmt s brk cont return throw)
  (state (cdr stmt)
         (state (car stmt) s brk cont return throw)
         brk cont return throw)))


;; Assignment

(define state-assign
  (lambda (stmt s)
    (if (is-declared (varname stmt) s)
        (state-set-binding
         (varname stmt)
         (value-evaluate (varexpr stmt) s)
         s)
        (raise 'assign-before-declare))))

(define varname
  (lambda (stmt) (cadr stmt)))

(define varexpr
  (lambda (stmt) (caddr stmt)))
  


;; If

(define state-if
  (lambda (stmt s brk cont return throw)
    (if (value-evaluate (condition stmt) s)
        (state (stmt1 stmt) s brk cont return throw)
        (state (stmt2 stmt) s brk cont return throw))))

(define condition (lambda (stmt) (cadr stmt)))
(define stmt1 (lambda (stmt) (caddr stmt)))
(define stmt2
  (lambda (stmt)
    (if (null? (cdddr stmt))
        (null-value)
        (cadddr stmt))))


;; Var

(define state-var
  (lambda (stmt s)
    (if (is-declared (varname stmt) s)
        (raise 'illegal-var-use)
        (if (has-initialization stmt)
            (state-add-binding
             (varname stmt) (value-evaluate (car (initialization stmt)) s) s)
            (state-add-binding
             (varname stmt) '() s)))))

  (define has-initialization
    (lambda (stmt) (not (null? (initialization stmt)))))

  (define initialization
    (lambda (stmt) (cddr stmt)))


;; While

(define state-while
  (lambda (stmt s brk cont return throw)
   (call/cc (lambda (while-brk)
     (if (value-evaluate (condition stmt) s)
           (state stmt
                   (call/cc (lambda (while-cont) 
                    (state (loopbody stmt)
                           (state (condition stmt)
                                  s
                                  brk cont return throw)
                           while-brk while-cont return throw)))
                    while-brk cont return throw)
          (state (condition stmt) s brk cont return throw))))))

(define loopbody
  (lambda (stmt) (caddr stmt)))


;; Block

(define state-block
 (lambda (stmt s brk cont return throw)
  (state-remove-layer
   (state (block-contents stmt) 
          (state-add-layer s)
          (lambda (v) (brk (state-remove-layer v))) (lambda (v) (cont (state-remove-layer v))) return (lambda (state val) (throw (state-remove-layer state) val))))))

(define block-contents (lambda (stmt) (cdr stmt)))


;; Try

; TODO: only have one case and pass '() to state for undef blocks
(define state-try
 (lambda (stmt s brk cont return throw)
    (state (finally stmt)
           (call/cc (lambda (try-state)
            (state (try stmt)
                   s
                   (break-that-does-finally stmt brk cont return throw) cont return
                    (throw-that-does-catch stmt
                                           try-state
                                           (break-that-does-finally stmt brk cont return throw)
                                           cont
                                           return
                                           throw))))
           brk cont return throw)))

(define break-that-does-finally
  (lambda (stmt brk cont return throw)
    (lambda (exiting-state)
      (brk (state (finally stmt) exiting-state brk cont return throw)))))

(define throw-that-does-catch
 (lambda (stmt result-state brk cont return throw)
  (lambda (aborted-throw-state throw-val)
   (result-state
      (state (list 'begin (list 'var (catch-var stmt) throw-val) (catch stmt))
             aborted-throw-state
             brk cont return throw)))))
             


(define try (lambda (stmt) (cadr stmt)))
(define catch-var
 (lambda (stmt)
  (caadr (caddr stmt))))
(define catch
 (lambda (stmt)
  (if (null? (caddr stmt))
   '()
   (caddr (caddr stmt)))))

  
(define finally-block (lambda (stmt) (cadddr stmt)))
(define finally
 (lambda (stmt)
  (if (null? (finally-block stmt))
   '()
   (cadr (finally-block stmt)))))
