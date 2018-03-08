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
    (value (parser filename) (state-empty) null-value null-value return null-value)))))


;;; General helpers

(define in?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((eq? a (car lis)) #t)
      (else (in? a (cdr lis))))))


;;; Value

(define value
  (lambda (stmt-list s brk cont return throw)
    (cond
      ((null? stmt-list) (null-value))
      ((list? (car stmt-list))
       (if (null? (value (car stmt-list) s brk cont return throw))
           (value (cdr stmt-list) (call/cc
                                   (lambda (return)
                                    (state (car stmt-list)
                                          s
                                          default-brk
                                          default-cont
                                          return
                                          default-throw)))brk cont return throw)
           (value (car stmt-list) s brk cont return throw)))
      (else
       (value-evaluate stmt-list s brk cont return throw)))))
     
;Mathematical Operators
(define value-int
  (lambda (e s brk cont return throw)
    (cond
      ((number? e) e)
      ((number? (car e)) (car e))
      ((and (eq? '- (operator e)) (unary? e))
       (- 0 (value-evaluate(operand1 e s brk cont return throw) s brk cont return throw)))
      ((eq? '+ (operator e)) (compute + e s brk cont return throw))
      ((eq? '- (operator e)) (compute - e s brk cont return throw))
      ((eq? '* (operator e)) (compute * e s brk cont return throw))
      ((eq? '/ (operator e)) (compute quotient e s brk cont return throw))
      ((eq? '% (operator e)) (compute remainder e s brk cont return throw))
      (else (error 'badop "Undefined int operator")))))

;Comparison and Boolean Operations
(define value-bool
  (lambda (e s brk cont return throw)
    (cond
      ((eq? '== (operator e)) (compute eq? e s brk cont return throw))
      ((eq? '!= (operator e)) (compute (lambda (a b) (not (eq? a b))) e s brk cont return throw))
      ((eq? '> (operator e)) (compute > e s brk cont return throw))
      ((eq? '< (operator e)) (compute < e s brk cont return throw))
      ((eq? '>= (operator e)) (compute >= e s brk cont return throw))
      ((eq? '<= (operator e)) (compute <= e s brk cont return throw))
      ((eq? '&& (operator e)) (compute (lambda (a b) (and a b)) e s brk cont return throw))
      ((eq? '|| (operator e)) (compute (lambda (a b) (or a b)) e s brk cont return throw))
      ((eq? '! (operator e)) (not (value-evaluate(operand1 e s brk cont return throw) s brk cont return throw)))
      (else (error 'badop "Undefined bool operator")))))

(define value-evaluate
  (lambda (e s brk cont return throw)
    (if (list? e)
        (cond
          ((or (eq? (operator e) 'break) (eq? (operator e) 'continue)) (value-statement e s brk cont return throw)) 
          ((null? (cdr e)) (value-evaluate (car e) s brk cont return throw))
          ((interpreter-keyword? (operator e)) (value-statement e s brk cont return throw))
          ((int-operator? (operator e)) (value-int e s brk cont return throw))
          ((bool-operator? (operator e)) (value-bool e s brk cont return throw))
          (else (error 'badop "Undefined operator")))
        (cond
          ((number? e) e)
          ((boolean? e) e)
          ((eq? 'true e) #t)
          ((eq? 'false e) #f)
          (else (state-lookup e s))))))

(define value-statement
  (lambda (stmt s brk cont return throw)
    (cond
      ((eq? 'return (keyword stmt)) (value-return stmt s brk cont return throw))
      ((eq? 'if (keyword stmt)) (value-if stmt s brk cont return throw))
      ((eq? 'begin (keyword stmt)) (value (cdr stmt) (state-add-layer s) brk cont return throw))
      ((eq? 'break (keyword stmt)) (brk '()))
      ((eq? 'while (keyword stmt)) (value-while stmt s brk cont return throw))
      ((eq? 'continue (keyword stmt)) (cont '()))   ;TODO
      (else (null-value)))))

;has a value if the chosen clause has a return statement
(define value-if
  (lambda (stmt s brk cont return throw)
    (if (value-evaluate (condition stmt) s brk cont return throw)
        (value (stmt1 stmt) s brk cont return throw)
        (value (stmt2 stmt) s brk cont return throw))))
    

(define value-return
  (lambda (e s brk cont return throw)
    (cond 
      ((eq? (value-evaluate (cdr e) s brk cont return throw) #t) (return 'true))
      ((eq? (value-evaluate (cdr e) s brk cont return throw) #f) (return 'false))
      (else (return (value-evaluate (cdr e) s brk cont return throw))))))

(define value-while
  (lambda (stmt s brk cont return throw)
    (call/cc
     (lambda (while-break)
       (if (value-evaluate (condition stmt) s brk cont return throw)
           (call/cc (lambda (while-cont)
            (if (null? (value (loopbody stmt)
                              (state (condition stmt) s null-value null-value null-value null-value)
                              while-break (cont-with-while stmt s while-break while-cont return throw) return throw))
               (value stmt
                      (call/cc (lambda (state-cont) 
                                 (state (loopbody stmt)
                                        (state (condition stmt)
                                               s
                                               null-value null-value null-value null-value)
                                        null-value state-cont null-value null-value)))
                      while-break cont return throw)
               (return (value (loopbody stmt) s while-break cont return throw)))))
           (while-break '()))))))

(define cont-with-while
  (lambda (stmt s brk cont return throw)
    (lambda (v)
      (cont (value stmt
                   (call/cc (lambda (state-cont)
                    (state (loopbody stmt)
                           (state (condition stmt)
                                  s
                                  null-value null-value null-value null-value)
                           null-value state-cont null-value null-value)))
                   brk cont return throw)))))

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
  (lambda (lis s brk cont return throw)
    (cond
      ((list? (cadr lis)) (value (cadr lis) s brk cont return throw))
      ((or (number? (cadr lis))
           (eq? 'true (cadr lis))
           (eq? 'false (cadr lis)))
           (cadr lis))
      (else (state-lookup (cadr lis) s)))))

(define operand2
  (lambda (lis s brk cont return throw)
    (cond
      ((list? (caddr lis)) (value (caddr lis) s brk cont return throw))
      ((or (number? (caddr lis))
           (eq? 'true (caddr lis))
           (eq? 'false (caddr lis)))
           (caddr lis))
      (else (state-lookup (caddr lis) s)))))
    
(define compute
  (lambda (op e s brk cont return throw)
    (op (value-evaluate (operand1 e s brk cont return throw) s brk cont return throw)
        (value-evaluate (operand2 e s brk cont return throw) s brk cont return throw))))

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
(define null-value (lambda () '()))

;**IMPORTANT:  Add, Remove, and Lookup operate within the context of the topmost layer in the list
;Variables, Var-values, and Remaining return the variable, values, and remaining lists respectively for the topmost layer only
(define state-add-binding
  (lambda (var value s)
    (cons (list (cons var (variables s)) (cons value (var-values s))) (cdr s))))
     

(define state-remove-binding
  (lambda (var s)
    (cond
      ((equal? s (state-empty)) (state-empty))
      ((eq? var (car (variables s))) (remaining s))
      (else (list
             (cons (car (variables s)) (car (state-remove-binding var (remaining s))))
             (cons (car (var-values s)) (cadr (state-remove-binding var (remaining s)))))))))
       
(define state-lookup
  (lambda (var s)
    (cond
      ((null? (variables s)) (raise 'illegal-var-dereferencing))
      ((eq? var (car (variables s)))
       (if (null? (car (var-values s))) (raise 'illegal-var-dereferencing)
           (car (var-values s))))
       (else (state-lookup var (remaining s))))))

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
  (lambda (state) (car (top-layer state))))

(define var-values
  (lambda (state) (cadr (top-layer state))))

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
      ((eq? (keyword stmt) 'return) (return s))

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
  (throw s (value (cdr stmt) s null-value null-value null-value throw))))


;; Statement list

(define state-list
 (lambda (stmt s brk cont return throw)
  (state (cdr stmt)
         (state (car stmt) s brk cont return throw)
         brk cont return throw)))


;; Assignment

(define state-assign
  (lambda (stmt s)
    (if (in? (varname stmt) (variables s))
    (state-add-binding
     (varname stmt)
     (value-evaluate (varexpr stmt) s null-value null-value null-value null-value)  ;value of the expression
     (state-remove-binding (varname stmt) s))
    (raise 'assign-before-declare))))

(define varname
  (lambda (stmt) (cadr stmt)))

(define varexpr
  (lambda (stmt) (caddr stmt)))
  


;; If

(define state-if
  (lambda (stmt s brk cont return throw)
    (if (value-evaluate (condition stmt) s null-value null-value null-value null-value)
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
    (if (in? (varname stmt) (variables s))
        (raise 'illegal-var-use)
        (if (has-initialization stmt)
            (state-add-binding
             (varname stmt) (value-evaluate (car (initialization stmt)) s null-value null-value null-value null-value) s)
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
     (if (value-evaluate (condition stmt) s null-value null-value null-value null-value)
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
          brk cont return throw))))

(define block-contents (lambda (stmt) (cdr stmt)))


;; Try

; TODO: only have one case and pass '() to state for undef blocks
(define state-try
 (lambda (stmt s brk cont return throw)
  ;;; (cond 
  ;;;  ((and (null? (finally stmt))
  ;;;        (null? (catch stmt)))
  ;;;   (state (try stmt) s brk cont return throw))
  ;;;  ((null? (catch stmt))
  ;;;   (state (finally stmt)
  ;;;          (call/cc (lambda (try-throw)
  ;;;           (state (try stmt) s brk cont return
  ;;;            (lambda (aborted-throw-state val)
  ;;;             (try-throw aborted-throw-state)))))
  ;;;          brk cont return throw))
  ;;;  ((null? (finally stmt))
  ;;;   (call/cc (lambda (final-state)
  ;;;    (state (try stmt)
  ;;;           s
  ;;;           brk cont return
  ;;;            (throw-that-does-catch stmt
  ;;;                                   final-state
  ;;;                                   brk cont return throw)))))
  ;;;  (else 
    (state (finally stmt)
           (call/cc (lambda (try-state)
            (state (try stmt)
                   s
                   brk cont return
                    (throw-that-does-catch stmt
                                           try-state
                                           brk
                                           cont
                                           return
                                           throw))))
           brk cont return throw)))

(define throw-that-does-catch
 (lambda (stmt result-state brk cont return throw)
  (lambda (aborted-throw-state throw-val)
   (result-state
     (state-remove-layer
      (state (catch stmt)
             (state-add-binding
              (catch-var stmt)
              throw-val
              (state-add-layer aborted-throw-state))
             brk cont return throw))))))



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
