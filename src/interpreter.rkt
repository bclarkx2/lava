;;; Interpreter project
;; Brian Clark
;; Danny Miles
;; Kaius Reed

#lang racket
(provide (all-defined-out))
(require "functionParser.rkt")
(require "simpleParser.rkt")

; top-level interpret function -- throws errors
(define interpret
  (lambda (filename)
    (with-handlers ([(lambda (msg) (error msg))
                     (lambda (msg) msg)])
     (interpret-raise filename))))

; interpret helper that raises exceptions
(define interpret-raise
  (lambda (filename)
     (value (list 'funcall 'main)
            (state-global-first-pass (fallback-parser filename)
                                     (state-empty))
            default-throw)))

; fallback that tries to parse with v3 parser,
; and falls back to v2 parser in case of failure
(define fallback-parser
  (lambda (filename)
    (with-handlers ([(lambda (msg) 'fine)
                     (lambda (msg) (simple-parser filename))])
      (parser filename))))


;;; Value
 
; Mathematical Operators
(define value-int
  (lambda (e s throw)
    (cond
      ((number? e) e)
      ((number? (car e)) (car e))
      ((and (eq? '- (operator e)) (unary? e))
       (- 0 (value (operand1 e s throw) s throw)))
      ((eq? '+ (operator e)) (compute + e s throw))
      ((eq? '- (operator e)) (compute - e s throw))
      ((eq? '* (operator e)) (compute * e s throw))
      ((eq? '/ (operator e)) (compute quotient e s throw))
      ((eq? '% (operator e)) (compute remainder e s throw))
      (else (error 'badop "Undefined int operator")))))

; Comparison and Boolean Operations
(define value-bool
  (lambda (e s throw)
    (cond
      ((eq? '== (operator e)) (compute eq? e s throw))
      ((eq? '!= (operator e)) (compute (lambda (a b) (not (eq? a b))) e s throw))
      ((eq? '> (operator e)) (compute > e s throw))
      ((eq? '< (operator e)) (compute < e s throw))
      ((eq? '>= (operator e)) (compute >= e s throw))
      ((eq? '<= (operator e)) (compute <= e s throw))
      ((eq? '&& (operator e)) (compute (lambda (a b) (and a b)) e s throw))
      ((eq? '|| (operator e)) (compute (lambda (a b) (or a b)) e s throw))
      ((eq? '! (operator e)) (not (value (operand1 e s throw) s throw)))
      (else (error 'badop "Undefined bool operator")))))

; Function calls
(define value-func
 (lambda (e s throw)
  (call/cc (lambda (return)
   (state-remove-layer
    (state (call-func-def e s)
           (state-function-first-pass (call-func-def e s)
                                      (new-func-env e s throw))
           default-brk
           default-cont
           return
           (mk-safe-throw throw s)))))))

(define value
  (lambda (e s throw)
    (if (list? e)
      (cond
        ((null? (cdr e)) (value (car e) s throw))
        ((int-operator? (operator e)) (value-int e s throw))
        ((bool-operator? (operator e)) (value-bool e s throw))
        ((eq? 'funcall (operator e)) (value-func e s throw))
        ((eq? '= (operator e)) (operand2 e s throw))
        (else (error 'badop "Undefined operator")))
      (cond
        ((number? e) e)
        ((boolean? e) e)
        ((eq? 'true e) #t)
        ((eq? 'false e) #f)
        (else (state-lookup e s))))))


;; Value helpers

(define mk-safe-throw
  (lambda (throw call-state)
   (lambda (throw-state val)
    (throw call-state val))))

(define call-func-params (lambda (e s) (car (closure e s))))
(define call-func-def (lambda (e s) (cadr (closure e s))))
(define call-func-env-procedure (lambda (e s) (caddr (closure e s))))

(define call-func-env
  (lambda (e s)
    ((call-func-env-procedure e s) s)))

(define closure
  (lambda (e s)
   (state-lookup (func-name e) s)))

(define new-func-env
  (lambda (e s throw)
   (resolve-params (state-add-layer (call-func-env e s))
                   s
                   (call-func-params e s)
                   (actual-params e)
                   throw)))

(define resolve-params
  (lambda (func-env cur-state formal actual throw)
    (cond
      ((and (null? formal) (null? actual))
       func-env)
      ((xor (null? formal) (null? actual))
       (raise 'parameter-mismatch))
      (else
       (resolve-params (resolve-param func-env cur-state formal actual throw)
                       cur-state
                       (cdr formal)
                       (cdr actual)
                       throw)))))

(define resolve-param
  (lambda (func-env cur-state formal actual throw)
    (state-add-binding (car formal)
                       (value (car actual) cur-state throw)
                       func-env)))

(define actual-params
  (lambda (e)
    (cddr e)))

(define operator
  (lambda (e)
    (car e)))

(define int-operator?
  (lambda (op)
    (member op '(+ - * / %))))

(define bool-operator?
  (lambda (op)
    (member op '(== != > < >= <= && || !))))

(define operator?
  (lambda (word)
    (or (int-operator? word)
        (bool-operator? word))))

(define unary?
  (lambda (lis)
    (if (pair? (cddr lis))#f
        #t)))

(define operand1
  (lambda (lis s throw) (operand (cadr lis) s throw)))

(define operand2
  (lambda (lis s throw) (operand (caddr lis) s throw)))

(define operand
  (lambda (expr s throw)
    (cond
      ((list? expr) (value expr s throw))
      ((or (number? expr)
           (eq? 'true expr)
           (eq? 'false expr))
           expr)
      (else (state-lookup expr s)))))

(define compute
  (lambda (op e s throw)
    (op (value (operand1 e s throw) s throw)
        (value (operand2 e s throw) s throw))))


;;; Bindings

(define state-empty (lambda () '((() ()))))
(define layer-empty (lambda () '(() ())))
(define null-value (lambda () '()))

;**IMPORTANT:  Add, Remove, and Lookup operate within the context of the topmost layer in the list
;Variables, Var-values, and Remaining return the variable, values, and remaining lists respectively for the topmost layer only
(define state-add-binding
  (lambda (var value s)
    (cons (list (cons var (top-layer-variables s))
                (cons (box value) (top-layer-values s)))
          (state-remaining s))))

(define state-set-binding
  (lambda (var newValue s)
    (cond
      ((equal? s (state-empty)) (state-empty))
      ((null? s) (null-value))
      ((member var (top-layer-variables s))
        (cons (change-binding var newValue (top-layer s))
              (state-remaining s)))
      (else
        (cons (top-layer s) 
              (state-set-binding var newValue (state-remaining s)))))))

(define state-lookup
  (lambda (var lis)
    (cond
      ((null? lis) (raise 'illegal-var-dereferencing))
      ((equal? lis (state-empty)) (raise 'illegal-var-dereferencing))
      ((equal? lis (layer-empty)) '())
      ((is-state? lis)
       (if (null? (state-lookup var (top-layer lis)))
           (state-lookup var (state-remaining lis))
           (state-lookup var (top-layer lis))))
      (else
       (if (eq? var (car (layer-variables lis)))
           (unbox (car (layer-values lis)))
           (state-lookup var (layer-remaining lis)))))))

(define is-declared
  (lambda (var lis)
    (cond
      ((null? lis) #f)
      ((equal? lis (layer-empty)) #f)
      ((is-state? lis) (or (is-declared var (top-layer lis))
                           (is-declared var (state-remaining lis))))
      ((eq? var (car (layer-variables lis))) #t)
      (else (is-declared var (layer-remaining lis))))))

(define is-declared-top-layer
 (lambda (var state)
  (is-declared var (top-layer state))))

(define state-add-layer
  (lambda (state)
    (cons '(() ()) state)))

(define state-remove-layer
  (lambda (state)
    (cdr state)))


; Binding helpers

(define change-binding
  (lambda (var newValue layer)
    (cond
      ((equal? layer (layer-empty)) (layer-empty))
      ((eq? var (car (layer-variables layer)))
        (begin (set-box! (car (layer-values layer)) newValue) layer))
      (else (list (cons (car (layer-variables layer))
                        (car (change-binding var newValue (layer-remaining layer))))
                  (cons (car (layer-values layer))
                        (cadr (change-binding var newValue (layer-remaining layer)))))))))

(define is-state?
  (lambda (s)
    (and (not (null? (car s))) (list? (caar s)))))

; top layer        
(define top-layer
  (lambda (state) (car state)))

(define top-layer-variables
 (lambda (state) (layer-variables (top-layer state))))

(define top-layer-values
  (lambda (state) (layer-values (top-layer state))))

; layer
(define layer-variables
  (lambda (layer) (car layer)))

(define layer-values
  (lambda (layer) (cadr layer)))

(define layer-remaining
  (lambda (state)
    (list (cdr (layer-variables state)) (cdr (layer-values state)))))

; overall state
(define state-remaining
  (lambda (state) (cdr state)))


;;; State gotos

(define default-brk (lambda (x) (raise 'illegal-break)))
(define default-cont (lambda (x) (raise 'illegal-cont)))
(define default-throw (lambda (x y) (raise 'illegal-throw)))
(define default-return (lambda (x) (raise 'illegal-return)))


;;; State Mappings

(define state-global-first-pass
  (lambda (stmt-list s)
    (cond
      ((null? stmt-list) s)
      ((not (list? stmt-list)) s)
  
      ; may be a list of statements
      ((list? (keyword stmt-list))
        (state-global-first-pass (cdr stmt-list)
                                 (state-global-first-pass (car stmt-list)
                                                          s)))

      ; remaining operations delegated to helpers
      ((eq? (keyword stmt-list) 'function)
       (state-function-declaration stmt-list s)) 
      ((eq? (keyword stmt-list) 'var)
       (state-var stmt-list s default-brk default-cont default-return default-throw))
      
      (else s))))

(define state-function-first-pass
  (lambda (stmt-list s)
    (cond
      ((null? stmt-list) s)
      ((not (list? stmt-list)) s)

      ((list? (keyword stmt-list))
       (state-function-first-pass (cdr stmt-list)
                                  (state-function-first-pass (car stmt-list)
                                                              s)))

      ((eq? (keyword stmt-list) 'function) (state-function-declaration stmt-list s))
      (else s))))

(define state
  (lambda (stmt s brk cont return throw)
    (cond

      ; null and return statements do not alter state
      ((null? stmt) s)
      ((not (list? stmt)) s)
      ((eq? (keyword stmt) 'return) (handle-return stmt s return throw))

      ; may be a list of statements
      ((list? (keyword stmt)) (state-list stmt s brk cont return throw))

      ; remaining operations delegated to helpers
      ((eq? (keyword stmt) '=) (state-assign stmt s brk cont return throw))
      ((eq? (keyword stmt) 'if) (state-if stmt s brk cont return throw))
      ((eq? (keyword stmt) 'var) (state-var stmt s brk cont return throw))
      ((eq? (keyword stmt) 'while) (state-while stmt s brk cont return throw))
      ((eq? (keyword stmt) 'begin) (state-block stmt s brk cont return throw))
      ((eq? (keyword stmt) 'try) (state-try stmt s brk cont return throw))
      ((eq? (keyword stmt) 'function) s)
      ((eq? (keyword stmt) 'funcall) (begin (value stmt s throw) s))

      ; goto keywords
      ((eq? (keyword stmt) 'break) (brk s))
      ((eq? (keyword stmt) 'continue) (cont s))
      ((eq? (keyword stmt) 'throw) (handle-throw stmt s throw))

      ; assignment
      ((operator? (keyword stmt))
       (if (unary? stmt)
           (state (cdr stmt) s brk cont return throw)
           (state (varexpr stmt) (state (varname stmt) s brk cont return throw) brk cont return throw)))
      
      (else s))))

(define keyword
  (lambda (stmt) (car stmt)))


;; Handlers

(define handle-throw
 (lambda (stmt s throw)
  (throw s (value (cdr stmt) s throw))))

(define handle-return
  (lambda (stmt s return throw)
    (cond 
      ((eq? (value (cdr stmt) s throw) #t) (return 'true))
      ((eq? (value (cdr stmt) s throw) #f) (return 'false))
      (else (return (value (cdr stmt) s throw))))))


;; Statement list

(define state-list
 (lambda (stmt s brk cont return throw)
  (state (cdr stmt)
         (state (car stmt) s brk cont return throw)
         brk cont return throw)))


;; Assignment

(define state-assign
  (lambda (stmt s brk cont return throw)
    (if (is-declared (varname stmt) s)
        (state-set-binding
         (varname stmt)
         (value (varexpr stmt) s throw)
         s)
         ;(state (varexpr stmt) s brk cont return throw))
        (raise 'assign-before-declare))))

(define varname
  (lambda (stmt) (cadr stmt)))

(define varexpr
  (lambda (stmt) (caddr stmt)))
  

;; If

(define state-if
  (lambda (stmt s brk cont return throw)
    (if (value (condition stmt) s throw)
        (state (stmt1 stmt)
               (state (condition stmt) s brk cont return throw)
               brk cont return throw)
        (state (stmt2 stmt)
               (state (condition stmt) s brk cont return throw)
               brk cont return throw))))

(define condition (lambda (stmt) (cadr stmt)))
(define stmt1 (lambda (stmt) (caddr stmt)))
(define stmt2
  (lambda (stmt)
    (if (null? (cdddr stmt))
        (null-value)
        (cadddr stmt))))


;; Var

(define state-var
  (lambda (stmt s brk cont return throw)
    (if (is-declared-top-layer (varname stmt) s)
        (raise 'illegal-var-use)
        (if (has-initialization stmt)
            (state-add-binding
             (varname stmt)
             (value (initialization stmt) s throw)
             (state (initialization stmt) s brk cont return throw))
            (state-add-binding
             (varname stmt) '() s)))))

  (define has-initialization
    (lambda (stmt) (not (null? (cddr stmt)))))

  (define initialization
    (lambda (stmt) (caddr stmt)))


;; While

(define state-while
  (lambda (stmt s brk cont return throw)
   (call/cc (lambda (while-brk)
     (if (value (condition stmt) s throw)
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
      (state (block-form-of-catch stmt throw-val)
             aborted-throw-state
             brk cont return throw)))))

(define block-form-of-catch
  (lambda (stmt throw-val)
    (if (is-catch? stmt)
      (list 'begin (list 'var (catch-var stmt) throw-val) (catch stmt))
      '())))

(define try (lambda (stmt) (cadr stmt)))
(define catch-var
 (lambda (stmt)
  (caadr (caddr stmt))))
(define is-catch?
  (lambda (stmt)
     (not (null? (caddr stmt)))))
(define catch
 (lambda (stmt)
  (if (is-catch? stmt)
   (caddr (caddr stmt))
   '())))

(define finally-block (lambda (stmt) (cadddr stmt)))
(define finally
 (lambda (stmt)
  (if (null? (finally-block stmt))
   '()
   (cadr (finally-block stmt)))))


;; Function

(define state-function-declaration
  (lambda (exp s)
    (state-add-binding (func-name exp) (list
                                        (func-params exp)
                                        (func-def exp)
                                        (mk-environment-func s))
                                        s)))

(define layer-count
  (lambda (s)
    (if (null? (state-remaining s))
        1
        (+ 1 (layer-count (state-remaining s))))))

(define mk-environment-func
  (lambda (s)
    (lambda (call-state)
      (subenvironment (layer-count s) (reverse call-state)))))

(define subenvironment
  (lambda (num-layers s)
    (cond
      ((eq? 1 num-layers) (cons (top-layer s) '()))
      ((equal? (state-empty) s) (raise 'Illegal-call))
      (else 
       (cons (top-layer s)
             (subenvironment (- num-layers 1)
                             (cdr s)))))))

(define func-name (lambda (exp) (cadr exp)))
(define func-params (lambda (exp) (caddr exp)))
(define func-def (lambda (exp) (cadddr exp)))
