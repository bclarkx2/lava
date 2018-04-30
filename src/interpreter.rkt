;;; Interpreter project
;; Brian Clark
;; Danny Miles
;; Kaius Reed

#lang racket
(provide (all-defined-out))

(require "classParser.rkt")
(require "functionParser.rkt")
(require "simpleParser.rkt")

; top-level interpret function -- throws errors
(define interpret
  (lambda (filename classname)
    (with-handlers ([(lambda (msg) (error msg))
                     (lambda (msg) msg)])
     (interpret-raise filename classname))))

; interpret helper that raises exceptions
(define interpret-raise
  (lambda (filename classname)
    (interpret-main classname (class-global-first-pass (fallback-parser filename)
                                                       (state-empty)))))

(define interpret-main
 (lambda (classname state)
  (value-func
   (state-lookup 'main
    (class-static-functions
     (state-lookup classname state '()))'())
   (default-this)
   (list 'funcall 'main)
   state
   default-throw
   classname)))
  

; all accepted parsers, in order of usage
(define parsers
 (lambda ()
  (list parser function-parser simple-parser)))
  

; fallback parser that tries to parse with v4 parser,
; and falls back to v3/v2 parser in case of failure
(define fallback-parser
 (lambda (filename)
  (try-parsers filename (parsers))))


; attempts to parse filename with all in parsers
(define try-parsers
  (lambda (filename parsers)
    (if (null? parsers)
      (raise 'no-valid-parser)
      (with-handlers ([(lambda (msg) 'fine)
                       (lambda (msg) (try-parsers filename
                                                      (cdr parsers)))])
        ((car parsers) filename)))))


;;; Value
 
; Mathematical Operators
(define value-int
  (lambda (e s symbol? throw current-type)
    (cond
      ((number? e) e)
      ((number? (car e)) (car e))
      ((and (symbol? '-) (unary? e))
       (- 0 (value (operand1 e s throw current-type) s throw current-type)))
      ((symbol? '+) (compute + e s throw current-type))
      ((symbol? '-) (compute - e s throw current-type))
      ((symbol? '*) (compute * e s throw current-type))
      ((symbol? '/) (compute quotient e s throw current-type))
      ((symbol? '%) (compute remainder e s throw current-type))
      (else (error 'badop "Undefined int operator")))))

; Comparison and Boolean Operations
(define value-bool
  (lambda (e s symbol? throw current-type)
    (cond
      ((symbol? '==) (compute eq? e s throw current-type))
      ((symbol? '!=) (compute (lambda (a b) (not (eq? a b))) e s throw current-type))
      ((symbol? '>) (compute > e s throw current-type))
      ((symbol? '<) (compute < e s throw current-type))
      ((symbol? '>=) (compute >= e s throw current-type))
      ((symbol? '<=) (compute <= e s throw current-type))
      ((symbol? '&&) (compute (lambda (a b) (and a b)) e s throw current-type))
      ((symbol? '||) (compute (lambda (a b) (or a b)) e s throw current-type))
      ((symbol? '!) (not (value (operand1 e s throw current-type) s throw current-type)))
      (else (error 'badop "Undefined bool operator")))))


; Function calls
(define value-func
 (lambda (closure this expr s throw current-type)
  (call/cc (lambda (return)
   (state-remove-layer
    (state (call-func-def closure)
           (state-function-first-pass (call-func-def closure)
                                      (new-func-env closure this
                                                            expr
                                                            s
                                                            throw
                                                            current-type))
           default-brk
           default-cont
           return
           (mk-safe-throw throw s)
           (function-class closure s)))))))


(define value-dot
  (lambda (expr state throw current-type)
    (field-lookup (dot-member-part expr)
                  current-type
                  (eval-reference expr state throw current-type)
                  state)))

(define value-new
  (lambda (expr s current-type)
    ((default-constructor (state-lookup (true-type expr) s current-type))
     s)))

(define value
  (lambda (e s throw current-type)
    (let ([symbol? (lambda (sym) (eq? sym (operator e)))])
      (if (list? e)
        (cond
          ((null? (cdr e)) (value (car e) s throw current-type))
          ((int-operator? (operator e)) (value-int e s symbol? throw current-type))
          ((bool-operator? (operator e)) (value-bool e s symbol? throw current-type))
          ((symbol? 'funcall) (value-func (method-lookup (funcall-reference e s throw current-type)
                                                         (funcall-name e)
                                                         s current-type)
                                          (funcall-reference e s throw current-type)
                                          e s throw current-type))
          ((symbol? '=) (operand2 e s throw current-type))
          ((symbol? 'new) (value-new e s current-type))
          ((symbol? 'dot) (value-dot e s throw current-type))
          (else e))
        (cond
          ((number? e) e)
          ((boolean? e) e)
          ((eq? 'true e) #t)
          ((eq? 'false e) #f)
          (else (state-lookup e s current-type)))))))


;; Value helpers

(define mk-safe-throw
  (lambda (throw call-state)
   (lambda (throw-state val)
    (throw call-state val))))

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
    (not (pair? (cddr lis)))))

(define operand1
  (lambda (lis s throw current-type)
    (operand (cadr lis) s throw current-type)))

(define operand2
  (lambda (lis s throw current-type)
    (operand (caddr lis) s throw current-type)))

(define operand
  (lambda (expr s throw current-type)
    (cond
      ((list? expr) (value expr s throw current-type))
      ((or (number? expr)
           (eq? 'true expr)
           (eq? 'false expr))
           expr)
      (else (state-lookup expr s current-type)))))

(define compute
  (lambda (op e s throw current-type)
    (op (value (operand1 e s throw current-type)
               s
               throw
               current-type)
        (value (operand2 e s throw current-type)
               s
               throw
               current-type))))

(define true-type (lambda (e) (cadr e)))


;;; Method binding

; (funcall expr, state) -> instance closure
(define funcall-reference
  (lambda (expr state throw current-type)
    (eval-reference (funcall-ref expr) state throw current-type)))
  
; (funcall expr) -> func name
(define funcall-name
  (lambda (expr)
    (if (list? (funcall-ref expr))
      (dot-member-part (funcall-ref expr))
      (cadr expr))))

(define eval-reference
  (lambda (ref-part state throw current-type)
    (if (list? ref-part)
      (value (dot-ref-part ref-part) state throw current-type)
      (state-lookup 'this state current-type))))

; (instance closure, function name, state) -> function closure
(define method-lookup
 (lambda (iclosure fname state current-type)
  (state-lookup fname
                (class-instance-functions (instance-true-type iclosure state current-type)) current-type)))

(define funcall-ref (lambda (funcall-expr) (cadr funcall-expr)))

(define dot-ref-part (lambda (dot-expr) (cadr dot-expr)))
(define dot-member-part (lambda (dot-expr) (caddr dot-expr)))
  

;;; Closures

;; Function closures

(define function-closure
 (lambda (params
          def
          env-func
          class-func)
  (list
   params
   def
   env-func
   class-func)))

(define call-func-params (lambda (closure) (car closure)))
(define call-func-def (lambda (closure) (cadr closure)))
(define call-func-env-procedure (lambda (closure) (caddr closure)))

(define call-func-env
  (lambda (closure s)
    ((call-func-env-procedure closure) s)))

(define new-func-env
  (lambda (closure this e s throw current-type)
   (resolve-params (state-add-layer (call-func-env closure s))
                   s
                   (if (equal? (default-this) this)
                     (call-func-params closure)
                     (cons 'this (call-func-params closure)))
                   (if (equal? (default-this) this)
                     (actual-params e)
                     (cons this (actual-params e)))
                   throw
                   current-type)))

(define resolve-params
  (lambda (func-env cur-state formal actual throw current-type)
    (cond
      ((and (null? formal) (null? actual))
       func-env)
      ((xor (null? formal) (null? actual))
       (raise 'parameter-mismatch))
      (else
       (resolve-params (resolve-param func-env
                                      cur-state
                                      formal
                                      actual
                                      throw
                                      current-type)
                       cur-state
                       (cdr formal)
                       (cdr actual)
                       throw
                       current-type)))))

(define resolve-param
  (lambda (func-env cur-state formal actual throw current-type)
    (state-add-binding (car formal)
                       (value (car actual)
                              cur-state
                              throw
                              current-type)
                       func-env)))

(define actual-params
  (lambda (e)
    (cddr e)))

(define function-class
 (lambda (fclosure state)
  ((cadddr fclosure) state)))


;; Class closures

(define class-closure
 (lambda (parent
          instance-field-names
          class-static-functions
          class-instance-functions
          class-constructors)
  (list parent
        instance-field-names
        class-static-functions
        class-instance-functions
        class-constructors)))
  

(define class-parent-name (lambda (closure) (car closure)))

(define class-parent
 (lambda (closure state current-type)
  (state-lookup (class-parent-name)
                state current-type)))

(define class-instance-field-names
 (lambda (closure)
  (cadr closure)))

(define class-static-functions
 (lambda (closure)
  (caddr closure)))

(define class-instance-functions
 (lambda (closure)
  (cadddr closure)))

(define class-constructors
 (lambda (closure)
  (list-ref closure 4)))

(define default-constructor
 (lambda (closure)
  (car (class-constructors closure))))

;; Instance closures

(define instance-closure
  (lambda (true-type instance-field-values)
    (list true-type instance-field-values)))

(define instance-true-type-name
  (lambda (closure)
    (car closure)))

(define instance-true-type
  (lambda (closure state current-type)
   (state-lookup (instance-true-type-name closure)
                 state current-type)))

(define instance-field-values
  (lambda (closure)
   (cadr closure)))
  

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
  (lambda (var s current-type)
    (if (null? (resolve-in-state var s current-type))
      (if (has-this? s current-type)
        (field-lookup var
                      current-type
                      (resolve-in-state 'this s current-type)
                      s)
        (raise 'illegal-var-dereferencing))
      (resolve-in-state var s current-type))))

(define is-declared
  (lambda (var lis)
    (cond
      ;;; (with-handlers ([(lambda (err) #f)
      ;;;                  (lambda (result) #t)])
      ;;;   (state-lookup var lis current-type)))))
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

; resolve
(define resolve-in-state
  (lambda (var lis current-type)
    (cond
      ((null? lis) '())
      ((equal? lis (state-empty)) '())
      ((equal? lis (layer-empty)) '())
      ((is-state? lis)
       (if (null? (resolve-in-state var (top-layer lis) current-type))
           (resolve-in-state var (state-remaining lis) current-type)
           (resolve-in-state var (top-layer lis) current-type)))
      (else
       (if (eq? var (car (layer-variables lis)))
           (unbox (car (layer-values lis)))
           (resolve-in-state var (layer-remaining lis) current-type))))))
  
(define has-this?
 (lambda (state current-type)
   (not (null? (resolve-in-state 'this state current-type))))) 
  

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
(define default-this (lambda () 'no-this))


;;; State Mappings

(define state-class-vars
  (lambda (stmt-list s current-type)
    (cond
      ((null? stmt-list) s)
      ((not (list? stmt-list)) s)
  
      ; may be a list of statements
      ((list? (keyword stmt-list))
        (state-class-vars (cdr stmt-list)
                          (state-class-vars (car stmt-list)
                                            s
                                            current-type)
                          current-type))

      ((eq? (keyword stmt-list) 'var)
       (state-var stmt-list
                  s
                  default-brk
                  default-cont
                  default-return
                  default-throw
                  current-type))
      
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
  (lambda (stmt s brk cont return throw current-type)
    (cond

      ; null and return statements do not alter state
      ((null? stmt) s)
      ((not (list? stmt)) s)
      ((eq? (keyword stmt) 'return) (handle-return stmt s return throw current-type))

      ; may be a list of statements
      ((list? (keyword stmt)) (state-list stmt s brk cont return throw current-type))

      ; remaining operations delegated to helpers
      ((eq? (keyword stmt) '=) (state-assign stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'if) (state-if stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'var) (state-var stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'while) (state-while stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'begin) (state-block stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'try) (state-try stmt s brk cont return throw current-type))
      ((eq? (keyword stmt) 'function) s)
      ((eq? (keyword stmt) 'funcall) (begin (value stmt s throw current-type) s))
      ((eq? (keyword stmt) 'new) s)
      ((eq? (keyword stmt) 'class) s)

      ; goto keywords
      ((eq? (keyword stmt) 'break) (brk s))
      ((eq? (keyword stmt) 'continue) (cont s))
      ((eq? (keyword stmt) 'throw) (handle-throw stmt s throw current-type))

      ; assignment
      ((operator? (keyword stmt))
       (if (unary? stmt)
           (state (cdr stmt) s brk cont return throw current-type)
           (state (varexpr stmt)
                  (state (varname stmt)
                         s brk cont return throw current-type)
                  brk cont return throw)))
      
      (else s))))

(define keyword
  (lambda (stmt) (car stmt)))


;; Handlers

(define handle-throw
 (lambda (stmt s throw current-type)
  (throw s (value (cdr stmt) s throw current-type))))

(define handle-return
  (lambda (stmt s return throw current-type)
    (cond 
      ((eq? (value (cdr stmt) s throw current-type) #t) (return 'true))
      ((eq? (value (cdr stmt) s throw current-type) #f) (return 'false))
      (else (return (value (cdr stmt) s throw current-type))))))


;; Statement list

(define state-list
 (lambda (stmt s brk cont return throw current-type)
  (state (cdr stmt)
         (state (car stmt) s brk cont return throw current-type)
         brk cont return throw current-type)))


;; Assignment

(define state-assign
  (lambda (stmt s brk cont return throw current-type)
    (if (list? (varname stmt))
      (field-update (dot-member-part (varname stmt))
                        current-type
                        (state-lookup (dot-ref-part (varname stmt))
                                      s
                                      current-type)
                        s
                        (value (varexpr stmt) s throw current-type))
      (if (null? (resolve-in-state (varname stmt) s current-type))
          (field-update (dot-member-part (varname stmt))
                        current-type
                        (state-lookup 'this s current-type)
                        s
                        (value (varexpr stmt) s throw current-type))
          (state-set-binding
           (varname stmt)
           (value (varexpr stmt) s throw current-type)
           s)))))

(define varname
  (lambda (stmt) (cadr stmt)))

(define varexpr
  (lambda (stmt) (caddr stmt)))
  

;; If

(define state-if
  (lambda (stmt s brk cont return throw current-type)
    (if (value (condition stmt) s throw current-type)
        (state (stmt1 stmt)
               (state (condition stmt) s brk cont return throw current-type)
               brk cont return throw current-type)
        (state (stmt2 stmt)
               (state (condition stmt) s brk cont return throw current-type)
               brk cont return throw current-type))))

(define condition (lambda (stmt) (cadr stmt)))
(define stmt1 (lambda (stmt) (caddr stmt)))
(define stmt2
  (lambda (stmt)
    (if (null? (cdddr stmt))
        (null-value)
        (cadddr stmt))))


;; Var

(define state-var
  (lambda (stmt s brk cont return throw current-type)
    (if (is-declared-top-layer (varname stmt) s)
        (raise 'illegal-var-use)
        (if (has-initialization stmt)
            (state-add-binding
             (varname stmt)
             (value (initialization stmt) s throw current-type)
             s)
             ;;; (state (initialization stmt) s brk cont return throw))
            (state-add-binding
             (varname stmt) '() s)))))

  (define has-initialization
    (lambda (stmt) (not (null? (cddr stmt)))))

  (define initialization
    (lambda (stmt) (caddr stmt)))


;; While

(define state-while
  (lambda (stmt s brk cont return throw current-type)
   (call/cc (lambda (while-brk)
     (if (value (condition stmt) s throw current-type)
         (state stmt
                (call/cc (lambda (while-cont) 
                 (state (loopbody stmt)
                        (state (condition stmt)
                               s
                               brk cont return throw current-type)
                        while-brk while-cont return throw current-type)))
                 while-brk cont return throw current-type)
          (state (condition stmt) s brk cont return throw current-type))))))

(define loopbody
  (lambda (stmt) (caddr stmt)))


;; Block

(define state-block
 (lambda (stmt s brk cont return throw current-type)
  (state-remove-layer
   (state (block-contents stmt) 
          (state-add-layer s)
          (lambda (v) (brk (state-remove-layer v)))
          (lambda (v) (cont (state-remove-layer v)))
          return 
          (lambda (state val)
           (throw (state-remove-layer state) val))))))

(define block-contents (lambda (stmt) (cdr stmt)))


;; Try

(define state-try
 (lambda (stmt s brk cont return throw current-type)
    (state (finally stmt)
           (call/cc (lambda (try-state)
            (state (try stmt)
                   s
                   (break-that-does-finally stmt brk cont return throw current-type) cont return
                    (throw-that-does-catch stmt
                                           try-state
                                           (break-that-does-finally stmt brk cont return throw)
                                           cont
                                           return
                                           throw
                                           current-type))))
           brk cont return throw current-type)))

(define break-that-does-finally
  (lambda (stmt brk cont return throw current-type)
    (lambda (exiting-state)
      (brk (state (finally stmt)
                  exiting-state 
                  brk cont return throw
                  current-type)))))

(define throw-that-does-catch
 (lambda (stmt result-state brk cont return throw current-type)
  (lambda (aborted-throw-state throw-val)
   (result-state
      (state (block-form-of-catch stmt throw-val)
             aborted-throw-state
             brk cont return throw current-type)))))

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
  (lambda (exp s current-type)
    (state-add-binding (func-name exp) (function-closure
                                        (func-params exp)
                                        (func-def exp)
                                        (mk-environment-func s)
                                        (mk-class-func s current-type))
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

(define mk-class-func
 (lambda (s current-type)
  (lambda (call-state)
   current-type)))
  

(define func-name (lambda (exp) (cadr exp)))
(define func-params (lambda (exp) (caddr exp)))
(define func-def (lambda (exp) (cadddr exp)))


;; Class definition

(define class-global-first-pass
  (lambda (stmt-list s)
    (cond
      ((null? stmt-list) s)
      ((not (list? stmt-list)) s)

      ((list? (keyword stmt-list))
       (class-global-first-pass (cdr stmt-list)
                                (class-global-first-pass (car stmt-list)
                                                         s)))

      ((eq? (keyword stmt-list) 'class)
       (state-class stmt-list s default-brk default-cont default-return default-throw))

      (else s))))

(define state-class
  (lambda (stmt s brk cont return throw)
    (state-add-binding (class-name stmt)
                       (class-closure (parent-class-name stmt)
                                      (instance-field-names (body stmt))
                                      (static-functions (body stmt)
                                                        (class-name stmt))
                                      (instance-functions (body stmt)
                                                          (class-name stmt))
                                      (constructors (body stmt)
                                                    (class-name stmt)))
                       s)))

(define class-name (lambda (stmt) (cadr stmt)))
(define extends-clause (lambda (stmt) (caddr stmt)))
(define parent-class-name
  (lambda (stmt)
    (if (null? (extends-clause stmt))
      '()
      (cadr (extends-clause stmt)))))
(define body (lambda (stmt) (cadddr stmt)))  

(define instance-field-names
  (lambda (body)
    (if (null? body)
      body
      (let* ([stmt (car body)]
             [key (keyword stmt)])
        (if (eq? key 'var)
          (cons (varname stmt) (instance-field-names (cdr body)))
          (instance-field-names (cdr body)))))))

(define static-functions
  (lambda (body current-type)
    (get-functions body
                   (state-empty)
                   'static-function
                   current-type)))

(define instance-functions
  (lambda (body current-type)
    (get-functions body
                   (state-empty)
                   'function
                   current-type)))

(define get-functions
  (lambda (body state signifier current-type)
    (if (null? body)
      state  
      (let* ([stmt (car body)]
             [key (keyword stmt)])
        (if (eq? key signifier)
          (state-function-declaration stmt
                                      (get-functions (cdr body)
                                                     state
                                                     signifier
                                                     current-type)
                                      current-type)
          (get-functions (cdr body)
                         state
                         signifier
                         current-type))))))

(define constructors
 (lambda (body classname)
  (list
   (lambda (state)
    (instance-closure classname
     (top-layer-values (state-class-vars body
                                         state
                                         classname)))))))
  
;; Field functions -- fields stored so that parent class field names come before subclass field names
(define field-lookup
  (lambda (name current-type iclosure state)
    (let* ([cclosure (state-lookup current-type state current-type)]
           [fields (class-instance-field-names cclosure)]
           [index (get-field-index name fields cclosure -1 current-type)])
      (if (eq? -1 index)
          (raise 'illegal-var-dereferencing)
          (field-value index (instance-field-values iclosure))))))

(define field-update
 (lambda (name current-type iclosure state new-val)
  (let* ([cclosure (state-lookup current-type state current-type)]
         [fields (class-instance-field-names cclosure)]
         [index (get-field-index name fields cclosure -1 current-type)])
      (if (eq? -1 index)
          (raise 'illegal-var-assignment)
          (begin
            (field-set index (instance-field-values iclosure) new-val)
            state)))))

 ; Helper used in trick to determine which field value to select
(define get-field-index
  (lambda (name fields cclosure acc current-type)
    (cond
      ((and (null? fields)
            (null? (class-parent-name cclosure)))
        acc)
      ((null? fields)
        (get-field-index name
                         (class-instance-field-names (class-parent cclosure state current-type))
                         (class-parent cclosure state current-type)
                         acc current-type))
      ((or (>= acc 0)
           (eq? name (car fields)))
        (get-field-index name (cdr fields) cclosure (+ acc 1) current-type))
      (else
        (get-field-index name (cdr fields) cclosure acc current-type)))))

(define field-box
 (lambda (index instanceFields)
  (list-ref instanceFields index)))

(define field-set
 (lambda (index instanceFields new-val)
  (set-box! (field-box index instanceFields) new-val)))

(define field-value
  (lambda (index instanceFields)
    (if (null? (unbox (field-box index instanceFields)))
      (raise 'unset-instance-field)
      (unbox (field-box index instanceFields)))))
