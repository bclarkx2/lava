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
 (lambda (a) '()))

(define M.value.int
  (lambda (e)
    (cond
      ((number? e) e)
      ((eq? '+ (operator e)) (+ (M.value.int(cadr e)) (M.value.int(caddr e))))
      ((eq? '- (operator e)) (- (M.value.int(cadr e)) (M.value.int(caddr e))))
      ((eq? '* (operator e)) (* (M.value.int(cadr e)) (M.value.int(caddr e))))
      ((eq? '/ (operator e)) (quotient (M.value.int(cadr e)) (M.value.int(caddr e))))
      ((eq? '% (operator e)) (remainder (M.value.int(cadr e)) (M.value.int(caddr e))))
      (else (error 'badop "Undefine operator")))))


;abstract ideas in the above by defining little things like this
(define operator
  (lambda (e)
    (car e)))



;;; Bindings

(define state.add-binding
 (lambda (a b c) '()))

(define state.remove-binding
 (lambda (a b c) '()))



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
