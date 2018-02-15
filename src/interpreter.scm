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
   ((null? stmt) s)

   ; return statements do not alter state
   ((eq? (keyword stmt) 'return) s)

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
   (state (stmt1 stmt) s))
   (state (stmt2 stmt) s)))

(define condition (lambda (stmt) (car stmt)))
(define stmt1 (lambda (stmt) (cadr stmt)))
(define stmt2 (lambda (stmt) (caddr stmt)))

