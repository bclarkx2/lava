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


;example abstraction
(define operator
  (lambda (e)
    (car e)))
