
;;; Continuation structurs

(define mk-conts
 (lambda (brk continue return throw)
  (list brk continue return throw)))

(define get-brk (lambda (conts) (car conts)))
(define get-cont (lambda (conts) (cadr conts)))
(define get-ret (lambda (conts) (caddr conts)))
(define get-throw (lambda (conts) (cadddr conts)))

(define set-brk
 (lambda (conts brk)
  (cons brk (cdr conts))))

(define set-cont
 (lambda (conts continue)
  (append (list (get-brk conts))
          (cons continue (cddr conts)))))

(define set-ret
 (lambda (conts return)
  (append (list (get-brk conts) (get-cont conts))
          (cons return (get-throw conts)))))

(define set-throw
 (lambda (conts throw)
  (append (list (get-brk conts) (get-cont conts) (get-ret conts))
          (list throw))))
