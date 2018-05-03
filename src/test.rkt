(define multi
 (lambda (x)
  (call/cc (lambda (return)
    (return x x)))))
