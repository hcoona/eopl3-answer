(module let-interp eopl
  (require "let-environment.rkt"
           "let-syntax.rkt"
           "let-expval.rkt"
           "let-parser.rkt")
  
  (provide run
           value-of-program
           value-of)
  
  ;; Page 71
  (define (run string)
    (value-of-program (scan&parse string)))
  
  (define (value-of-program pgm)
    (cases program pgm
      [a-program (exp1)
        (value-of exp1 (init-env))]))
           
  (define (value-of exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val (- num1 num2))))]
      [zero?-exp (exp1)
                 (let* ([val1 (value-of exp1 env)]
                        [num1 (expval->num val1)])
                   (if (zero? num1)
                       (bool-val #t)
                       (bool-val #f)))]
      [if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [let-exp (var exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body
                           (extend-env var val1 env)))]))
  
#|
  (run "let x = 7
        in let y = 2
           in let y = let x = -(x,1) in -(x,y)
              in -(-(x,8),y)")
|#
  )