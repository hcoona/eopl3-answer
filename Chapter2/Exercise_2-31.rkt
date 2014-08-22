#lang eopl

(define-datatype prefix-exp prefix-exp?
  [const-exp (num integer?)]
  [diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)])

(define-syntax car!
  (syntax-rules ()
    [(_ l) (let ([rst (car l)])
             (begin
               (set! l (cdr l))
               rst))]))

(define (parse-prefix-exp datum)
  (let loop ()
    (cond
      [(pair? datum)
       (let ([fst (car! datum)])
         (cond
           [(eqv? '- fst)
            (let ([rand1 (loop)])
              (let ([rand2 (loop)])
                (diff-exp rand1 rand2)))]
           [(integer? fst) (const-exp fst)]
           [else (eopl:error 'parse-prefix-exp
                             "Expecting '-' but got ~s"
                             (car datum))]))]
      [else
       (eopl:error 'parse-prefix-exp
                   "Expecting an integer or a list, but got ~s"
                   datum)])))

; (parse-prefix-exp '(- - 3 2 - 4 - 12 7))