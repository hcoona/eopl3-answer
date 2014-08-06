#lang eopl
(require "chap2-common.rkt")

; Exercise 2.5 [*]
(define empty-env (lambda () '()))
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))
(define apply-env
  (lambda (env search-var)
    (let ([a-pair (assoc search-var env)])
      (if (eq? #f a-pair)
          (report-no-binding-found search-var)
          (cdr a-pair)))))

; Exercise 2.8 [*]
(define (empty-env? env) (null? env))

; Exercise 2.9 [*]
(define (has-binding? env search-var)
  (pair? (assoc search-var env)))

; Exercise 2.10 [*]
(define (extend-env* vars vals env)
  (cond [(null? vars)
         (cond [(null? vals) env]
               [else
                eopl:error 'extend-env* "vars & vals have different length."])]
        [else
         (cond [(null? vals)
                eopl:error 'extend-env* "vars & vals have different length."]
               [else
                (extend-env* (cdr vars)
                             (cdr vals)
                             (extend-env (car vars) (car vals) env))])]))
