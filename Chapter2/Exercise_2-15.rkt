#lang eopl

(provide var-exp
         lambda-exp
         app-exp)
(provide var-exp?
         lambda-exp?
         app-exp?)
(provide var-exp->var
         lambda-exp->bound-var
         lambda-exp->body
         app-exp->rator
         app-exp->rand)

(define (var-exp var) var)
(define (lambda-exp var lc-exp)
  `(lambda (,var) ,lc-exp))
(define (app-exp e1 e2)
  `(,e1 ,e2))

(define (var-exp? e) (symbol? e))
(define (lambda-exp? e) (and (not (var-exp? e)) (eq? (car e) 'lambda)))
(define (app-exp? e) (and (not (var-exp? e)) (not (lambda-exp? e))))

(define (var-exp->var e) e)
(define (lambda-exp->bound-var e) (caadr e))
(define (lambda-exp->body e) (caddr e))
(define (app-exp->rator e) (car e))
(define (app-exp->rand e) (cadr e))