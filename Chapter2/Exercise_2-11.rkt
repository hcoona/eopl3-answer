#lang eopl

(require "chap2-common.rkt")

(provide empty-env
         empty-env?
         extend-env
         extend-env*
         apply-env)

; Exercise 2.11
(define (empty-env) '())
(define (empty-env? env) (null? env))
(define (extend-env var val env)
  (cons (cons (list var) (list val)) env))
(define (extend-env* vars vals env)
  (cons (cons vars vals) env))
(define (apply-env env search-var)
  (if (empty-env? env)
      (report-no-binding-found search-var)
      (let ([saved-vars (caar env)]
            [saved-vals (cdar env)]
            [saved-env (cdr env)])
        (let ([search-rib-rst (let search-rib ([vars saved-vars]
                                               [vals saved-vals])
                                (cond [(null? vars) #f] ; Assume vars and vals always have same number of elements
                                      [(eq? search-var (car vars)) (car vals)]
                                      [else (search-rib (cdr vars) (cdr vals))]))])
          (if (eq? #f search-rib-rst)
              (apply-env (cdr env) search-var)
              search-rib-rst)))))