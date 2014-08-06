#lang eopl

(require "chap2-common.rkt")

(provide empty-env
         empty-env?
         extend-env
         apply-env
         has-binding?)

(define (empty-env)
  (list [lambda (search-var)
          (report-no-binding-found search-var)]
        [lambda () #t]
        [lambda (_) #f]))

(define (empty-env? env)
  ((cadr env)))

(define (extend-env saved-var saved-val saved-env)
  (list [lambda (search-var)
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var))]
        [lambda () #f]
        [lambda (search-var)
          (if (eqv? search-var saved-var)
              #t
              (has-binding? saved-env search-var))]))

(define (apply-env env search-var)
  ((car env) search-var))

(define (has-binding? env search-var)
  ((caddr env) search-var))