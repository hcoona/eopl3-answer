#lang eopl

(require "chap2-common.rkt")

(define (empty-env)
  (list (lambda (search-var)
          (report-no-binding-found search-var))
        (lambda () #t)))

(define (empty-env? env)
  ((cadr env)))

(define (extend-env saved-var saved-val saved-env)
  (list (lambda (search-var)
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var)))
        (lambda () #f)))

(define (apply-env env search-var)
  ((car env) search-var))