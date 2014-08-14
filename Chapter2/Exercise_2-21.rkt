#lang eopl

; Env = (empty-env) | (extend-env Var SchemeVal Env)
; Var = Sym

; empty-env  : () -> Env
; extend-env : Var, SchemeVal, Env -> Env 
; apply-env  : Env, Var -> SchemeVal
; has-binding? : Env, Var -> bool

(define-datatype env env?
  [empty-env]
  [extend-env
   (var symbol?)
   (val (lambda (x) #t))
   (env env?)])

(define apply-env
  (lambda (search-env search-var)
    (cases env search-env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-var)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))])))

(define (has-binding? search-env search-var)
  (cases env search-env
    [empty-env () #f]
    [extend-env (saved-var saved-val saved-env)
                (if (eqv? search-var saved-var)
                    #t
                    (has-binding? saved-env search-var))]))