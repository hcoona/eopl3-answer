(module let-environment eopl
  (require "../environment.rkt")
  (require "let-expval.rkt")
  
  (provide init-env)
  (provide (all-from-out "../environment.rkt"))
  
  (define (init-env)
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env)))))
  )
