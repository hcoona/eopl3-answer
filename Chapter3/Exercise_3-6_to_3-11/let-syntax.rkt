(module let-syntax eopl
  (provide program
           program?
           a-program
           expression
           expression?
           const-exp
           apply-exp
           if-exp
           var-exp
           let-exp)
  
  (define identifier? symbol?)
  
  (define (func? symbol)
    (memv symbol
          '(+ - * / zero? minus equal? greater? less? cons car cdr null? emptylist list)))
  
  (define (expression-list? lst)
    (cond
      [(null? lst) #t]
      [(pair? lst) (and (expression? (car lst))
                        (expression-list? (cdr lst)))]
      [else
       (eopl:error 'expression-list?
                   "Can only accept list, but got ~s"
                   lst)]))
  
  ;; Page 69
  (define-datatype program program?
    [a-program
     (exp1 expression?)])
  
  (define-datatype expression expression?
    [const-exp
     (num number?)]
    [apply-exp
     (func func?)
     (exp-list expression-list?)]
    [if-exp
     (exp1 expression?)
     (exp2 expression?)
     (exp3 expression?)]
    [var-exp
     (var identifier?)]
    [let-exp
     (var identifier?)
     (exp1 expression?)
     (body expression?)])
  )