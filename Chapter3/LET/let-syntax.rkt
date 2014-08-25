(module let-syntax eopl
  (provide (all-defined-out))
  
  (define identifier? symbol?)
  
  ;; Page 69
  (define-datatype program program?
    [a-program
     (exp1 expression?)])
  
  (define-datatype expression expression?
    [const-exp
     (num number?)]
    [diff-exp
     (exp1 expression?)
     (exp2 expression?)]
    [zero?-exp
     (exp1 expression?)]
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