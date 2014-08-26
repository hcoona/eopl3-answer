(module let-interp eopl
  (require "../environment.rkt"
           "let-syntax.rkt"
           "let-expval.rkt"
           "let-parser.rkt")
  
  (provide run
           value-of-program
           value-of)
  
  ;; Page 71
  (define (run string)
    (value-of-program (scan&parse string)))
  
  (define (value-of-program pgm)
    (cases program pgm
      [a-program (exp1)
                 (value-of exp1 (empty-env))]))
  
  (define (value-of exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [let-exp (var exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body
                           (extend-env var val1 env)))]
      [apply-exp (func exp-list)
                 (let ([val-list (map (lambda (exp)
                                        (value-of exp env))
                                      exp-list)]
                       [val-length (length exp-list)])
                   (define (car/num val) (expval->num (car val)))
                   (define (cadr/num val) (expval->num (cadr val)))
                   (define (car/any val) (expval->any (car val)))
                   (define (cadr/list val) (expval->list (cadr val)))
                   (cond
                     [(eqv? '+ func) (if (eq? 2 val-length)
                                         (num-val (+ (car/num val-list) (cadr/num val-list)))
                                         (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? '- func) (if (eq? 2 val-length)
                                         (num-val (- (car/num val-list) (cadr/num val-list)))
                                         (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? '* func) (if (eq? 2 val-length)
                                         (num-val (* (car/num val-list) (cadr/num val-list)))
                                         (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? '/ func) (if (eq? 2 val-length)
                                         (num-val (/ (car/num val-list) (cadr/num val-list)))
                                         (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? 'zero? func) (if (eq? 1 val-length)
                                             (bool-val (zero? (car/num val-list)))
                                             (report-arity-mismatch-error func 1 val-length))]
                     [(eqv? 'minus func) (if (eq? 1 val-length)
                                             (num-val (- (car/num val-list)))
                                             (report-arity-mismatch-error func 1 val-length))]
                     [(eqv? 'equal? func) (if (eq? 2 val-length)
                                              (bool-val (equal? (car/num val-list) (cadr/num val-list)))
                                              (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? 'greater? func) (if (eq? 2 val-length)
                                                (bool-val (> (car/num val-list) (cadr/num val-list)))
                                                (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? 'less? func) (if (eq? 2 val-length)
                                             (bool-val (< (car/num val-list) (cadr/num val-list)))
                                             (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? 'cons func) (if (eq? 2 val-length)
                                            (list-val (cons (car/any val-list) (cadr/list val-list)))
                                            (report-arity-mismatch-error func 2 val-length))]
                     [(eqv? 'car func) (if (eq? 1 val-length)
                                           (car val-list)
                                           (report-arity-mismatch-error func 1 val-length))]
                     [(eqv? 'cdr func) (if (eq? 1 val-length)
                                           (cdr val-list)
                                           (report-arity-mismatch-error func 1 val-length))]
                     [(eqv? 'null? func) (if (eq? 1 val-length)
                                             (bool-val (null? (expval->list val-list)))
                                             (report-arity-mismatch-error func 1 val-length))]
                     [(eqv? 'emptylist func) (if (eq? 0 val-length)
                                                 (list-val '())
                                                 (report-arity-mismatch-error func 0 val-length))]
                     [(eqv? 'list func) (list-val (map expval->any val-list))]))]))
  
  (define (report-arity-mismatch-error func expected given)
    (eopl:error 'value-of
                "~s: Arity mismatch, expected: ~s, given: ~s"
                func
                expected
                given))
  )