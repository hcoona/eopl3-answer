#lang racket
(require rackunit
         "Exercise_2-15.rkt")

(check-equal? (var-exp 'a) 'a)
(check-equal? (lambda-exp (var-exp 'a) (var-exp 'a))
              '(lambda (a) a))
(check-equal? (lambda-exp (var-exp 'a)
                          (lambda-exp (var-exp 'b)
                                      (app-exp (app-exp (var-exp '+)
                                                        'a)
                                               'b)))
              '(lambda (a) (lambda (b) ((+ a) b))))

(check-true  (var-exp? (var-exp 'a)))
(check-false (var-exp? (lambda-exp (var-exp 'a) (var-exp 'a))))
(check-false (var-exp? (app-exp (var-exp '-) (var-exp 'a))))

(check-true  (lambda-exp? (lambda-exp (var-exp 'a) (var-exp 'a))))
(check-false (lambda-exp? (var-exp 'a)))
(check-false (lambda-exp? (app-exp (var-exp '-) (var-exp 'a))))

(check-true  (app-exp? (app-exp (var-exp '-) (var-exp 'a))))
(check-false (app-exp? (var-exp 'a)))
(check-false (app-exp? (lambda-exp (var-exp 'a) (var-exp 'a))))

(check-equal? (var-exp->var (var-exp 'a)) 'a)
(check-equal? (lambda-exp->bound-var (lambda-exp (var-exp 'a) (var-exp 'b))) 'a)
(check-equal? (lambda-exp->body (lambda-exp (var-exp 'a) (var-exp 'b))) 'b)
(check-equal? (app-exp->rator (app-exp (var-exp '-) (var-exp 'a))) '-)
(check-equal? (app-exp->rand (app-exp (var-exp '-) (var-exp 'a))) 'a)