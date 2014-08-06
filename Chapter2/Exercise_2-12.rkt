#lang eopl

; Exercise 2.12 [*]
(define (empty-stack)
  (lambda (op)
    (cond
      [(eq? op 'top)
       (eopl:error 'top "Empty Stack")]
      [(eq? op 'pop)
       (eopl:error 'pop "Empty Stack")]
      [(eq? op 'empty?) #t])))

(define (empty-stack? stack) (stack 'empty?))

(define (push value stack)
  (lambda (op)
    (cond
      [(eq? op 'top) value]
      [(eq? op 'pop) stack]
      [(eq? op 'empty?) #f])))

(define (pop stack) (stack 'pop))

(define (top stack) (stack 'top))