#lang eopl

; Exercise 2.4 [**]
; empty-stack? & top are observer, while others are constructors.
(define (empty-stack) '())
(define (empty-stack? stack) (null? stack))
(define (push stack value)
  (cons value stack))
(define (pop stack) (cdr stack))
(define (top stack) (car stack))
