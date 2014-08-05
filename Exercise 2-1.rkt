#lang racket
(require rackunit)

; Would need 512MB memory.

(define N (expt 2 4))

(define (zero) null)
(define (is-zero? n) (null? n))
(define (succ n)
  (cond
    [(is-zero? n) '(1)]
    [else
     (let ([r (add1 (car n))])
       (if (= N r)
           (cons 0 (succ (cdr n)))
           (cons r (cdr n))))]))
(define (pred n)
  (let ([lsb (car n)])
    (cond
      [(= 0 lsb) (cons (sub1 N) (pred (cdr n)))]
      [(and (= 1 lsb) (null? (cdr n))) null]
      [else (cons (sub1 (car n)) (cdr n))])))

(define (plus x y)
  (if (is-zero? x)
      y
      (succ (plus (pred x) y))))
(define (mult x y)
  (if (or (is-zero? x) (is-zero? y))
      0
      (let loop ([x x] [sum (zero)])
        (if (is-zero? x)
            sum
            (loop (pred x) (plus sum y))))))

(define (factorial n)
  (if (is-zero? n)
      (succ (zero))
      (mult n (factorial (pred n)))))

(define (apply-n n f . args)
  (if (= n 0)
      (car args)
      (apply-n (sub1 n) f (apply f args))))
(define (number->bignum n)
  (apply-n n succ (zero)))

(define bn:2 (number->bignum 2))
(define bn:4 (number->bignum 4))
(define bn:8 (number->bignum 8))
(define bn:10 (number->bignum 10))
(define bn:16 (number->bignum 16))

(check-equal?
 (factorial bn:10)
 (number->bignum (foldl * 1 (range 1 (add1 10)))))

; Q: How does the execution time vary as this argument changes?
; A: Increasingly

; Q: How does the execution time vary as the base changes?
; A: Almost don't change. Due to the low efficient `plus` method.