#lang eopl

(define-datatype bintree bintree?
  [leaf-node
   (num integer?)]
  [interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)])

(define (max-interior tree)
  (cases bintree tree
    [leaf-node (_)
               (eopl:error 'max-interior
                           "max-interior can only accept interior-node, but givin ~s"
                           tree)]
    [interior-node (_ __ ___)
                   (let ([max-key #f]
                         [max-val -inf.0])
                     (begin
                       (let traverse/sum ([tree tree])
                         (cases bintree tree
                           [leaf-node (n) n]
                           [interior-node (k l r)
                                          (let* ([l-sum (traverse/sum l)]
                                                 [r-sum (traverse/sum r)]
                                                 [sum (+ l-sum r-sum)])
                                            (begin
                                              (when (> sum max-val)
                                                (begin
                                                  (set! max-key k)
                                                  (set! max-val sum)))
                                              sum))]))
                       max-key))]))

(define t2 (interior-node 'bar
                          (leaf-node -1)
                          (interior-node 'foo
                                         (leaf-node 3)
                                         (leaf-node 2))))
(define t (interior-node 'zoo
                         (leaf-node 2)
                         t2))