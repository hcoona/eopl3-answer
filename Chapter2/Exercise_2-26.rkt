#lang eopl

(define-datatype red-blue-tree red-blue-tree?
  [leaf-node (num integer?)]
  [red-node
   (leftson red-blue-tree?)
   (rightson red-blue-tree?)]
  [blue-node
   (tree-list (lambda (lst)
                (let loop ([lst lst])
                  (cond
                    [(null? lst) #t]
                    [(pair? lst) (and (red-blue-tree? (car lst))
                                      (loop (cdr lst)))]
                    [else #f]))))])

(define (add1 n) (+ 1 n))

(define (mark-leaves-with-red-depth tree)
  (let loop ([tree tree]
             [red-depth 0])
    (cases red-blue-tree tree
      [leaf-node (_)
                 (leaf-node red-depth)]
      [red-node (leftson rightson)
                (red-node (loop leftson (add1 red-depth))
                          (loop rightson (add1 red-depth)))]
      [blue-node (childern)
                 (blue-node (map (lambda (t) (loop t red-depth))
                                 childern))])))

(define t
  (red-node
   (blue-node (list (leaf-node 26)
                    (leaf-node 12)))
   (red-node (leaf-node 11)
             (blue-node (list (leaf-node 117)
                              (leaf-node 14))))))

; (mark-leaves-with-red-depth t)