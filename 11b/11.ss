; Assignment 11a by Connor Peper
; Teammates- Jaden Rigg and Sam Munro

(load "chez-init.ss")

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))
   
   ; Problem 1



(define-syntax my-let
  (syntax-rules ()
    [(_ ((x v) ...) e1 e2 ...)
      ( (lambda (x ...) e1 e2 ...) 
      v ...)]

    [(_ name ((x v) ...) e1 e2 ...)
      (letrec ((name (lambda (x ...)  e1 e2 ...)
      )) (name v ...))]))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]
    [(_ exp1 exp2 ...)
     (let ([prev exp1])
       (if prev
       (if (number? prev)
           prev
           #t)
       (my-or exp2 ...)))]))

  (define-syntax +=
    (syntax-rules ()
      [(_ e exp1) (begin (set! e (+ e exp1)) e)]))

  (define-syntax return-first
    (syntax-rules ()
      [(_ exp1 exp2 ...) exp1]))


   ; Problem 2

(define bintree-to-list (lambda (T)
  (cases bintree T
    [leaf-node (datum) (cons 'leaf-node (list datum))]
    [interior-node (key left right)
      (append (list 'interior-node key) (cons (bintree-to-list left) (list (bintree-to-list right))))]))) 


; Problem 3

(define max-interior (lambda (T)
  (cadr (max-interior-helper T))))


(define max-interior-helper (lambda (T)
  (cases bintree T
    [leaf-node (datum) datum]
    [interior-node (key left right)
      (cond
        ; If both interior-nodes
        [(and (eqv? 'interior-node (car left)) (eqv? 'interior-node (car right)))
         ; (display "Case 1")
          (let ([leftTree (max-interior-helper left)] [rightTree (max-interior-helper right)])
            (list (car (max-tree leftTree (max-tree rightTree
              (list (+ (car leftTree) (car rightTree)) key))))
                (cadr (max-tree leftTree (max-tree rightTree (list (+ (caddr leftTree) (caddr rightTree)) key))))
                  (+ (caddr leftTree) (caddr rightTree))))]
      
        ; If left is interior-node and right is a leaf-node
        [(eqv? 'interior-node (car left))
        ;  (display "Case 2")
          (let ([leftTree (max-interior-helper left)] [rightTree (max-interior-helper right)])
            (list (car (max-tree leftTree (list (+ (car leftTree) rightTree) key)))
              (cadr (max-tree leftTree (list (+ (caddr leftTree) rightTree) key)))
                (+ (caddr leftTree) rightTree)))]
        ; If left is a leaf-node and right is an interior-node
        [(eqv? 'interior-node (car right))
      ;    (display "Case 3")
          (let ([rightTree (max-interior-helper right)] [leftTree (max-interior-helper left)])
            (list (car (max-tree rightTree (list (+ (car rightTree) leftTree) key)))
              (cadr (max-tree rightTree (list (+ (caddr rightTree) leftTree) key)))
                (+ (caddr rightTree) leftTree)))]
        [#t (list (+ (max-interior-helper right) (max-interior-helper left)) key (+ (max-interior-helper right) (max-interior-helper left)))])])))



  (define max-tree (lambda (T1 T2)
    (if (>= (car T1) (car T2))
      T1
      T2)))