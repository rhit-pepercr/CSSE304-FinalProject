(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (ids (list-of symbol?))
   (bodies (list-of expression?))]
  [lambda-n-exp
    (id symbol?)
    (bodies (list-of expression?))]
  [lambda-imp-exp
    (ids (list-of symbol?))
    (opt-id symbol?)
    (bodies (list-of expression?))]
  [let-binding-exp
    (id symbol?)
    (binding expression?)]
  [let-exp
    (bindings (list-of expression?))
    (bodies (list-of expression?))]
  [let*-exp 
    (bindings (list-of let-binding))
    (bodies (list-of expression?))]
  [app-exp
   (operator expression?)
   (operands (list-of expression?))]
  [lit-exp
    (id number?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(trace-define parse-exp         
  (lambda (datum)
    (cond
      ; variable expression
      [(symbol? datum) (var-exp datum)]

      ; literal expression
      [(number? datum) (lit-exp datum)]

      [(pair? datum)
        (cond

          ; lambda expressions
          [(eqv? (car datum) 'lambda)
            (if (list? (2nd datum)) 
              ; normal lambda expression
              (lambda-exp 
                (2nd  datum)
                (map parse-exp (cddr datum)))
              (if (pair? (2nd datum)) 
                ; improper params lambda expression
                (let ([split-list (split-imp (2nd datum))])
                  (lambda-imp-exp
                    (car split-list)
                    (cadr split-list)
                    (map parse-exp (cddr datum))))
                ; arbitrary param count lambda expression
                (lambda-n-exp
                  (2nd datum)
                  (map parse-exp (cddr datum)))))]

          ; let expression
          [(eqv? (car datum) 'let)
            (let-exp 
              (map (lambda (binding) (let-binding-exp (car binding) (parse-exp (cadr binding)))) (2nd datum))
              (map parse-exp (cddr datum)))]

          ; app expression
          [else (app-exp (parse-exp (1st datum))
		        (map parse-exp (cdr datum)))])]

      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (parsed-exp)
    (cases expression parsed-exp
      [var-exp (id) id]
      [lambda-exp (ids bodies) (append (list 'lambda ids) (map unparse-exp bodies))]
      [lambda-n-exp (id bodies) (append (list 'lambda id) (map unparse-exp bodies))]
      [lambda-imp-exp (ids opt-id bodies) (append (list 'lambda (append ids opt-id)) (map unparse-exp bodies))]
      [let-exp (bindings bodies) 
        (append 
          (list 'let (map unparse-exp bindings))
          (map unparse-exp bodies))]
      [let-binding-exp (id binding) (list id (unparse-exp binding))]
      [let*-exp (bindings bodies) 'nyi]
      [app-exp (operator operands) (cons (unparse-exp operator) (map unparse-exp operands))]
      [lit-exp (id) id]))) 

; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
;(var-exp? (var-exp 'a))
;(var-exp? (app-exp (var-exp 'a) (var-exp 'b)))

(define split-imp
  (lambda (imp-list)
    (let helper ([imp-list imp-list] [acc '()])
      (if (symbol? (cdr imp-list))
        (list (reverse (cons (car imp-list) acc)) (cdr imp-list))
        (helper (cdr imp-list) (cons (car imp-list) acc))))))






