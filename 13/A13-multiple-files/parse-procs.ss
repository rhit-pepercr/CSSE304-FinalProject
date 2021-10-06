; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
      ; literal number
      [(number? datum) (lit-exp datum)]

      ; literal string
      [(string? datum) (lit-exp datum)]

      ; literal boolean
      [(boolean? datum) (lit-exp datum)]

      ; variable expression
      [(symbol? datum) (var-exp datum)]

      [(pair? datum)
        (cond
          [(not (list? datum)) (eopl:error 'parse-exp "Improper list ~s" datum)]

          ; literal symbol and list
          [(eqv? (car datum) 'quote)
            (lit-exp datum)]
                
          ; lambda expressions
          [(eqv? (car datum) 'lambda)
            (cond
              [(or (null? (cdr datum)) (null? (cddr datum))) 
                (eopl:error 'parse-exp "Invalid Lambda Expression ~s: Missing Parameter(s)" datum)]
              [(and (not (null? (2nd datum))) (not (pair? (2nd datum))) (not (symbol? (2nd datum))))
                (eopl:error 'parse-exp "Invalid Lambda Expression ~s: Missing Parameters" datum)])


            (if (list? (2nd datum)) 
              ; normal lambda expression
              (if (andmap symbol? (2nd datum))
                (lambda-exp 
                  (2nd  datum)
                  (map parse-exp (cddr datum)))
                (eopl:error 'parse-exp "Invalid Lambda Expression: Formal arguments ~s must all be symbols" (2nd datum)))
              (if (pair? (2nd datum)) 
                ; improper params lambda expression
                (let ([split-list (split-imp (2nd datum))])
                  (if (and (andmap symbol? (car split-list)) (symbol? (cadr split-list)))
                    (lambda-imp-exp
                      (car split-list)
                      (cadr split-list)
                      (map parse-exp (cddr datum)))
                  (eopl:error 'parse-exp "Invalid Lambda Expression: Formal arguments ~s must all be symbols" (2nd datum))))
                ; arbitrary param count lambda expression
                (lambda-n-exp
                  (2nd datum)
                  (map parse-exp (cddr datum)))))]

          ; let expressions
          [(eqv? (car datum) 'let)
              (if (symbol? (2nd datum))
                ; named let expression
                (begin
                  (let-error-check 'let (cdr datum))
                  (named-let-exp
                    (2nd datum)
                    (map (lambda (binding) (let-binding-exp (car binding) (parse-exp (cadr binding)))) (3rd datum))
                    (map parse-exp (cdddr datum))))
                ; regular let expression
                (begin 
                  (let-error-check 'let datum)
                  (let-exp 
                    (map (lambda (binding) (let-binding-exp (car binding) (parse-exp (cadr binding)))) (2nd datum))
                    (map parse-exp (cddr datum)))))]

          ; let* expression
          [(eqv? (car datum) 'let*)
            (begin 
              (let-error-check 'let* datum)
              (let*-exp 
                (map (lambda (binding) (let-binding-exp (car binding) (parse-exp (cadr binding)))) (2nd datum))
                (map parse-exp (cddr datum))))]

          ; letrec expression
          [(eqv? (car datum) 'letrec)
            (begin 
              (let-error-check 'letrec datum)
              (letrec-exp 
                (map (lambda (binding) (let-binding-exp (car binding) (parse-exp (cadr binding)))) (2nd datum))
                (map parse-exp (cddr datum))))]

          ; if expression
          [(eqv? (car datum) 'if)
            (if (and (not (null? (cdr datum))) (not (null? (cddr datum))))
              (if-exp
                (parse-exp (2nd datum))
                (parse-exp (3rd datum))
                (if (null? (cdddr datum))
                  (parse-exp 'void)
                  (if (null? (cddddr datum))
                    (parse-exp (cadddr datum))
                    (eopl:error 'parse-exp "Invalid 'if' Expression: Excess Length in ~s" datum))))
              (eopl:error 'parse-exp "Invalid 'if' Expression: Insufficient Length in ~s" datum))]

          ; set! expression
          [(eqv? (car datum) 'set!)
            (cond
              [(or (null? (cdr datum)) (null? (cddr datum)))
                (eopl:error 'parse-exp "Invalid 'set!' Expression: Insufficient Length in ~s" datum)]
              [(not (null? (cdddr datum)))
                (eopl:error 'parse-exp "Invalid 'set!' Expression: Excess Length in ~s" datum)]
              [(not (symbol? (2nd datum)))
                (eopl:error 'parse-exp "Invalid 'set!' Expression: Variable ~s to mutate is not a symbol" (2nd datum))]
              [#t 
                (set!-exp
                  (2nd datum)
                  (parse-exp (3rd datum)))])]

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
      [let*-exp (bindings bodies) 
        (append 
          (list 'let* (map unparse-exp bindings))
          (map unparse-exp bodies))]
      [letrec-exp (bindings bodies) 
        (append 
          (list 'letrec (map unparse-exp bindings))
          (map unparse-exp bodies))]
      [named-let-exp (name bindings bodies)
        (append 
          (list 'let name (map unparse-exp bindings))
          (map unparse-exp bodies))]
      [if-exp (condition then else) 
        (if (equal? '(var-exp void) else)
          (list 'if (unparse-exp condition) (unparse-exp then))
          (list 'if (unparse-exp condition) (unparse-exp then) (unparse-exp else)))]
      [set!-exp (id value) (list 'set! id (unparse-exp value))]
      [app-exp (operator operands) (cons (unparse-exp operator) (map unparse-exp operands))]
      [lit-exp (id) id]))) 

(define split-imp
  (lambda (imp-list)
    (let helper ([imp-list imp-list] [acc '()])
      (if (not (pair? (cdr imp-list)))
        (list (reverse (cons (car imp-list) acc)) (cdr imp-list))
        (helper (cdr imp-list) (cons (car imp-list) acc))))))

(define let-error-check
  (lambda (type datum)
    (if (or (null? (cdr datum)) (null? (cddr datum)))
      (eopl:error 'parse-exp "Invalid '~s' Expression ~s: Missing Parameter(s)" type datum)
      (cond
        [(not (list? (2nd datum)))
          (eopl:error 'parse-exp "Invalid '~s' Expression: Declarations ~s not a List" type (2nd datum))]
        [(not (andmap list? (2nd datum)))
          (eopl:error 'parse-exp "Invalid '~s' Expression: Declarations ~s not Lists" type (2nd datum))]
        [(not (andmap symbol? (map car (2nd datum))))
          (eopl:error 'parse-exp "Invalid '~s' Expression: Variable Declarations in ~s not all symbols" type (2nd datum))]
        [(not (andmap (lambda (dec) (and (not (null? (cdr dec))) (null? (cddr dec)))) (2nd datum)))
          (eopl:error 'parse-exp "Invalid '~s' Expression: Variable Declarations in ~s not all length-2" type (2nd datum))]))))











