; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define lexical-address
  (lambda (expanded-parsed-exp)
    (let helper ([exp expanded-parsed-exp] [scope-list '()])
      (if (null? exp)
        exp
        (cases expression exp
          [lit-exp (id) exp]
          [var-exp (id) (var-exp (get-lex-add (cadr exp) scope-list))]
          [lambda-exp (ids bodies)
            (let ([scope-list (cons (cadr exp) scope-list)])
              (lambda-exp
                  (cadr exp)
                  (map (lambda (body) (helper body scope-list)) (caddr exp))))]
          [if-exp (condition then else)
            (if-exp
              (helper condition scope-list)
              (helper then scope-list)
              (helper else scope-list))]
          [set!-exp (id expression)
            (set!-exp
              (helper (var-exp (cadr exp)) scope-list)
              (helper (caddr exp) scope-list))]
          [app-exp (operator operands)
            (app-exp
              (helper operator scope-list)
              (map (lambda (rand) (helper rand scope-list)) operands))]
          [define-exp (id expression)
            (define-exp
              (helper (var-exp (cadr exp)) scope-list)
              (helper (caddr exp) scope-list))]
          [else (map (lambda (arg) (helper arg scope-list)) exp)])))))

(define get-lex-add
    (lambda (var scope-list)
        (let helper ([scope-list scope-list] [depth 0])
            (if (null? scope-list)
                (list ': 'free var)
                (if (ormap (lambda (arg) (eqv? var arg)) (car scope-list))
                    (let bound-lex-add ([args (car scope-list)] [pos 0])
                        (if (eqv? (car args) var)
                            (list ': depth pos)
                            (bound-lex-add (cdr args) (+ pos 1))))
                    (helper (cdr scope-list) (+ depth 1)))))))

(define parse-exp         
  (lambda (datum)
    (cond
      ; literal number
      [(number? datum) (lit-exp datum)]

      ; literal string
      [(string? datum) (lit-exp datum)]

      ; literal boolean
      [(boolean? datum) (lit-exp datum)]

      ; literal vector
      [(vector? datum) (lit-exp datum)]

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
                  (parse-exp '(void))
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

          ; begin expression
          [(eqv? (car datum) 'begin)
            (if (null? (cdr datum))
              (begin-exp (list (parse-exp '(void))))
              (begin-exp
                (map parse-exp (cdr datum))))]

          ; cond expression
          [(eqv? (car datum) 'cond)
            (if (null? (cdr datum))
              (eopl:error 'parse-exp "Invalid 'cond' Expression: Insufficient Length in ~s" datum)
              (cond-exp
                (map 
                  (lambda (clause) 
                    (cond-clause 
                      (parse-exp (car clause)) 
                      (map parse-exp (cdr clause))))
                  (cdr datum))))]

          ; and expression
          [(eqv? (car datum) 'and)
            (if (null? (cdr datum))
              (and-exp (list (lit-exp #t)))
              (and-exp (map parse-exp (cdr datum))))]

          ; or expression
          [(eqv? (car datum) 'or)
            (if (null? (cdr datum))
              (or-exp (list (lit-exp #f)))
              (or-exp (map parse-exp (cdr datum))))]

          ; while loop expression
          [(eqv? (car datum) 'while)
            (if (null? (cdr datum))
              (eopl:error 'parse-exp "Invalid 'while' expression ~s: Insufficient length" datum)
              (if (null? (cddr datum))
                (while-exp (parse-exp (cadr datum)) (list (parse-exp '(void))))
                (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))))]

          [(eqv? (car datum) 'define)
            (if (null? (cdr datum))
              (eopl:error 'parse-exp "Invalid 'define' expression ~s: Insufficient length" datum)
              (define-exp 
                (cadr datum) 
                (if (null? (cddr datum))
                  (parse-exp '(void))
                  (parse-exp (caddr datum)))))]

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
        (if (equal? '(app-exp (var-exp void) ()) else)
          (list 'if (unparse-exp condition) (unparse-exp then))
          (list 'if (unparse-exp condition) (unparse-exp then) (unparse-exp else)))]
      [set!-exp (id expression) (list 'set! id (unparse-exp expression))]
      [begin-exp (expressions) (append (list 'begin) (map unparse-exp expressions))]
      [cond-exp (clauses) (append (list 'cond) (map unparse-exp clauses))]
      [cond-clause (test bodies) (append (list (unparse-exp test)) (map unparse-exp bodies))]
      [and-exp (clauses) (append (list 'and) (map unparse-exp clauses))]
      [or-exp (clauses) (append (list 'or) (map unparse-exp clauses))]
      [while-exp (test bodies) (append (list 'while (unparse-exp test)) (map unparse-exp bodies))]
      [define-exp (id expression) (list 'define id (unparse-exp expression))]
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











