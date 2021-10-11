; When you write syntax-expand for A14, this could be a good place to put your code.

(define syntax-expand
    (lambda (parsed-exp)
        (cases expression parsed-exp
            [var-exp (id) parsed-exp]
            [lambda-exp (ids bodies)
                (lambda-exp
                    ids
                    (map syntax-expand bodies))]
            [lambda-n-exp (id bodies)
                (lambda-n-exp
                    id
                    (map syntax-expand bodies))]
            [lambda-imp-exp (ids opt-id bodies)
                (lambda-imp-exp
                    ids
                    opt-id
                    (map syntax-expand bodies))]
            [let-binding-exp (id binding) parsed-exp] ; SHOULD NOT OCCUR
            [let-exp (bindings bodies)
                (app-exp
                    (lambda-exp 
                        (map cadr bindings)
                        (map syntax-expand bodies))
                    (map syntax-expand (map caddr bindings)))]
            [let*-exp (bindings bodies) 
                (syntax-expand
                    (car (let let-nest ([num-lets (length bindings)] [bindings bindings])
                        (if (zero? num-lets)
                            bodies
                            (list (let-exp (list (car bindings)) (let-nest (sub1 num-lets) (cdr bindings))))))))]

            [begin-exp (expressions)
                (syntax-expand
                    (let-exp
                        '()
                        expressions))]

            [cond-exp (clauses)
                (syntax-expand
                    (let if-nest ([num-ifs (length clauses)] [clauses clauses])
                        (if (zero? num-ifs)
                            (var-exp 'void)
                            (if (eqv? (car (cdadar clauses)) 'else)
                                (if-exp (lit-exp #t) (car (caddar clauses)) (if-nest (sub1 num-ifs) (cdr clauses)))
                                (if-exp (cadar clauses) (car (caddar clauses)) (if-nest (sub1 num-ifs) (cdr clauses)))))))]

            [cond-clause (test bodies) parsed-exp]

            [letrec-exp (bindings bodies) parsed-exp] ; TODO
            [named-let-exp (name bindings bodies) parsed-exp] ; TODO
            [app-exp (operator operands) parsed-exp] ; TODO
            [lit-exp (id) parsed-exp] 
            [if-exp (condition then else) parsed-exp] ; TODO
            [set!-exp (id value) parsed-exp]))) ; TODO