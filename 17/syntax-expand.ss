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
            [let-binding-exp (id binding) 
                (let-binding-exp
                    id
                    (syntax-expand binding))]
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

            [letrec-exp (bindings bodies)
                (syntax-expand
                    (let-exp
                        (map (lambda (binding) (let-binding-exp (cadr binding) (lit-exp '()))) bindings)
                        (append
                            (map (lambda (binding) (set!-exp (cadr binding) (caddr binding))) bindings)
                            bodies)))]


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

            [cond-clause (test bodies) parsed-exp] ; SHOULD NOT OCCUR

            [and-exp (clauses)
                (syntax-expand 
                    (let if-nest ([num-ifs (length clauses)] [clauses clauses])
                        (cond 
                            [(zero? num-ifs) (lit-exp #t)]
                            [(= num-ifs 1) (car clauses)]
                            [#t 
                                (if-exp 
                                    (car clauses)
                                    (if-nest (sub1 num-ifs) (cdr clauses))
                                    (lit-exp #f))])))] 

            [or-exp (clauses)
                (syntax-expand 
                    (let if-nest ([num-ifs (length clauses)] [clauses clauses])
                        (if (zero? num-ifs)
                            (lit-exp #f)
                            (let-exp (list (let-binding-exp 'result (car clauses)))
                                (list (if-exp 
                                    (var-exp 'result)
                                    (var-exp 'result)
                                    (if-nest (sub1 num-ifs) (cdr clauses))))))))] 

            [named-let-exp (name bindings bodies) 
                (syntax-expand
                    (let-exp 
                        bindings
                        (list 
                            (letrec-exp 
                                (list (let-binding-exp name (lambda-exp (map cadr bindings) bodies)))
                                (list (app-exp (var-exp name) (map caddr bindings)))))))]

            [app-exp (operator operands) 
                (app-exp
                    (syntax-expand operator)
                    (map syntax-expand operands))]
            [lit-exp (id) parsed-exp]
            [if-exp (condition then else)
                (if-exp
                    (syntax-expand condition)
                    (syntax-expand then)
                    (syntax-expand else))]
            [set!-exp (id value) 
                (set!-exp
                    id
                    (syntax-expand value))]
            [while-exp (test bodies)
                (syntax-expand 
                    (named-let-exp
                        'loop
                        '()
                        (list 
                            (if-exp
                                test
                                (let-exp '() (append bodies (list (app-exp (var-exp 'loop) (list (lit-exp '()))))))
                                (var-exp 'void)))))])))