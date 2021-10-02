(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (id (list-of symbol?))
   (body expression?)]
  [app-exp
   (operator expression?)
   (operand (list-of expression?))]
  [lit-exp
    (id number?)])

   (define-datatype symbol-exp symbol-exp?
    [symbol-symbol-exp
      (data symbol?)]
    [s-list-symbol-exp
      (data s-list?)])

    (define-datatype s-list s-list?
      [an-s-list
        (data (list-of symbol-exp?))])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? (car datum) 'lambda)
	(lambda-exp (2nd  datum)
		    (parse-exp (3rd datum)))]

      [else (app-exp (parse-exp (1st datum))
		     (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (parsed-exp)
    (display parsed-exp)
    (newline)
    (cases expression parsed-exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id body) (list 'lambda id (unparse-exp body))]
      [app-exp (operator operand) 
        (list (cons (unparse-exp operator) (map unparse-exp operand)))])))



; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
;(var-exp? (var-exp 'a))
;(var-exp? (app-exp (var-exp 'a) (var-exp 'b)))









