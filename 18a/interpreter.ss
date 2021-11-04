; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (cases expression form
      [define-exp (id expression) 
        (update-global-env 
          id
          (eval-exp expression (empty-env-record) (init-k)))]
      [app-exp (operator operands)
        (if (null? operands)
          (cases expression operator
            [lambda-exp (ids bodies)
              (if (null? ids)
                (for-each (lambda (body) (top-level-eval body)) bodies)
                (eval-exp form (empty-env-record) (init-k)))]
            [else (eval-exp form (empty-env-record) (init-k))])
          (eval-exp form (empty-env-record) (init-k)))]
      [else (eval-exp form (empty-env-record) (init-k))])))


; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum)
        (if (pair? datum)
          (if (eqv? (car datum) 'quote)
            (apply-k k (cadr datum)))
          (apply-k k datum))]

      [var-exp (id)
	      (apply-k k (apply-env env id))]

      [if-exp (condition then-exp else-exp)
        (eval-exp 
          condition
          env
          (if-k then-exp else-exp env k))]

      [app-exp (operator operands)
        (map-cps
          (lambda (operand k) (eval-exp operand env k))
          operands
          (app-k operator env k))]

      [lambda-exp (ids bodies)
        (apply-k k (closure ids bodies env))]
      
      [lambda-n-exp (id bodies)
        (apply-k k (n-closure id bodies env))]

      [lambda-imp-exp (ids opt-id bodies)
        (apply-k k (imp-closure ids opt-id bodies env))]

      [set!-exp (id expression)
        (eval-exp
          expression
          env
          (set!-k id env k))]

      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define split-for-imp
  (lambda (lst proper-len)
    (if (zero? proper-len)
      (list lst)
      (cons (car lst) (split-for-imp (cdr lst) (sub1 proper-len))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]

      [closure (ids bodies env)
        (let ([new-env (extend-env ids args env)])
          (eval-exp
            (car bodies)
            new-env
            (closure-k bodies new-env k)))]

      [n-closure (id bodies env)
        (let ([new-env (extend-env (list id) (list args) env)])
          (eval-exp
            (car bodies)
            new-env
            (closure-k bodies new-env k)))]

      [imp-closure (ids opt-id bodies env)
        (let* ([split-args (split-for-imp args (length ids))]
               [new-env (extend-env (append ids (list opt-id)) split-args env)])
          (eval-exp
            (car bodies)
            new-env
            (closure-k bodies new-env k)))]

      [k-proc (k2)
        (apply-k k2 (1st args))]

			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 cons = zero? not >= > < <= car cdr list null? eq? equal? length list->vector 
    list? not pair? vector->list number? vector? symbol? caar cadr cdar cddr caaar caadr cadar caddr 
    cdaar cdadr cddar cdddr procedure? set-car! set-cdr! assq atom? vector make-vector vector-ref 
    vector-set! display newline void map apply negative? positive? quotient append eqv? list-tail 
    call/cc exit-list))

(define global-env         
  (make-init-env))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply-k k (= (1st args) (2nd args)))]
      [(zero?) (apply-k k (= (1st args) 0))]
      [(>=) (apply-k k (>= (1st args) (2nd args)))]
      [(>) (apply-k k (> (1st args) (2nd args)))]
      [(<) (apply-k k (< (1st args) (2nd args)))]
      [(<=) (apply-k k (<= (1st args) (2nd args)))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (cdr (1st args)))]
      [(list) (apply-k k (apply list args))]
      [(null?) (apply-k k (eq? (1st args) '()))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(length) (apply-k k (length (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(not) (apply-k k (not (1st args)))]
      [(pair?) (apply-k k (apply pair? args))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(caar) (apply-k k (car (car (1st args))))]
      [(cadr) (apply-k k (car (cdr (1st args))))]
      [(cddr) (apply-k k (cdr (cdr (1st args))))]
      [(cdar) (apply-k k (cdr (car (1st args))))]
      [(caaar) (apply-k k (car (car (car (1st args)))))]
      [(caadr) (apply-k k (car (car (cdr (1st args)))))]
      [(cadar) (apply-k k (car (cdr (car (1st args)))))]
      [(caddr) (apply-k k (car (cdr (cdr (1st args)))))]
      [(cdaar) (apply-k k (cdr (car (car (1st args)))))]
      [(cdadr) (apply-k k (cdr (car (cdr (1st args)))))]
      [(cddar) (apply-k k (cdr (cdr (car (1st args)))))]
      [(cdddr) (apply-k k (cdr (cdr (cdr (1st args)))))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(atom?) (apply-k k (atom? (1st args)))]
      [(vector) (apply-k k (apply vector args))]
      [(make-vector) (apply-k k (apply make-vector args))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (apply-k k (apply display args))]
      [(newline) (apply-k k (newline))]
      [(void) (apply-k k (void))]
      [(map) (map-cps (lambda (arg k) (apply-proc (1st args) (list arg) k)) (2nd args) k)]
      [(apply) (apply-proc (1st args) (2nd args) k)]
      [(negative?) (apply-k k (negative? (1st args)))]
      [(positive?) (apply-k k (positive? (1st args)))]
      [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
      [(append) (apply-k k (apply append args))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
      [(call/cc) (apply-proc (1st args) (list (k-proc k)) k)]
      [(exit-list) args]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([input (read)])
      (if (equal? input '(exit))
        (eopl:error 'rep "Successfully Exited rep")
        ;; notice that we don't save changes to the environment...
        (let ([answer (top-level-eval (syntax-expand (parse-exp input)))])
          (if (proc-val? answer)
            (eopl:pretty-print '<interpreter-procedure>)
            (eopl:pretty-print answer))
          (newline)
          (rep))))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))










