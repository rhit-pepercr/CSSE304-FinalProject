; When you convert your interpreter to CPS for A18, this could be a good place
; to put your apply-k procedure and your continuation datatype definition.

(define-datatype continuation continuation?
    [init-k]
    [if-k 
        (then expression?)
        (else expression?)
        (env environment?)
        (k continuation?)]
    [binding-k
        (id symbol?)
        (k continuation?)]
    [app-k
        (operands (list-of expression?))
        (k continuation?)]
    [map-k 
        (proc-cps procedure?)
        (L list?)
        (k continuation?)]
    [map-proc-k
        (mapped-cdr list?)
        (k continuation?)])

(trace-define apply-k
    (lambda (k v)
        (cases continuation k
            [init-k () v]
            [if-k (then else env k)
                (if v
                    (eval-exp then env k)
                    (eval-exp else env k))]
            [binding-k (id k) 
                (apply-k k (cons id v))]
            [app-k (operands k)
                ]
            [map-k (proc-cps L k) 
                (proc-cps
                    (car L)
                    (map-proc-k v k))]
            [map-proc-k (mapped-cdr k)
                (apply-k k (cons v mapped-cdr))])))

(trace-define map-cps
    (lambda (proc-cps L k)
        (if (null? L)
            (apply-k k '())
            (map-cps
                proc-cps
                (cdr L)
                (map-k proc-cps L k)))))