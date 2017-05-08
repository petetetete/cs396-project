; Church Encoding

; c->n: procedure -> number
(define c->n
  (lambda (church)
    ((church (lambda (a) (+ a 1))) 0)))

; n->c: number -> procedure
(define n->c
  (lambda (num)
    (letrec ((c->n_loop (lambda (num res)
                          (if (= num 0)
                              (eval (quasiquote (lambda (f) (lambda (x) (unquote res)))))
                              (c->n_loop (- num 1) (quasiquote (f (unquote res))))))))
      (c->n_loop num 'x))))

; add: procedure -> procedure
(define add
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f)((n f) x)))))))

; mul: procedure -> procedure
(define mul
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((n (m f)) x))))))


; Compiler to Lambda

(define q_c_helper
  (lambda (args body)
    (if (empty? args)
        body
        (quasiquote (lambda ((unquote (car args))) (unquote (q_c_helper (cdr args) body)))))))

(define quoted_compilation
  (lambda (program)
    (cond ((number? program) (n->c program)) ; natural number
          ((and (list? program) (equal? (car program) '*))
           (quasiquote (( (unquote mul) (unquote (quoted_compilation (cadr program)) )) (unquote (quoted_compilation (caddr program)))))) ; multiplication
          ((and (list? program) (equal? (car program) '+))
           (quasiquote (((unquote add) (unquote (quoted_compilation (cadr program)))) (unquote (quoted_compilation (caddr program)))))) ; addition
          ((and (list? program) (equal? (car program) 'lambda)) (q_c_helper (cadr program) (quoted_compilation (caddr program)))) ; lambda 
          (else program)))) ; variables

(define lcompile
  (lambda (program)
    (eval (quoted_compilation program))))



; Church Encoding Tests
(c->n ((add (n->c 1)) (n->c 1)))
(c->n((add ((mul ((add (n->c 1))(n->c 2)) )(n->c 13))) (n->c 3)))

; Compiler to Lambda Tests
(quoted_compilation '5)
(quoted_compilation '(* x 2))
(quoted_compilation '(lambda (x y)(+(* 2 x) y)))
(c->n (((lcompile '(lambda (x y)(+(* 2 x) y)))(n->c 20))(n->c 2)))