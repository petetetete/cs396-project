(define c->n
  (lambda (church)
    ((church (lambda (a) (+ a 1))) 0)))
       
(define n->c
  (lambda (number)
    (letrec ((repeat (lambda (c f i) (if (= c 0) i (repeat (- c 1) f (f i))))))
      (lambda (f) (lambda (x) (repeat number f x))))))

(define add
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f)((n f) x)))))))

(define mul
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((n (m f)) x))))))

(define list-index
  (lambda (n l)
    (if (empty? l)
        #f
        (if (eq? (car l) n)
            0
            (let ((result (list-index n (cdr l))))
              (if (not result)
                  #f
                  (+ 1 result)))))))

(define quoted_compilation
  (lambda (input)
    `(lambda (,(caadr input))
       (lambda (,(cadadr input))
         ))))


(c->n ((add (n->c 3)) (n->c 1)))
(c->n ((add (n->c 3)) (n->c 15)))
(c->n ((add ((mul (n->c 2))(n->c 13))) (n->c 3)))

(cadadr '(lambda (x y) (+ x 1)))
(quoted_compilation '(lambda (x y) (+ x 1)))

(caaddr '(lambda (x) (+ x 1)))


