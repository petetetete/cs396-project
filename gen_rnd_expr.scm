(load "list.scm")

; gen_rnd_expr: procedure vector vector positive-integer symbol -> list
; generate an initial population of programs using
; vectors of functions and terms
(define gen_rnd_expr
  (lambda (rand funcs terms maxd method)
    (letrec ((get_arguments
              (lambda(arity)
                (if (= arity 0)
                    empty
                    (cons (gen_rnd_expr rand funcs terms (- maxd 1) method) (get_arguments (- arity 1)))))))
      (if (or (= maxd 0) (and (eq? method 'grow) (< (rand) (/ (vector-length terms) (+ (vector-length terms) (vector-length funcs))))))
          (vector-ref terms (floor (rand (vector-length terms))))
          (let ((func (vector-ref funcs (floor (rand (vector-length funcs))))))
            (cons (car func) (get_arguments (cdr func))))))))

(define gen_rnd_exprs
  (lambda (count)
    (if (= count 0)
        empty
        (cons
         (gen_rnd_expr random functions terminals max_depth method)
         (gen_rnd_exprs (- count 1))))))

(define fitness
  (lambda (expr)
    (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (exn) 0)])
      (let ((val (eval expr)))
        (if (= val 42)
            1
            (/ 1 (abs (- 42 val))))))))


; Tests
(define goal 42)
(define pop_size 20)
(define to_kill 5)
(define max_depth 4)
(define method 'full)
(define max_iter 50)
(define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 )))
(define terminals (vector 1 2 5 10))

(define initial_pop (gen_rnd_exprs pop_size))

(fitness (car (sort initial_pop (lambda (a b) (> (fitness a) (fitness b))))))


