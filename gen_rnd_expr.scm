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

(define gen_init_pop
  (lambda (size rand funcs terms maxd method)
    (if (= size 0)
        empty
        (cons (gen_rnd_expr rand funcs terms maxd method) (gen_init_pop (- size 1) rand funcs terms maxd method)))))

(define fitness
  (lambda (goal expr)
    (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (exn) 0)])
      (let ((val (eval expr)))
        (if (= val 42)
            1
            (/ 1 (abs (- 42 val))))))))

; Tests
(define goal 42)
(define pop_size 20)
(define to_kill 5)
(define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 )))
(define terminals (vector 1 2 5 10))

(define population (gen_init_pop pop_size random functions terminals 3 'full))


(fitness goal (car population))

