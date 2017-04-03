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
          (vector-ref terms (rand (vector-length terms)))
          (let ((func (vector-ref funcs (rand (vector-length funcs)))))
            (cons (car func) (get_arguments (cdr func))))))))


(define gen_init_pop
  (lambda (count)
    (if (= count 0)
        empty
        (cons
         (gen_rnd_expr random functions terminals max_depth method)
         (gen_init_pop (- count 1))))))

(define fitness
  (lambda (expr)
    (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (exn) 0)])
      (let ((val (eval expr)))
        (if (= val 42)
            1
            (/ 1 (abs (- 42 val))))))))

(define kill_weakest
  (lambda (population to_kill)
    (if (= to_kill 0)
        population
        (if (>= (random) (fitness (car population)))
            (kill_weakest (cdr population) (- to_kill 1))
            (cons (car population) (kill_weakest population to_kill))))))

(define gen_children
  (lambda (population to_gen)
    (if (= to_gen 0)
        empty
        (if #t
            (cons (car population) (gen_children (cdr population) (- to_gen 1)))
            (gen_children (cdr population) to_gen)))))

(define sim_cycle
  (lambda (population max_iter to_kill)
    (let ((sorted_pop (sort population (lambda (a b) (> (fitness a) (fitness b))))))
      (if (= max_iter 0)
          population
          (sim_cycle (append (kill_weakest (reverse sorted_pop) to_kill) (gen_children sorted_pop to_kill)) (- max_iter 1) to_kill)))))

(define simulation
  (lambda (pop_size max_iter to_kill)
    (sim_cycle (gen_init_pop pop_size) max_iter to_kill)))
        
; Example variables
(define goal 42)
(define pop_size 30)
(define to_kill 5)
(define max_depth 4)
(define method 'full)
(define max_iter 2)
(define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 )))
(define terminals (vector 1 2 5 10))

(simulation pop_size max_iter to_kill)


