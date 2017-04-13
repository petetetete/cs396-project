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


; Additional steps ;

; gen_init_pop: integer list list integer quote -> list
; generate a list of initial population elements
(define gen_init_pop
  (lambda (count functions terminals max_depth method)
    (if (= count 0)
        empty
        (cons
         (gen_rnd_expr random functions terminals max_depth method)
         (gen_init_pop (- count 1) functions terminals max_depth method)))))

; fitness: quote -> real-number
; simple example fitness function that checks
; the evaluated functions distance from 42
(define fitness
  (lambda (expr)
    (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (exn) 0)])
      (let ((val (eval expr)))
        (if (= val 42)
            1
            (/ 1 (abs (- 42 val))))))))

; kill_weakest: list integer -> list
; kills least fit elements in the population probabilistically
; based on their fitness
(define kill_weakest
  (lambda (population to_kill)
    (if (= to_kill 0)
        population
        (if (>= (random) (fitness (car population)))
            (kill_weakest (cdr population) (- to_kill 1))
            (cons (car population) (kill_weakest population to_kill))))))

; gen_children: list integer -> list
; generates new children based on a select population
; currently just copies the best elements, resulting in no
; real evolution
(define gen_children
  (lambda (population to_gen)
    (if (= to_gen 0)
        empty
        (if #t
            (cons (car population) (gen_children (cdr population) (- to_gen 1)))
            (gen_children (cdr population) to_gen)))))

; sim_cycle: list integer integer -> list
; runs generations/cycles of the evolution
(define sim_cycle
  (lambda (population max_iter to_kill)
    (let ((sorted_pop (sort population (lambda (a b) (> (fitness a) (fitness b))))))
      (if (= max_iter 0)
          population
          (sim_cycle (append (kill_weakest (reverse sorted_pop) to_kill) (gen_children sorted_pop to_kill)) (- max_iter 1) to_kill)))))

; simulation: integer integer integer list list integer -> list
; runs the evolution simulator
(define simulation
  (lambda (pop_size max_iter to_kill functions terminals max_depth method)
    (sim_cycle (gen_init_pop pop_size functions terminals max_depth method) max_iter to_kill)))
        
; Example variables
; (define pop_size 30)
; (define to_kill 5)
; (define max_depth 4)
; (define method 'full)
; (define max_iter 2)
; (define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 )))
; (define terminals (vector 1 2 5 10))

; (simulation pop_size max_iter to_kill functions terminals max_depth method)


