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



; Tests
(define (iflt e1 e2 then else)  (quote (if (< e1 e2) then else)))                
(define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 ) '(iflt . 4)))
(define terminals (vector 'x 'y 1 5 10))

(define side #f)
(define last -1)
(define not_random
  (lambda args
    (if (null? args)
        (if side 0.0 (begin (set! side (not side)) 0.99))
        (let ((bound (car args)))
          (begin (set! last (modulo (+ 1 last) bound))
                 last)))))

(display "Not random tests\n")
(gen_rnd_expr not_random functions terminals 2 'grow)
(gen_rnd_expr not_random functions terminals 2 'grow)
(gen_rnd_expr not_random functions terminals 2 'full)

(display "\nRandom tests\n")
(gen_rnd_expr random functions terminals 2 'grow)
(gen_rnd_expr random functions terminals 3 'full)
