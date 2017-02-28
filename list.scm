; list-rep: exact-nonnegative-integer any -> list
; creates a list of size natural number containing only
; the character/string/number specified
(define list-rep
  (lambda (n v)
    (if (= n 0)
        empty
        (cons v (list-rep (- n 1) v)))))

; list-map: procedure list -> list
; applies a procedure to every element in a list and
; returns the resulting list
(define list-map
  (lambda (f l)
    (if (empty? l)
        empty
        (cons (f (car l)) (list-map f (cdr l))))))

; list-zip: list list -> list
; takes two lists as input and returns a list of
; element pairings
(define list-zip
  (lambda (l1 l2)
    (cond ((or (and (empty? l1) (not (empty? l2))) (and (not (empty? l1)) (empty? l2))) (error "Mismatched array sizes"))
          ((and (empty? l1) (empty? l2)) empty)
          (else (cons (cons (car l1) (car l2)) (list-zip (cdr l1) (cdr l2)))))))

; list-foldl: procedure any list -> any
; takes a procedure to apply to every element in a list
; with respect to the initial value given
(define list-foldl
  (lambda (op e l)
    (if (empty? l)
        e
        (list-foldl op (op e (car l)) (cdr l)))))

; list-min: list -> number
; takes a list and returns the smallest number in the list
(define list-min
  (lambda (l)
    (list-foldl
     (lambda (p1 p2) (if (< p1 p2) p1 p2)) (car l) l)))

; list-max: list -> number
; takes a list and returns the largest number in the list
(define list-max
  (lambda (l)
    (list-foldl
     (lambda (p1 p2) (if (> p1 p2) p1 p2)) (car l) l)))

; sum: list -> number
; takes a list and returns the sum of all the elements
(define sum
  (lambda (l)
    (if (empty? l)
        0
        (list-foldl + 0 l))))

; list-index: number list -> any
; finds the index of a number in a list, returning
; false if the number was not found
(define list-index
  (lambda (n l)
    (if (empty? l)
        #f
        (if (= (car l) n)
            0
            (let ((result (list-index n (cdr l))))
              (if (not result)
                  #f
                  (+ 1 result)))))))

; list-mem?: predicate any list -> boolean
; checks if an element is within a list using the given
; equality predicate
(define list-mem?
  (lambda (eq el l)
    (cond ((empty? l) #f)
          ((eq el (car l)) #t)
          (else (list-mem? eq el (cdr l))))))

; all-diff?: predicate list -> boolean
; checks whether all elements in a list are unique using
; the given equality predicate
(define all-diff?
  (lambda (eq l)
    (cond ((empty? l) #t)
          ((list-mem? eq (car l) (cdr l)) #f)
          (else (all-diff? eq (cdr l))))))
