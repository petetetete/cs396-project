(load "list.scm")
(load "list-compl.scm")
(load "point.scm")
(load "centroids.scm")

(define get-class
  (lambda (cp cns)
    (let ((list (list-map (lambda (cn) (p-dist (car cp) cn)) cns)))
      (list-index (list-min list) list))))


(define cpoints-update
  (lambda (cps cns)
    (let ((update-cpoint (lambda (cp) (cons (car cp) (get-class cp cns)))))
      (list-map update-cpoint cps))))


(define class-size
  (lambda (cl cps)
    (let ((conditional-increment (lambda (total cp) (if (= (cdr cp) cl) (+ total 1) total))))
      (list-foldl conditional-increment 0 cps))))


(define class-sum
  (lambda (cl cps)
    (let ((conditional-add (lambda (total cp) (if (= (cdr cp) cl) (p-add (car cp) total)  total))))
      (list-foldl conditional-add '(0 0) cps))))


(define get-centroid
  (lambda (cl cps)
    (let ((cpoint-average (lambda (p1 size) (list (/ (car p1) size) (/ (cadr p1) size)))))
      (cpoint-average (class-sum cl cps) (class-size cl cps)))))


(define get-centroids
  (lambda (k cps)
    (letrec ((helper
              (lambda (kv cps)
                (if (= kv 0)
                    empty
                    (cons (get-centroid (- k kv) cps) (helper (- kv 1) cps))))))
      (helper k cps))))


(define repeat
  (lambda (c f i)
    (if (= c 0)
        i
        (repeat (- c 1) f (f i)))))


(define k-means
  (lambda (k pts c)
    (let ((centroids (initial-centroids k pts))
          (cpoints (list-map (lambda (pt) (cons pt 0)) pts)))
      (let ((repeat-function (lambda (data) (cons (cpoints-update (car data) (cdr data)) (get-centroids k (car data)))))
            (repeat-initial (cons (cpoints-update cpoints centroids) centroids)))
        (repeat c repeat-function repeat-initial)))))


;(define points '((0 0) (1 0) (0 1) (0 2) (2 2) (4 2)))
;(define cpoints '(((0 0) . 0) ((1 0) . 0) ((0 1) . 0) ((0 2) . 1) ((2 2) . 1) ((42 2) . 2)))
;(define centroids (initial-centroids 2 points))

;(write 'get-class:)
;(get-class '((0 0) . 0) points)

;(write 'cpoints-update:)
;(cpoints-update cpoints centroids)

;(write 'class-size:)
;(class-size 0 cpoints)

;(write 'class-sum:)
;(class-sum 0 cpoints)

;(write 'get-centroid:)
;(get-centroid 0 cpoints)

;(write 'get-centroids:)
;(get-centroids 3 cpoints)

;(write 'repeat:)
;(repeat 10 (lambda (x) (+ 4 x)) 2)

;(write 'k-means:)
;(k-means 4 points 5)
;(initial-centroids 3 points)