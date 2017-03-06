(load "list.scm")
(load "list-compl.scm")
(load "point.scm")
(load "cpoint.scm")
(load "centroids.scm")

; get-class: cpoint points -> positive-integer
; finds a points class with respect to the centroids
(define get-class
  (lambda (cp cns)
    (let ((list (list-map (lambda (cn) (p-dist (car cp) cn)) cns)))
      (list-index (list-min list) list))))

; cpoint-update: cpoints points -> cpoints
; updates each cpoint with respect to the centroids
(define cpoints-update
  (lambda (cps cns)
    (let ((update-cpoint (lambda (cp) (cons (car cp) (get-class cp cns)))))
      (list-map update-cpoint cps))))

; class-size: positive-integer cpoints -> positive-integer
; finds the number of points within the given
; points with a given class
(define class-size
  (lambda (cl cps)
    (let ((conditional-increment (lambda (total cp) (if (cpoint-class? cl cp) (+ total 1) total))))
      (list-foldl conditional-increment 0 cps))))

; class-sum: positive-integer cpoints -> point
; sums each point within the given points
; that match the given class
(define class-sum
  (lambda (cl cps)
    (let ((conditional-add (lambda (total cp) (if (cpoint-class? cl cp) (p-add (car cp) total)  total))))
      (list-foldl conditional-add '(0 0) cps))))

; get-centroid: positive-integer cpoints -> point
; finds the centroid within the given set of
; points for the given class
(define get-centroid
  (lambda (cl cps)
    (let ((cpoint-average (lambda (p1 size) (list (/ (car p1) size) (/ (cadr p1) size)))))
      (cpoint-average (class-sum cl cps) (class-size cl cps)))))

; get-centroids: positive-integer cpoints -> points
; finds k centroids within the given set of points
(define get-centroids
  (lambda (k cps)
    (letrec ((helper
              (lambda (kv)
                (if (= kv 0)
                    empty
                    (cons (get-centroid (- k kv) cps) (helper (- kv 1)))))))
      (helper k))))

; repeat: positive-integer procedure any -> any
; calls the given procedure c times on the
; given initial value
(define repeat
  (lambda (c f i)
    (if (= c 0)
        i
        (repeat (- c 1) f (f i)))))

; k-means: positive-integer points positive-integer -> pair
; finds the updated cpoints and k centroids after
; c iterations of the algorithm
(define k-means
  (lambda (k pts c)
    (let ((centroids (initial-centroids k pts))
          (cpoints (list-map point->cpoint pts)))
      (let ((repeat-function (lambda (data) (cons (cpoints-update (car data) (cdr data)) (get-centroids k (car data)))))
            (repeat-initial (cons (cpoints-update cpoints centroids) centroids)))
        (repeat c repeat-function repeat-initial)))))

(define cpoints '( ((0 0) . 0) ((1 0) . 0) ((0 1) . 0) ((0 2) . 1) ((2 2) . 1) ((42 2) . 2)))
(get-centroids 2 cpoints)