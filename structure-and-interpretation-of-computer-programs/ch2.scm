(define (make-rat n d)
  (let ((g (gcd n d))
	(sign (if (< d 0) - +)))
    (cons (sign (/ n g)) (abs (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(define (ex2.1)
  (print-rat (make-rat 1 2))
  (print-rat (make-rat -1 2))
  (print-rat (make-rat 1 -2))
  (print-rat (make-rat -1 -2)))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ((start-x (x-point (start-segment s)))
	(start-y (y-point (start-segment s)))
	(end-x (x-point (end-segment s)))
	(end-y (y-point (end-segment s))))
    (make-point
     (average start-x end-x)
     (average start-y end-y))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define p1 (make-point 2 2))
(define p2 (make-point 4 4))

(define segment (make-segment p1 p2))

(define midpoint (midpoint-segment segment))

(define (ex2.2)
  (print-point midpoint))

;;; Representing a rectangle as its corner points
;;;
;;; Another possibility would be to represent the rectangle
;;; as a point followed by width and height. Only the
;;; internal methods `bottom-left', `top-right',
;;; `bottom-right', and `top-left' would have to be
;;; modified. Everything else remains the same.
;;;
;;; Too lazy to implement that, but shouldn't be hard if we
;;; make the rectangle a cons of a cons, i.e.:
;;;
;;;	(define (make-rect point width height)
;;;	  (cons point (cons width height)))
;;;
;;; We could also simplify the representation above and
;;; below by assuming rectangles to always be axis aligned
;;; starting on the origin. This should be okay if we also
;;; assume that transformations (translation, rotation,
;;; scaling) are applied after the fact.
;;;
;;; If we go down that route, every rectangle is
;;; representable by a single point (top-left corner) or a
;;; pair (width . height). We can assume the other point to
;;; be (0 . 0).
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))

(define (bottom-left rect)
  (car rect))

(define (top-right rect)
  (cdr rect))

(define (bottom-right rect)
  (make-point (x-point (top-right rect))
	      (y-point (bottom-left rect))))

(define (top-left rect)
  (make-point (x-point (bottom-left rect))
	      (y-point (top-right rect))))

(define (height-rect rect)
  (let ((top-y (y-point (top-right rect)))
	(bottom-y (y-point (bottom-left rect))))
    (- top-y bottom-y)))

(define (width-rect rect)
  (let ((left-x (x-point (bottom-left rect)))
	(right-x (x-point (top-right rect))))
    (- right-x left-x)))

(define (perimeter-rect rect)
  (+ (* (width-rect rect) 2)
     (* (height-rect rect) 2)))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

(define (ex2.3)
  (let ((r (make-rect (make-point 0 0) (make-point 2 3))))
    (display (area-rect r))
    (newline)
    (display (perimeter-rect r))
    (newline)))
