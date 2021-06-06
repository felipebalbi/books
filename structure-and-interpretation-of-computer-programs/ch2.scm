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

(define (kons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- KONS" m))))
  dispatch)

(define (kar z) (z 0))
(define (kdr z) (z 1))

(define (qons x y)
  (lambda (m) (m x y)))

(define (qar z)
  (z (lambda (p q) p)))

(define (qdr z)
  (z (lambda (p q) q)))

(define (ex2.4)
  (display (qdr (qons 1 2)))
  (newline))

(define (int-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (largest-power-of-n n z)
  (if (= (remainder z n) 0)
      (+ 1 (largest-power-of-n n (/ z n)))
      0))

(define (int-car z)
  (largest-power-of-n 2 z))

(define (int-cdr z)
  (largest-power-of-n 3 z))

(define (ex2.5)
  (let ((z (int-cons 3 4)))
    (display (int-car z))
    (newline)
    (display (int-cdr z))
    (newline)))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; This stuff is mindboggling
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define four (lambda (f) (lambda (x) (f (f (f (f x)))))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (ex2.6)
  (display (add three four))
  (newline))

(define (make-interval a b) (cons a b))

;;; If we assume lower bound always first, we can use `car'
;;; and `cdr' directly. For the sake of completeness, let's
;;; not make such assumption and take the lower bound to be
;;; the minimum between `car z' and `cdr z'. Upper bound is
;;; analogous.
(define (lower-bound z) (min (car z) (cdr z)))
(define (upper-bound z) (max (car z) (cdr z)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define interval1 (make-interval 8 2))
(define interval2 (make-interval -2 4))

(define (print-interval interval)
  (display "[")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]"))

(define (ex2.7)
  (print-interval interval1)
  (newline))

(define (sub-interval x y)
  (add-interval x
		(make-interval (- (upper-bound y))
			       (- (lower-bound y)))))

(define (ex2.8)
  (print-interval (sub-interval interval1 interval2))
  (newline))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error" y)
      (mul-interval
       x
       (make-interval
	(/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

(define span0 (make-interval -1 1))

(define (ex2.10)
  (div-interval interval1 span0))

;;; ex2.11 is complicated. In summary, we must consider that
;;; each interval has 3 cases:
;;;
;;; 1. both ends positive
;;; 2. both ends negative
;;; 3. one positive, one negative
;;;
;;; Because we're multiplying 2 intervals, there's are 9
;;; possible cases (i.e. 3 * 3).
;;;
;;; The difficult part is figuring out how to multiply
;;; depending on each of these cases.
;;;
;;; These little helpers below, will aid with finding out
;;; the three cases for each interval.
(define (opposite-signs a b)
  (or (and (negative? a) (possitive? b))
      (and (positive? a) (negative? b))))

(define (both-positive a b)
  (and (positive? a) (positive? b)))

(define (both-negative a b)
  (and (negative? a) (negative? b)))


