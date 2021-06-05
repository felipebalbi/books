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


