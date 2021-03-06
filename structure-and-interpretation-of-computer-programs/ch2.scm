(define nil '())

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
(define (opposite-signs? x)
  (or (and (negative? (lower-bound x))
	   (positive? (upper-bound x)))
      (and (positive? (lower-bound x))
	   (negative? (upper-bound x)))))

(define (both-positive? x)
  (and (positive? (lower-bound x))
       (positive? (upper-bound x))))

(define (both-negative? x)
  (and (negative? (lower-bound x))
       (negative? (upper-bound x))))

;;; I was lucky to find a nice paper on interval
;;; multiplication
;;;
;;; http://fab.cba.mit.edu/classes/S62.12/docs/Hickey_interval.pdf
;;;
;;; Acording to the paper, we can simplify our
;;; multiplications like so:
;;;
;;; | sign int1 | sign int2 | lower bound       | upper bound       |
;;; |-----------+-----------+-------------------+-------------------|
;;; | P         | P         | x1*x2             | y1*y2             |
;;; | P         | N         | y1*x2             | x1*y2             |
;;; | P         | M         | y1*x2             | y1*y2             |
;;; |-----------+-----------+-------------------+-------------------|
;;; | N         | P         | x1*y2             | y1*x2             |
;;; | N         | N         | y1*y2             | x1*x2             |
;;; | N         | M         | x1*y2             | x1*x2             |
;;; |-----------+-----------+-------------------+-------------------|
;;; | M         | P         | x1*y2             | y1*y2             |
;;; | M         | N         | y1*x2             | x1*x2             |
;;; | M         | M         | min(x1*y2, y1*x2) | max(x1*x2, y1*y2) |
(define (mul-interval x y)
  (cond
   ((and (both-positive? x) (both-positive? y))
    (make-interval (* (lower-bound x) (lower-bound y))
		   (* (upper-bound x) (upper-bound y))))
   ((and (both-positive? x) (both-negative? y))
    (make-interval (* (upper-bound x) (lower-bound y))
		   (* (lower-bound x) (upper-bound y))))
   ((and (both-positive? x) (opposite-signs? y))
    (make-interval (* (upper-bound x) (lower-bound y))
		   (* (upper-bound x) (upper-bound y))))

   ((and (both-negative? x) (both-positive? y))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (upper-bound x) (lower-bound y))))
   ((and (both-negative? x) (both-negative? y))
    (make-interval (* (upper-bound x) (upper-bound y))
		   (* (lower-bound x) (lower-bound y))))
   ((and (both-negative? x) (opposite-signs? y))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (lower-bound x) (lower-bound y))))

   ((and (opposite-signs? x) (both-positive? y))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (upper-bound x) (upper-bound y))))
   ((and (opposite-signs? x) (both-negative? y))
    (make-interval (* (upper-bound x) (lower-bound y))
		   (* (lower-bound x) (lower-bound y))))
   (else ; (and (opposite-signs? x) (opposite-signs y))
    (make-interval (min (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (lower-bound y)))
		   (max (* (lower-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y)))))))

(define (ex2.11)
  (print-interval (mul-interval interval1 interval2))
  (newline))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((delta (* c (/ p 100.0))))
    (make-interval (- c delta) (+ c delta))))

(define (percent i)
  (let* ((center (center i))
	 (delta (width i)))
    (/ (* delta 100) center)))

(define (ex2.12)
  (let ((interval (make-center-percent 5 5)))
    (print-interval interval)
    (newline)
    (display (percent interval))
    (newline)))

(define (last-pair lst)
  (if (= (length lst) 1)
      lst
      (last-pair (cdr lst))))

(define (ex2.17)
  (display (last-pair (list 1 2 3 4 5)))
  (newline))

(define (reverse lst)
  (define (iter items result)
    (if (null? items)
	result
	(iter (cdr items) (cons (car items) result))))
  (iter lst '()))

(define (ex2.18)
  (display (reverse (list 1 2 3 4)))
  (newline))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount
			(first-denomination coin-values))
		     coin-values)))))

(define (no-more? coin-values) (null? coin-values))
(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define euro-coins (list 200 100 50 20 10 5 1))

(define (same-parity x . y)
  (if (odd? x)
      (cons x (filter (lambda (e) (odd? e))  y))
      (cons x (filter (lambda (e) (even? e)) y))))

(define (ex2.19)
  (display (same-parity 1 2 3 4 5 6 7))
  (newline)
  (display (same-parity 2 3 4 5 6 7 8))
  (newline))

(define (square n)
  (* n n))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(define (ex2.21)
  (display (square-list (list 1 2 3 4)))
  (newline))

(define (for-each proc items)
  (if (null? items)
      #t
      (and (proc (car items)) (for-each proc (cdr items)))))

(define (ex2.23)
  (for-each
   (lambda (x)
     (display x)
     (newline))
   (list 1 2 3 4)))

(define (ex2.25)
  (display (car
	    (cdr
	     (car
	      (cdr
	       (cdr
		'(1 3 (5 7) 9)))))))
  (newline)
  (display (car
	    (car
	     '((7)))))
  (newline)
  (display (car
	    (cdr
	     (car
	      (cdr
	       (car
		(cdr
		 (car
		  (cdr
		   (car
		    (cdr
		     (car
		      (cdr
		       '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))
  (newline))

(define (deep-reverse lst)
  (define (iter items result)
    (cond ((null? items)
	   result)

	  ((not (pair? (car items)))
	   (iter (cdr items) (cons (car items) result)))

	  (else
	   (iter (cdr items)
		 (cons (deep-reverse (car items)) result)))))
  (iter lst '()))

(define (ex2.27)
  (display (deep-reverse '(1 (2 3) (4 5 (6 7 8)))))
  (newline))

(define (fringe tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else
	 (append (fringe (car tree)) (fringe (cdr tree))))))

(define nightmarish
  (list
   (list
    (list
     (list 1 2 (list 3 (list 5 6) (list 7))))
    (list 8 9 (list 10 (list (list 11 12))))
    (list 13 (list 14) (list 15)))))

(define (ex2.28)
  (display (fringe nightmarish))
  (newline))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define mobile
  (make-mobile
   (make-branch 2
		(make-mobile (make-branch 2 2)
			     (make-branch 2 2)))
   (make-branch 2 (make-mobile (make-branch 2 2)
			       (make-branch 2 2)))))

(define (total-weight m)
  (cond ((null? m) 0)
	((not (pair? m)) m)
	(else (+ (total-weight (branch-structure (left-branch m)))
		 (total-weight (branch-structure (right-branch m)))))))

(define (torque m)
  (* (branch-length m) (total-weight (branch-structure m))))

(define (balanced? m)
  (cond ((not (pair? m)) #t)
	(else
	 (and (= (torque (left-branch m))
		 (torque (right-branch m)))
	      (balanced? (branch-structure (left-branch m)))
	      (balanced? (branch-structure (right-branch m)))))))

(define (ex2.29)
  (display (total-weight mobile))
  (newline)
  (display (balanced? mobile))
  (newline))

(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

(define (square-tree-direct tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree-direct (car tree))
		    (square-tree-direct (cdr tree))))))

(define (ex2.30)
  (display (square-tree tree))
  (newline)
  (display (square-tree-direct tree))
  (newline))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree-map tree)
  (tree-map square tree))

(define (ex2.31)
  (display (square-tree-map tree))
  (newline))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest
		(map (lambda (x)
		       (cons (car s) x))
		     rest)))))

(define (ex2.32)
  (display (subsets '(1 2 3 4)))
  (newline))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     (+ (square p) (square q))
		     (+ (* 2 p q) (square q))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define (even-fibs n)
  (accumulate cons
	       '()
	       (filter even?
		       (map fib
			    (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      '()
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (ex2.33)
  (display (my-map square (list 1 2 3)))
  (newline)
  (display (my-append (list 1 2 3) (list 4 5 6)))
  (newline)
  (display (my-length (list 1 2 3)))
  (newline))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms)
		(+ this-coef (* higher-terms x)))
	      0
	      coefficient-sequence))

(define (count-leaves tree)
  (accumulate
   +
   0
   (map
    (lambda (x)
      (cond ((null? x) 0)
	    ((not (pair? x)) 1)
	    (else (count-leaves x)))) tree)))

(define (ex2.35)
  (display tree)
  (newline)
  (display (count-leaves tree))
  (newline))

(define seq-of-seqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (ex2.36)
  (display (accumulate-n + 0 seq-of-seqs))
  (newline))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row col) (dot-product row col) cols) m n)))

(define matrix
  (list (list 1  2  3  4)
	(list 5  6  7  8)
	(list 9 10 11 12)))

(define vector (list 1 2 3 4))

(define (ex2.37)
  (display (dot-product vector vector))
  (newline)
  (display (matrix-*-vector matrix vector))
  (newline)
  (display (matrix-*-matrix matrix matrix))
  (newline))

;;; If we want `fold-left' and `fold-right' to give the same
;;; result, `op' must be commutative
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (ex2.38)
  (display (fold-right / 1 (list 1 2 3)))
  (newline)
  (display (fold-left / 1 (list 1 2 3)))
  (newline)
  (display (fold-right list '() (list 1 2 3)))
  (newline)
  (display (fold-left list '() (list 1 2 3)))
  (newline))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (ex2.39)
  (display (reverse-left vector))
  (newline)
  (display (reverse-right vector))
  (newline))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

(define (prime? n)
  (fast-prime? n 100))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs-with-unique-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(define (ex2.40)
  (display (prime-sum-pairs-with-unique-pairs 5))
  (newline))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
	    (map (lambda (k) (list i j k))
		 (enumerate-interval 1 (- j 1))))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-triples-up-to-sum n s)
  (filter (lambda (triple) (<= (+ (car triple) (cadr triple) (caddr triple)) s))
	  (unique-triples n)))

(define (ex2.41)
  (display (unique-triples-up-to-sum 5 8))
  (newline))
