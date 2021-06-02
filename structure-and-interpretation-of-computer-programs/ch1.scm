(define (ex1.2)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex1.3 a b c)
  (cond
   ((and (< c a)
	 (< c b))
    (sum-of-squares a b))
   ((and (< b a)
	 (< b c))
    (sum-of-squares a c))
   ((and (< a b)
	 (< a c))
    (sum-of-squares b c))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (1- n)))))
  (iter 1 b n))

(define (ex1.16 b n)
  (fast-expt-iter b n))

;;; 1.17
(define (fast-mult a b)
  (cond ((= b 0) 0)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (1- b))))))

(define (ex1.17 a b)
  (fast-mult a b))

;;; 1.18
(define (fast-mult-iter a b)
  (define (double x)
    (+ x x))

  (define (halve x)
    (/ x 2))

  (define (iter p a b)
    (cond ((= b 0) p)
	  ((even? b) (iter p (double a) (halve b)))
	  (else (iter (+ a p) a (1- b)))))

  (iter 0 a b))

(define (ex1.18 a b)
  (fast-mult-iter a b))

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

(define (ex1.19 n)
  (fib n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (ex1.21)
  (display (smallest-divisor 199))
  (newline)
  (display (smallest-divisor 1999))
  (newline)
  (display (smallest-divisor 19999))
  (newline))

(define (prime? n)
  (fast-prime? n 100))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start end)
  (define (iter n)
    (cond ((<= n end)
	   (timed-prime-test n)
	   (iter (+ n 2)))))
  (iter (if (odd? start)
	    start
	    (+ start 1))))

(define (smallest-divisor-with-next n)
  (find-divisor-with-next n 2))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor-with-next n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor-with-next n (next test-divisor)))))

(define (ex1.23)
  (display (smallest-divisor-with-next 199))
  (newline)
  (display (smallest-divisor-with-next 1999))
  (newline)
  (display (smallest-divisor-with-next 19999))
  (newline))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1.0)
	     ((odd? k) 4.0)
	     (else 2.0))
       (yk k)))
  (* (/ h 3) (sum term 0.0 1+ n)))

(define (ex1.29)
  (display (simpson cube 0 1 100))
  (newline)
  (display (simpson cube 0 1 1000))
  (newline))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (ex1.30)
  (define (simpson-iter f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1.0)
	     ((odd? k) 4.0)
	     (else 2.0))
       (yk k)))
  (* (/ h 3) (sum-iter term 0.0 1+ n)))

  (display (simpson-iter cube 0 1 100))
  (newline))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 1+ n))

(define (pi-approx n)
  (define (term k)
    (if (even? k)
	(/ (+ k 2) (+ k 1))
	(/ (+ k 1) (+ k 2))))
  (* 4 (product term 1.0 1+ n)))

(define (ex1.31)
  (display (factorial 5))
  (newline)
  (display (pi-approx 10000))
  (newline))
