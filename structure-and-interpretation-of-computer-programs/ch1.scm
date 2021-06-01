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
