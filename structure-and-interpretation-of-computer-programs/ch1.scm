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


