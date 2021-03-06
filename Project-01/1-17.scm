(define (fast-multiply i j)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
	(cond 
	((or (= i 0)(= j 0)) 0)
	((= i 1) j)
	((= i -1) (- j)) 
	((= j 1) i) 
	((= j -1) (- i))
	((even? j) (double (fast-multiply i (halve j))))
	(else (+ i (fast-multiply i (- j 1))))
	)
)