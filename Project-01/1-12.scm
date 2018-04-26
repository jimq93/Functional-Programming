(define (triangle i j)
	(cond ((< i j) #f)
			((or (= 0 j) (= i j)) 1)
			(else (+ (triangle (- i 1) j)
					(triangle (- i 1) (- j 1))
					)))