;==============================================================================
;==============================================================================
;provided stream header files and chapter 3 of SICP.

;;(load "stream_header.scm")
;;fixed by including functions from stream_header

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
		

;;provided helper functions from chapter 3 SICP
(define (add-streams s1 s2) 
	(stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers
	(cons-stream 1 (add-streams ones integers)))

;==============================================================================
;1.
;Define a function 
;(display-n stream n)
;that prints the first n elements of stream, each on a separate line. 
;==============================================================================

(define (display-n stream n)
		(define (nextstream string) 
			(display string) (newline))
		(stream-foreach nextstream
			(stream-filter (lambda (x) (> n 0)) stream)
		)
)

;==============================================================================
;2.
;Exercise 3.50 (page 324) 
;This exercise is pretty simple, provided you follow the suggestion in the 
;book. In fact, if you want, you can forget about checking for the empty 
;stream—we will only use this procedure for infinite streams.
; (define (stream-map proc . argsstream)
;     (if (?? (car argsstream))
;         the-empty-stream
;         (??
;             (apply proc (map ?? argsstream))
;             (apply stream-map
;                    (cons proc (map ?? argsstream))))))
;==============================================================================
;;no need to check for empty stream due to infinite streams

(define (stream-map proc . argstreams)
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))

;==============================================================================
;3.
;As explained in section 3.5.2, we can define an infinite stream of ones and 
;use this to define the stream of positive integers:

;(define ones (cons-stream 1 ones))

;(define integers (cons-stream 1 (add-streams ones integers)))

;Type in these definitions and verify that they work by using the display-n 
;procedure. Generate the stream notdiv-235 of all integers that are not 
;divisible by any of the numbers 2, 3, or 5. (Use stream-filter.)
;==============================================================================

(define notdiv-235
  (stream-filter
   (lambda (x) 
     (and (> (remainder x 2) 0)
     (and (> (remainder x 3) 0)
          (> (remainder x 5) 0)
	)))
   integers
   )
  )

 ;;testing
 
;;(display-n notdiv-235 1)
;;seems to display infinite streams
;==============================================================================
;4.
;Exercise 3.53 in the text. Write carefully about this. 

;==============================================================================
;;in notes.txt
(define s (cons-stream 1 (add-streams s s)))
;==============================================================================
;5.
;Exercise 3.54 in the text. 
;Define a procedure mul-streams, analogous to add-streams, that produces the 
;elementwise product of its two input streams. Use this together with the 
;stream of integers to complete the following definition of the stream whose 
;nth element (counting from 0) is n + 1 factorial: 

;(define factorials (cons-stream 1 (mul-streams <??> <??>)))

;==============================================================================

(define (mul-streams s1 s2)
		(stream-map * s1 s2))
(define factorials	
  (cons-stream 1 (mul-streams factorials integers)))
  
;==============================================================================
;6.
;Exercise 3.55 in the text. 
;Define a procedure partial-sums that takes as argument a stream S and returns
; the stream whose elements are S0
;, S0 + S1 , S0 + S1 + S2 , .... 
;For example, (partial-sums integers) should be the 
;stream 1, 3, 6, 10, 15, .... 
;==============================================================================

(define (partial-sums sums)
  (add-streams sums 
    (cons-stream 0
		(partial-sums sums)
		)))
		
;==============================================================================