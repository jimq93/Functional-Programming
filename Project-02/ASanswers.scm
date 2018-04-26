;==============================================================================
;==============================================================================
;1.Write a Scheme procedure is-list? that tests to see if an object is or is 
;not a list, according to the definition of a list in the Scheme language 
;definition. 

(define (is-list? obj) 
	(if (pair? obj) 
		(is-list? (cdr obj))(null? obj))
)	

; Going by R5RS's definition that describes an empty list on page 25. It is 
; descibed that "...a two element list is a pair whose car is the first 
; element and whose cdr is a pair whose car is the second element and whose 
; cdr is the empty list." 

;==============================================================================
;2. Exercise 2.55. 
; To her surprise, the interpreter prints back quote. Explain. 
; (car ’’abracadabra)

; The ' is a shorthand symbol for 'quote', so the expression evaluates to
; (car (quote (quote abracadabra))).
; 'car' in lisp/scheme goes for the first parameter of the expression which 
;is 'quote'. 

;Also answer the following question: why does (car 'abracadabra) not 
;evaluate to quote.

;in scheme a 'special form' is a lambda expression that evalutaes to a 
;procedure in the context of 

;"lambda formals expression expression" 

;(car 'abracadabra) evaluates to (car (quote abracadabra))

;car needs more than one parameter to evaluate to and there is only one 
;parameter. 

;==============================================================================
;3.Exercise 2.18.  
  
(define (my-reverse this)
	(cond 
		((null? (cdr this)) this)
	(else (append (my-reverse (cdr this))
		(cons (car this) '() ;quote
		)))
	))
  

; Using the provided append procedure on page 103, and following a similiar 
;logic to the append procedure , is what the recursive procedure my-reverse 
;is based on while using append to take a list of same elements last in 
;first out. 
;==============================================================================
;4. Exercise 2.20 

(define (same-parity a . b)
(define (same-filter sequence predicate)
  (if (null? sequence) '()    
      (cond ((predicate (car sequence)) 
		(cons (car sequence) (same-filter (cdr sequence) predicate)))
	  (else(same-filter (cdr sequence) predicate)))
   ))
	(cons a
	(same-filter b 
		(lambda (element) 
		(equal? (even? a) (even? element)))
	)
))

; In 2.2.3 in SICP, the procedure filter is built to select the respective 
; elements that will satisfy a given predicate. the filter procedure will 
; iterate through a list and decide to keep an depending on the predicate. The 
; cons goes through the first element up front with filter sorting out the 
; rest.  
;==============================================================================
;5.Exercise 2.21 
;Here are two different definitions of square-list. Complete both of them by 
;filling in the missing expressions:

(define (square-list items)
	(if (null? items) 
		'() ;nil is redundant
	(cons (* (car items) (car items))
	(square-list (cdr items)))
))
 
(define (square-list items)
	(map (lambda (x) (* x x)) items)
 )

 ;using (define (square x) (* x x)) as a reference
;The procedure square-list takes a list of numbers as 
;argument and returns a list of the squares of those numbers.
;first version has square hardcoded into it without the need to have to 
;define a helper function square.  
;Second version is based on map so square implemented as lambda would be the 
;optimal choice here.  
;==============================================================================
;6.Exercise 2.23
;Call your procedure my-for-each.

(define (my-for-each a b)
	(if (not (null? b))
	(begin
		(a (car b))
		(my-for-each a (cdr b))
		)))
		
;my-for-each is very much similiar to map, except it just applies the 
;procedure to each of elements in turn from left to right. 
;the initial statement is check if the list is not null then proceeds to go 
;through the list of elements. 

;==============================================================================
;==============================================================================
;8. Exercise 2.27 Call your procedure my-deep-reverse.

(define (my-deep-reverse this)
	(cond 
		((null? this) this)
		((pair? (car this))
		(append (my-deep-reverse (cdr this))
			(list (my-deep-reverse (car this)))))
	(else (append (my-deep-reverse (cdr this))
		(cons (car this) '() ;quote
		)))
	))

; Going from my solution from Exercise 2.18 of which which my-deep-reverse is 
; modified off of, this time it checks whether the list  is null then proceeds 
; to check if the car of the list is a pair, which if it is will be passed to 
; be reversed, otherwise the cdr of the list will be reversed instead.	 	
;==============================================================================
;==============================================================================
;9.Exercise 2.54.
;Two lists are said to be equal? if they contain equal elements arranged in 
;the same order. Call your procedure my-equal?.  

 (define (my-equal? this-list that-list) 
   (cond ((and (null? this-list) (null? that-list)) #t) 
         ((or (null? this-list) (null? that-list)) #f)
         ((not (eq? (car this-list) (car that-list)) #f) 
         (else (my-equal? (cdr this-list) (cdr that-list)))))) 

; The procedures first checks if both lists are null which will confirm true, 
; Next it will check to see if one or the other is null. Then it will check if 
; the car of the lists are not the same object and return false. Finally it 
; will recursively check if the cdr of the lists are equal. 
;==============================================================================
;10
;a.Define a procedure every? 

(define (every? pred seq)
	(cond 
		((null? seq) #t 
		((pred (car seq))
		(every? pred (cdr seq))) 
		(else #f) 
		)))
		
;every? takes pred as predicate, and seq as sequence as a list. the 
;procedure checks for an empty list to pass as a true statement. Then it will 
;check the current element of the sequence and checks if an element is passed 
;onto te next sequence. Otherwise every? should evaluate to false.

;b.What should happen if the list is empty? 

;With the list being empty, it is still a list. If a list and an empty list 
;are in a union, then the union is the same as the initial list. This is very 
;much like 1 + 0 = 1 , so in a sense the empty list should always return 
;true in this case. An empty list that is an inputed to seq should return 
;true without having to refer to pred so thus,(every? pred (append seq1 
;seq2)) should still be the same as (and (every? pred seq1) (every? pred seq2) 

;==============================================================================
;11.Exercise 2.59
;Implement the union-set operation for the unordered-list representation of 
;sets. Call your procedure unordered-union-set.

(define (unordered-union-set this-set that-set)
	(define (element-of-set? x set)
		(cond ((null? set) #f)
			((equal? x (car set)) #t)
			(else (element-of-set? x (cdr set)))))
	(define this (lambda (x) (not (element-of-set? x this-set))))
	(define (union-filter predicate sequence)  
		(cond ((null? sequence) '())        
		((predicate (car sequence))         
		(cons (car sequence)               
		(union-filter predicate (cdr sequence))))        
		(else (union-filter predicate (cdr sequence)))
	))
	(append this-set (union-filter this that-set)))  

; unordered-union-set uses a combination of helper functions such as 
; element-of-set? from 2.3.3 which checks to see if an element is part of the 
; set or whether the set is empty which takes care of that, followed by 'this' 
; which uses element-of-set? and check if x is not in the set. 'union-filter' 
; is based on the filter procedure in 2.2.3. which is useful for selecting a 
; value from a sequence to satisfy the given predicate which is 'this' for 
; this-set, that is then appended with that-set. 

;==============================================================================
;12.Exercise 2.62
;Give a O(n) implementation of union-set for sets represented as ordered lists. 
;Call your procedure ordered-union-set.

(define (ordered-union-set this-set that-set)
	(define a (car this-set))
	(define b (car that-set))
	(define c (cdr this-set))
	(define d (cdr that-set))
	(cond
		((null? that-set) this-set) 
		((null? this-set) that-set) 
		((= a b) ;add element if equal
		 (cons a (ordered-union-set c d)))
		((> (a b)
		(cons b (ordered-union-set this-set d)))
		;add element into the union
    (else 
		(cons (a) (ordered-union-set (c) that-set))
	);add element into the union
	)
))

;The helpers a,b,c,d are coded as the car and cdr of sets for the sake of 
;convience. The implementation of union-set here is based on intersection-set 
;from 2.3.3 in which the elements are checked in this-set and that-set to see 
;if the elements are in either respective set to see if they are copies of 
;each other. If the car of respective sets are equal then elements aka the 
;cdr of the sets are added. If the car of the this-set is greater than 
;that-set , then the cdr of that-set is added to this-set as a union, 
;otherwise the cdr of this-set is added into the union of that-set.
;==============================================================================
;13.Define a procedure remove-val which takes two parameters: a value and a 
;list. It returns a list which is the same as the original list except that 
;every element having the given value is deleted. 


(define (remove-val value this)
 (define that (lambda (n) ((not (equal? n value)))))
 (define (value-filter sequence predicate)
  (cond ((null? sequence) ('()))
    (else (cond ((predicate (car sequence)) 
		(cons (car sequence) (value-filter (cdr sequence) predicate)))
	  (else(value-filter (cdr sequence) predicate))))
   ))
(value-filter this that)
)

;The "value-filter" helper function is based on 'my-filter' function from 
;exercise 2.20 , will help sort out the elements from the list. 'that' will 
;check to see if the 'n' and 'value' parameter are not equal, which will 
;result in the n values of the list that are not equal to value will be the 
;ones that will not be removed due by the filter. 
;==============================================================================
;==============================================================================