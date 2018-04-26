;==============================================================================
;==============================================================================
; read-file produces a list whose elements are the expressions in the file.
;provided code for hw3
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))
; Here we go:  read in the database.
(define source (with-input-from-file "units.dat" read-file))	


;;Convert a quantity to an equal quantity q in a different 
;;unit-list V.
;;the main procedure that ties other procedures together
;;based on U and V being compatible
(define (convert q V) ;the main call procedure

;;initiates a unit based on its representation in source,base-unit  power and 
;;returns function to get these two things.

 (define (start-unit represent)
  (lambda (x)
    (cond ((equal? x 'base) (car represent))
          ((equal? x 'power) (cadr represent))
          (else (error "Wrong:Only base and power"))))) 
;;initiates quantity based on its representation in source(factor (unit 1)
;;(unit 2)) and then returns a function to get one of these units.
; only first element of list y will be used
(define (start-quantity represent)
	(lambda (x . y)   
		(cond 
			((equal? x 'factor) (car represent))
			((equal? x 'unit-list) (cdr represent))
			(else (error "Wrong:only factor and unit-list")))))

;;raise the quantity by a power, and returns new quantity with the 
;;proper values.
; keep base (car x) while changing power by multiplication
(define (power-up-quantity quantity power)
	(start-quantity
		(cons (expt (quantity 'factor) power)
		
		(map (lambda (x) (list (car x) 
			(* (cadr x) power)))
				(quantity 'unit-list)))))
;;get a quantity from a line in the file
;;quantity mapped to this name
;; 1 meter, 1 sec, 1 kg, etc.
(define (fetch-quantity name)
	(let ((entry (assoc name source)))
		(start-quantity 
			(if entry
			(cadr entry) 
            (list 1 (list name 1))))))

;;transforms unit-list to an equal quantity whose unit-list is 
;;only in base units , m,kg,sec are not defined in the file.	
(define (normalized derived-units)
	(define (basic-quantities unit-list)		  	
		(cond ((null? unit-list) '())
			(else 
				(let ((this-unit (start-unit (car unit-list))))
					(let ((this-power (this-unit 'power))
                        (basic-units (fetch-quantity (this-unit 'base))))
                    (cons
                     (if (equal? this-power 1)
                         basic-units
                         (power-up-quantity basic-units this-power))
                     (basic-quantities (cdr unit-list))))))))
  
; product of all the factors of old quantities
	(define (new-factor quantity-list)
		(if (null? quantity-list)
			1
			(* ((car quantity-list) 'factor) 
				(new-factor (cdr quantity-list)))))

;appends all base units together while combining 
;similiar base units with their powers.
  (define (standard-base-units quantities)  
	(define (get-the-powers power-m power-sec power-kg unit-list)
		(if (null? unit-list)
          (list (list 'm power-m) (list 'sec power-sec) (list 'kg power-kg))
            (let ((unit (start-unit (car unit-list))))
				(let ((base (unit 'base)) (power (unit 'power))) 
			   (get-the-powers
               (if (equal? base 'm) (+ power-m power) power-m)
               (if (equal? base 'sec) (+ power-sec power) power-sec)
               (if (equal? base 'kg) (+ power-kg power) power-kg)
               (cdr unit-list)))))) 
	(define (all-units-together quantity-list)
		(if (null? quantity-list)
          '()
          (append
           ((car quantity-list) 'unit-list)
           (all-units-together (cdr quantity-list)))))
    (get-the-powers 0 0 0 (all-units-together quantities)))

	(let ((untouched-quantity (basic-quantities derived-units)))
		(start-quantity
			(cons
				(new-factor untouched-quantity)
				(standard-base-units untouched-quantity)
			))))
;;check two set of unit-lists to see if equal.sub-lists 
;;must be in order though, always (base-unit power).
(define (its-equal m n)
		(define (this-is-in-it i j)
			(cond 
				((null? i) #t)
				((member (car i) j) 
					(this-is-in-it (cdr i) j))
				(else #f)))	 
  ;; m of n and --> m = n
  (and (this-is-in-it m n) (equal? (length m) (length n))))

  (let ((quantity (start-quantity q)))
    (let ((a (quantity 'factor)) (U (quantity 'unit-list)))
      (let ((base-U (normalized U)) (base-V (normalized V)))
		(if (its-equal (base-U 'unit-list) (base-V 'unit-list))
            (cons (* a (/ (base-U 'factor) (base-V 'factor))) V)
            (error "Wrong Units")))))
			
		
)

;(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))

;==============================================================================
;==============================================================================