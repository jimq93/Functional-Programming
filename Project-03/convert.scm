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

;;Converts quantity q to an equal quantity based on a different unit list V.
;;based on U and V are assumed as compatible
;;Main procedure relying on helper functions.
(define (convert q V) ;the main call procedure

(define (true) #t)
(define (false) #f)
;;initialize a unit based on representation of its soruce such as power and 
;;base-unit. returns function to get such quantities. 

(define (initUnit unitData)
  (lambda (x)
    (if (equal? x 'power) (car (cdr unitData))
        (if (equal? x 'base) (car unitData)
        (error "base and power only!")))))
		  
;;initialize quantity based on representation of its source (factor (unit 1)
;;(unit 2)) and then returns a function to such quantities using only the 
;;first element of y list will be used
(define (initQuantity unitData)
	(lambda (x . y)   
		(if (equal? x 'unitList) (cdr unitData)
			(if (equal? x 'factor) (car unitData)
			(error "factor and unitList only!")))))

;;raise the quantity by power of an exponenet, keeping base while changing 
;;power by multiplication and returns new quantity with the correct values.

(define (raisePower quantity power)
	(initQuantity
		(cons (expt (quantity 'factor) power)
		(map (lambda (x) (list (car x) 
		(* (car (cdr x)) power)))
		(quantity 'unitList)))
	))
;;get a quantity from a line in units.dat
;;quantity mapped to this name such as 1 m, 1 s, 1 kg, etc.
(define (getAmount name)
	(let ((entry (assoc name source)))
		(initQuantity 
		(cond (entry (car (cdr entry))) 
            (list 1 (list name 1))))))

;;transforms the unit list to that of an equal quantity in terms of base 
;;units.
	
(define (normalized derivedUnits)
	(define (baseAmount unitList)		  	
		(if (null? unitList) '()
		(let ((currentUnit (initUnit (car unitList))))
		(let ((currentPow (currentUnit 'power))
              (baseUnits (getAmount (currentUnit 'base))))
          (cons (cond 
				((equal? currentPow 1) baseUnits)
				(else (raisePower baseUnits currentPow)))
    (baseAmount (cdr unitList))
	)))))
  
; product of all the factors of old quantities
(define (reFactor oldQuantList)
	(cond ((null? oldQuantList) 1)
		(else(* ((car oldQuantList) 'factor) 
			(reFactor (cdr oldQuantList))))))

;appends all base units together while combining 
;similiar base units with their powers.
  (define (baseMetricUnits quantities)  
	(define (getPower mPower secPower kgPower unitList)
		(cond ((null? unitList)
          (list (list 'm mPower) (list 'sec secPower) (list 'kg kgPower)))
        (else (let ((unit (initUnit (car unitList))))
			(let ((base (unit 'base)) (power (unit 'power))) 
			(getPower
            (cond ((equal? base 'm) (+ mPower power))(else mPower))
            (cond ((equal? base 'sec) (+ secPower power)) (else secPower))
            (cond ((equal? base 'kg) (+ kgPower power))(else kgPower))
            (cdr unitList)))))
		)) 
	(define (combineUnits oldQuantList)
		(cond ((null? oldQuantList) '())
        (else (append
         ((car oldQuantList) 'unitList)
          (combineUnits (cdr oldQuantList))))
	))
    (getPower 0 0 0 (combineUnits quantities)))

(let ((unfactoredAmount (baseAmount derivedUnits)))
	(initQuantity
	(cons
	(reFactor unfactoredAmount)
	(baseMetricUnits unfactoredAmount)
	))))
;;check two set of units to see if equal.
;;sub-lists have to be in order always 
;; in priority (base-unit power).

(define (itsEqual m n)
		(define (withinIt i j)
			(if (null? i) (true)
				(if (member (car i) j) 
					(withinIt (cdr i) j)
				 (false))))	 
  ;; m of n and --> m = n
  (and (withinIt m n) (equal? (length m) (length n))))

;;based on conversion principle  
;;main algorithm
(let ((quantity (initQuantity q)))
    (let ((a (quantity 'factor)) (U (quantity 'unitList)))
      (let ((base-U (normalized U)) (base-V (normalized V)))
		(cond ((itsEqual (base-U 'unitList) (base-V 'unitList))
            (cons (* a (/ (base-U 'factor) (base-V 'factor))) V))
            (else (error "Wrong Units"))
	))))
)

;(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))

;==============================================================================
;==============================================================================