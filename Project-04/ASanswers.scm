;==============================================================================
;==============================================================================
;Part 1: Assignment, Local State, and the Environment Model
;==============================================================================
;1.Rewrite the make-account procedure on page 223 so that it uses lambda 
;more explicitly. Create several versions, as follows. (Important: please do 
;exactly what each of the three following versions specifies; no more and no 
;less.) 
;==============================================================================
;1.1.
;==============================================================================

(define make-account-lambda 
	(lambda (balance) ;lambda added
		(define withdraw 
			(lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
				balance) "Insufficient funds"))
		);lambda added 
	(define deposit 
		(lambda (amount)
			(set! balance (+ balance amount)) balance))
	(lambda (m) ; dispatch replaced with expression
		(cond ((equal? m 'withdraw) withdraw)
			((equal? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT" m)))
)))

;==============================================================================
;1.2.Second version: Start with a copy of the first version. Then inline the 
;internal procedures deposit and withdraw. That is, replace references to 
;them by the bodies of the procedures. Then you can eliminate the definitions 
;of those procedures. Call this procedure make-account-inline.
;==============================================================================

(define make-account-inline
	(lambda (balance)
		(lambda (m) ; define replaced with lambda
		(cond ((equal? m 'withdraw)
			(lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
				balance) "Insufficient funds"))) 
	((equal? m 'deposit) ;lambda added here
		(lambda (amount)
			(set! balance (+ balance amount)) balance))
			(else(error "Unknown request 
			-- MAKE-ACCOUNT" m)))
)))	

;==============================================================================
;1.3.Third version (A little extra credit): Start with a copy of the second 
;ersion. Call this new version make-account-inline-factored.
;==============================================================================

(define make-account-inline-factored
	(lambda (balance)
		(lambda (m) ; define replaced with lambda
		(if (or (equal? m 'deposit)
		(equal? m 'withdraw))
			(lambda (amount)
			(cond ((equal? m 'withdraw)
				(if (>= balance amount)
				(begin (set! balance (- balance amount))
				balance) "Insufficient funds")) 
			(else (set! balance (+ balance amount)) balance)))
			(error "Unknown request -- MAKE-ACCOUNT" m)
		))))	

		
;==============================================================================
;2.Abelson and Sussman ask you to show how the environment model explains 
;the behavior of make-account. Do that problem twice, once for the function 
;make-account as A&S wrote it, with internal defines, and then again for the 
;function make-account-lambda of problem 1.
;==============================================================================
;2.1. Exercise 3.11
;==============================================================================
;;ON PAPER
;==============================================================================
;2.2.
;==============================================================================
;;ON PAPER
;==============================================================================
;3.;Exercise 3.2 (page 224). Please read the statement of the problem very 
;carefully. It tells you precisely what you should do. Please do exactly what 
;it says. And do not use any global variables in doing this problem.
;==============================================================================

(define (make-monitored f)
  (let ((counter 0))
    (lambda (mf) 
      (cond ((equal? mf 'how-many-calls?) counter)
            ((equal? mf 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) 
                          (f mf)))
			))))			
		

;==============================================================================
;4.Exercise 3.3 (page 225).
;In doing this problem, build on your solution to Problem 1. In fact, see if 
;you can use one of your solutions to Problem 1 as a "black box" -- that is, 
;make the solution to this problem a "wrapper" procedure that just invokes 
;one of the versions of make-account from Problem 1, after handling password 
;checks. 
;==============================================================================

(define (make-pw-account balance password)
  (let ((account (make-account-inline-factored balance)))
	;wrapper procedure above from 1.3
    (lambda (input m)
      (let ((verify (equal? input password)))
		(if((equal? m 'check-password) verify)
              (verify (account m))
              (error "Wrong password"))
      ))))
	  

;==============================================================================
;5.Exercise 3.9 
;==============================================================================
;;ON PAPER
;==============================================================================
;==============================================================================