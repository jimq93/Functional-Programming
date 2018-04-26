;;; file: s450.scm (base)
;;;
;;; Metacircular evaluator from chapter 4 of STRUCTURE AND
;;; INTERPRETATION OF COMPUTER PROGRAMS (2nd edition)
;;;
;;; Modified by kwn, 3/4/97
;;; Modified and commented by Carl Offner, 10/21/98 -- 10/12/04
;;;
;;; This code is the code for the metacircular evaluator as it appears
;;; in the textbook in sections 4.1.1-4.1.4, with the following
;;; changes:
;;;
;;; 1.  It uses #f and #t, not false and true, to be Scheme-conformant.
;;;
;;; 2.  Some function names were changed to avoid conflict with the
;;; underlying Scheme:
;;;
;;;       eval => xeval
;;;       apply => xapply
;;;       extend-environment => xtend-environment
;;;
;;; 3.  The driver-loop is called s450.
;;;
;;; 4.  The booleans (#t and #f) are classified as self-evaluating.
;;;
;;; 5.  These modifications make it look more like UMB Scheme:
;;;
;;;        The define special form evaluates to (i.e., "returns") the
;;;          variable being defined.
;;;        No prefix is printed before an output value.
;;;
;;; 6.  I changed "compound-procedure" to "user-defined-procedure".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 xeval and xapply -- the kernel of the metacircular evaluator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HW6 Part 1

;;HW6 Part 2

(define (xeval exp env)
  (let ((action (lookup-action (type-of exp))))
	(cond (action
        ;;invoke action, passing it the expression and the environment
        (cond 
			((equal? action make-procedure)
            (action (lambda-parameters exp)
                    (lambda-body exp)
                    env))
            (else (action
				(if 
					(cond? exp) 
						(cond->if exp)
							(if (begin? exp) 
								(begin-actions exp)
													exp)) 
		env))))	
        (else (cond 
			((self-evaluating? exp) exp)
				((variable? exp)
					;;HW6 Part 3
					(cond 
						((lookup-action exp)  
						(begin
							(display "Special form: ")
							(display exp)
							""
                     ))
					(else (lookup-variable-value exp env))
					))
             
			;;HW7 Added
			 ((application? exp)
               (let ((param (xeval (operator exp) env)))
				(define (user-defined-values params oprnd env)
					(cond ((null? oprnd)'())
					(else (cons
						(cond ((delayed? (car params))
							(make-thunk (first-operand oprnd) env))
								((dynamic? (car params))
							(xeval (first-operand oprnd) 
							the-dynamic-environment))
								((reference? (car params))
								(make-reference 
								(first-operand oprnd) env))
						(else (xeval (first-operand oprnd) env)))
							(user-defined-values (cdr params)
							(rest-operands oprnd) env)))))
                 (xapply param
                         (cond ((primitive-procedure? param)
                             (list-of-values (operands exp) env))
                             (else (user-defined-values
                              (procedure-parameters param) (operands exp) env)
                             ))
                         )
                 )
               )
			((thunk? exp) (list 'thunk (thunk-exp exp)))
			((eof-object? exp) (exit))
              (else
               (s450error "Unknown expression type -- XEVAL " exp))
        ))
	)))
	
(define (type-of exp)
  (cond ((pair? exp)
      (car exp))
     (else the-empty-environment)
	  ))  

;; check for a pair of expressions in the 1d lookup table
 
(define (lookup-action type-of-expression)
  (let ((pair (assoc type-of-expression xeval-rules)))
    (cond (pair  
		(cdr pair))  
		(else #f))
		))
		 
(define (xapply procedure arguments)
	;;HW7 Added
  (define (basic-parameters procedure)
    (cond ((null? procedure) '())
		(else (cons
           (cond ((or (delayed? (car procedure)) 
						(dynamic? (car procedure))
						(reference? (car procedure)))
                 (thunk-exp (car procedure)))
                 (else (car procedure)))
         (basic-parameters (cdr procedure))))
        )
    ) 	
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((user-defined-procedure? procedure)
		 (let ((evaluate-frame
                (eval-sequence
                (procedure-body procedure)
                (xtend-environment
				  (basic-parameters 
				  (procedure-parameters procedure))
                  arguments
                  (procedure-environment procedure)))
                ))
          (begin 
		  (dynamic-frame)
		   evaluate-frame))
		)
        (else
         (s450error
          "Unknown procedure type -- XAPPLY " procedure))))

;;; Handling procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      the-empty-environment
      (cons (xeval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;; These functions, called from xeval, do the work of evaluating some
;;; of the special forms:

(define (eval-if exp env)
  (if (true? (xeval (if-predicate exp) env))
      (xeval (if-consequent exp) env)
      (xeval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (xeval (first-exp exps) env))
        (else (xeval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;HW6 Part 4 

(define (eval-assignment exp env)
  (let ((name (assignment-variable exp)))
    (cond 
		((lookup-action name)
			(s450error "CANT CHANGE VARIABLE: " name))
	(else 
	(set-variable-value! name
			 (xeval (assignment-value exp) env)
			 env)
  name))))    ;; A & S return 'ok

(define (eval-definition exp env)
  (let ((name (definition-variable exp)))  
    (cond 
		((lookup-action name)
			(s450error "CANT CHANGE DEFINITION: " name))
	(else
	(define-variable! name
      (xeval (definition-value exp) env)
      env)
  name))))     ;; A & S return 'ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers, strings, and booleans are all represented as themselves.
;;; (Not characters though; they don't seem to work out as well
;;; because of an interaction with read and display.)

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      ))

;;; variables -- represented as symbols

(define (variable? exp) (symbol? exp))

;;; quote -- represented as (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp env) (car(cdr exp)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

;;; assignment -- represented as (set! <var> <value>)

(define (assignment? exp) 
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; definitions -- represented as
;;;    (define <var> <value>)
;;;  or
;;;    (define (<var> <parameter_1> <parameter_2> ... <parameter_n>) <body>)
;;;
;;; The second form is immediately turned into the equivalent lambda
;;; expression.

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;; lambda expressions -- represented as (lambda ...)
;;;
;;; That is, any list starting with lambda.  The list must have at
;;; least one other element, or an s450error will be generated.

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; conditionals -- (if <predicate> <consequent> <alternative>?)

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; sequences -- (begin <list of expressions>)

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;;; procedure applications -- any compound expression that is not one
;;; of the above expression types.

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;; Derived expressions -- the only one we include initially is cond,
;;; which is a special form that is syntactically transformed into a
;;; nest of if expressions.

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (equal? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f                          ; no else clause -- return #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (s450error "ELSE clause isn't last -- COND->IF "
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Truth values and procedure objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Truth values

(define (true? x)
  (not (equal? x #f)))

(define (false? x)
  (equal? x #f))


;;; Procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (user-defined-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing environments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An environment is a list of frames.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;; Each frame is represented as a pair of lists:
;;;   1.  a list of the variables bound in that frame, and
;;;   2.  a list of the associated values.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;; Extending an environment

;;HW7 Part 2
(define (xtend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))    
	  (let ((xtend-frame (make-frame vars vals)))
        (set! the-dynamic-environment
         (cons xtend-frame the-dynamic-environment)) 
        (cons xtend-frame base-env)))
	  (else 
			(cond ((< (length vars) (length vals))
          (s450error "Too many arguments supplied " vars vals))
          (else (s450error "Too few arguments supplied " vars vals))))))

;;; Looking up a variable in an environment

;;HW7 Part 3
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars)) 
               (cond ((or (thunk? (car vals))(reference? (car vals))) 
						(xeval (thunk-exp (car vals))
                        (thunk-env (car vals)))
						)
                     (else (car vals)))
			 )
			(else (scan (cdr vars) (cdr vals)))))
    (cond ((equal? env the-empty-environment)
        (s450error "Unbound variable " var))
        (else 
			(let ((frame (first-frame env)))
			(scan (frame-variables frame)
                (frame-values frame))))
				))
  (env-loop env))


;;; Setting a variable to a new value in a specified environment.
;;; Note that it is an s450error if the variable is not already present
;;; (i.e., previously defined) in that environment.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))	
			;;HW7 Part 3
			(cond 
				((reference? (car vals))
                   (set-variable-value!
                    (thunk-exp (car vals))
                    val
                    (cond 
						((equal? var (thunk-exp (car vals)))
                        (cdr env))
                        (else env))
						)	
					)
                 (else (set-car! vals val))
				 )
			 )
            (else (scan (cdr vars) (cdr vals)))))
    (cond ((equal? env the-empty-environment)
        (s450error "Unbound variable -- SET! " var))
        (else 
		(let 
		((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))
				)))
  (env-loop env))

;;; Defining a (possibly new) variable.  First see if the variable
;;; already exists.  If it does, just change its value to the new
;;; value.  If it does not, define the new variable in the current
;;; frame.

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The initial environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is initialization code that is executed once, when the the
;;; interpreter is invoked.

;;HW6 Part 6

(define (setup-environment)
  (let ((initial-env
         (xtend-environment 
		 the-empty-environment
         the-empty-environment
         the-empty-environment)))
    initial-env))

;;; Define the primitive procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


;;HW6 Part 6 removed primitive procedures from original code

;;; Here is where we rely on the underlying Scheme implementation to
;;; know how to apply a primitive procedure.

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  load.s450
;;;
;;;  install special form (load "filename") in s450 evaluator
;;;

;;load.s450 added

(define eval-load
  (lambda (exp env)
    (define (filename exp) (cadr exp))
    (define thunk (lambda ()
		    (readfile)
		    ))
    (define readfile (lambda()
		       (let ((item (read)))
			 (if (not (eof-object? item))
			     (begin
			       (xeval item env)
			       (readfile))))
		       ))
    (with-input-from-file (filename exp) thunk)
    (filename exp)      ; return the name of the file - why not?
    ))

	 
;;HW6 Part 6

(define (install-primitive-procedure name action)
  (cond 
	((lookup-action name)  
	  (s450error "Wrong name: " name))
  (else 
	(begin
       (add-binding-to-frame!
        name
        (list 'primitive action)
        (first-frame the-global-environment))
        name))
	))
;;HW6 Part 2	
;;install special forms

(define (install-special-form name action)
	(cond 
	((member name 
		(frame-variables the-global-environment))
		(s450error "Wrong name: " name))
    ((assoc name xeval-rules)
      (s450error "Wrong name: " name))
	(else 
        (begin (set! xeval-rules
          (cons (cons name action) xeval-rules))
        name))
	 ))
	 

;; HW6 Part 2
;;1d lookup table to help xeval
(define xeval-rules
	(list
		(cons 'define eval-definition)
		(cons 'lambda make-procedure)	
		(cons 'set! eval-assignment)
		(cons 'if  eval-if)
		(cons 'cond xeval)
		(cons 'begin eval-sequence)
		(cons 'quote text-of-quotation)
))	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HW6 Part 5
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define	(frames env)(car env))
(define (var-names exp) (car (cdr exp)))	
(define	(var-frames env)(car (car env))) 				
(define	(val-frames env)(cdr (car env)))

(define (val-remove value this)
	(define (value-filter sequence predicate)
		(cond ((not (null? sequence)) 
			(cond ((predicate (car sequence)) 
				(cons (car sequence) 
					(value-filter (cdr sequence) predicate)))
				(else (value-filter (cdr sequence) predicate))
			))
		(else the-empty-environment)))
  (value-filter this (lambda (n) (not (equal? n value)))))

(define (scanning vars vals pre-var)
    (if (null? vars) the-empty-environment
        (if (equal? pre-var (car vars)) (cdr vals) 
            (cons (car vals)
                (scanning (cdr vars) (cdr vals) pre-var))
        )
    ))	
		
(define (defined? exp env)
  (if (null? env) #f
        (if (locally-defined? exp env) #t                                 
        (defined? exp (cdr env)) 
        )
  ))
  
(define (locally-defined? exp env)
  (cond ((member (var-names exp) (var-frames env))
      #t)
      (else #f)
	  ))
 

(define (make-unbound! exp env) 
	(cond ((null? env) #f)
	(else (begin
    (let* (
	(varnames (var-names exp)) 
	(framed (frames env))
    (varframes (var-frames env))
    (valframes (val-frames env)))
      (set-car! framed (val-remove varnames varframes))
      (set-cdr! framed (scanning
        varframes valframes varnames))
		varnames)
        (make-unbound! exp (cdr env))
    ))))
	
(define (locally-make-unbound! exp env)
	(let* (
	(varnames (var-names exp)) 
	(framed (frames env))
    (varframes (var-frames env))
    (valframes (val-frames env)))
      (set-car! framed (val-remove varnames varframes))
      (set-cdr! framed (scanning
       varframes  valframes  varnames))
      varnames
      ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 HW7 Part 1 Delayed Arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
(define (delayed? exp)
  (tagged-list? exp 'delayed))

  ;;thunk
(define (make-thunk exp env) (list 'thunk exp env))

(define (thunk? exp) (tagged-list? exp 'thunk))

(define (thunk-exp exp) (car (cdr exp)))
(define (thunk-env exp) (car (cdr (cdr exp))))

	;;streams
(define the-empty-stream '())
(define (stream-car strm) (car strm))
(define (cons-stream exp env)
  (cons (thunk-exp exp) 
        (make-thunk (thunk-env exp) env)))
(define (stream-cdr strm) 
	(xeval (thunk-exp (cdr strm)) (thunk-env (cdr strm))))
(define (stream-null? strm) (equal? strm the-empty-stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 HW7 Part 2 Dynamic Arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic? exp) (tagged-list? exp 'dynamic))
(define the-dynamic-environment '())
(define (dynamic-frame)
	(set! the-dynamic-environment 
		   (cdr the-dynamic-environment)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 HW7 Part 3 Reference Arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reference? exp)
  (tagged-list? exp 'reference))

(define (make-reference var env) (list 'reference var env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 HW7 Part 4 Continuations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define restart '())
(define terminate '())

(define (s450error .args)
(
  (newline)
  (display "s450error: ")
  (apply display* args)
  (newline)
  (restart args)))

(define (exit)
(
  (newline)
  (newline)
  (terminate "Out of s450")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The main driver loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that (read) returns an internal representation of the next
;;; Scheme expression from the input stream.  It does NOT evaluate
;;; what is typed in -- it just parses it and returns an internal
;;; representation.  It is the job of the scheme evaluator to perform
;;; the evaluation.  In this case, our evaluator is called xeval.

(define input-prompt "s450==> ")
 
(define (s450)
  ;;HW7 Part 4
	(call/cc
		(lambda (return)
        (set! restart return)
	(call/cc
		(lambda (quit)
		(set! terminate quit)))
	(prompt-for-input input-prompt)
     (let ((input (read)))
       (let ((output (xeval input the-global-environment)))
         (user-print output)))
     (s450))))
 
  
(define (prompt-for-input string)
  (newline) (newline) (display string))

;;; Note that we would not want to try to print a representation of the
;;; <procedure-env> below -- this would in general get us into an
;;; infinite loop.

(define (user-print object)
  (if (user-defined-procedure? object)
      (display (list 'user-defined-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Here we go:  define the global environment and invite the
;;;        user to run the evaluator.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define the-global-environment (setup-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;hw6 added install-special form/primitives

(install-primitive-procedure 'car car)
(install-primitive-procedure 'cdr cdr)
(install-primitive-procedure 'cons cons)
(install-primitive-procedure 'null? null?)
(install-primitive-procedure 'assoc assoc)
(install-primitive-procedure '+ +)
(install-primitive-procedure '- -)
(install-primitive-procedure '* *)
(install-primitive-procedure '/ /)
(install-primitive-procedure '> >)
(install-primitive-procedure '< <)
(install-primitive-procedure '= =)
(install-primitive-procedure 'display display)
(install-primitive-procedure 'newline newline)
(install-primitive-procedure 'member member)
(install-primitive-procedure 'zero? zero?)
(install-special-form 'defined? defined?)
(install-special-form 'locally-defined? locally-defined?)
(install-special-form 'make-unbound! make-unbound!)
(install-special-form 'locally-make-unbound! locally-make-unbound!)
(install-special-form 'load eval-load)

;;HW7 Part 1
(install-special-form 'cons-stream cons-stream)
(install-primitive-procedure 'stream-car stream-car)
(install-primitive-procedure 'stream-cdr stream-cdr)
(install-primitive-procedure 'stream-null? stream-null?)

(display "... loaded the metacircular evaluator. (s450) runs it.")
(newline)

;;load s450
; (s450)
