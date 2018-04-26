;;; file: s450.scm
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

(define (xeval exp env)
  (let ((action (lookup-action (type-of exp))))
    (if action
        ;; special check for lambda, a three-argument procedure
        (if (equal? action make-procedure)
            (action (lambda-parameters exp)
                    (lambda-body exp)
                    env)
            ;; check for begin, cond to change exp passing
            (action
             (cond ((begin? exp) (begin-actions exp))
                   ((cond? exp) (cond->if exp))
                   (else exp))
             env
             )
            )
        (cond ((self-evaluating? exp) exp)
              ((variable? exp)
               (if (lookup-action exp)  ;; special form check
                   (begin
                     (display "Special form: ")
                     (display exp)
                     ""
                     )
                   (lookup-variable-value exp env)))
              ((thunk? exp) (list 'thunk (thunk-exp exp)))
              ((application? exp)
               (let ((proc (xeval (operator exp) env)))
                 (xapply proc
                         (if (primitive-procedure? proc)
                             (list-of-values-primitive (operands exp) env)
                             (list-of-values-user-defined
                              (procedure-parameters proc) (operands exp) env)
                             )
                         )
                 )
               )
              ((eof-object? exp) (exit))
              (else
               (s450error "Unknown expression type -- XEVAL " exp))
              )
        )
    )
  )

(define (xapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((user-defined-procedure? procedure)
         ;; using let guarantees procedure will be applied first
         (let ((return-value
                (eval-sequence
                 (procedure-body procedure)
                 (xtend-environment
                  (procedure-basic-parameters procedure)
                  arguments
                  (procedure-environment procedure)))
                ))
           ;; now pop off dynamic frame after evaluation, then return.
           (set! the-dynamic-environment (cdr the-dynamic-environment))
           return-value)
         )
        (else
         (s450error
          "Unknown procedure type -- XAPPLY " procedure))))

;;; Handling procedure arguments
;;; Now separated by primitive and user-defined, since user-defined can contain
;;; delayed, dynamic or reference arguments.

;;; this is the default list-of-values that doesn't need to check params. There
;;; is no way to find the formal parameters of primitive procedures in this
;;; interpreter, thus a separate procedure is needed.

(define (list-of-values-primitive exps env)
  (if (no-operands? exps)
      '()
      (cons (xeval (first-operand exps) env)
            (list-of-values-primitive (rest-operands exps) env))))

;;; now, the evaluation of each argument is handled differently if the formal
;;; argument is delayed, dynamic or a reference.

(define (list-of-values-user-defined params exps env)
  (if (no-operands? exps)
      '()
      (cons
       (let ((param (procedure-first-param params))
             (operand (first-operand exps)))
         (cond ((delayed? param) (make-thunk operand env))
               ((dynamic? param) (xeval operand the-dynamic-environment))
               ((reference? param) (make-reference operand env))
               (else (xeval operand env)))
         )
       (list-of-values-user-defined
        (procedure-rest-params params) (rest-operands exps) env))))

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

(define (eval-assignment exp env)
  (let ((name (assignment-variable exp)))
    (if (lookup-action name)  ;; special form check
        (s450error "CANNOT CHANGE SPECIAL FORM: " name)
        (begin
          (set-variable-value!
           name
           (xeval (assignment-value exp) env)
           env)
          name))))    ;; A & S return 'ok

(define (eval-definition exp env)
  (let ((name (definition-variable exp)))
    (if (lookup-action name)  ;; special form check
        (s450error "CANNOT REDEFINE SPECIAL FORM: " name)
        (begin
          (define-variable!
            name
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

;; now takes and throws away env for elegance purposes
(define (text-of-quotation exp env) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
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
;;; least one other element, or an error will be generated.

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
  (eq? (cond-predicate clause) 'else))

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
;;;     delayed expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (delayed? exp)
  (tagged-list? exp 'delayed))

(define (delayed-variable exp) (cadr exp))


(define (make-thunk exp env) (list 'thunk exp env))

(define (thunk? exp) (tagged-list? exp 'thunk))

(define (thunk-exp exp) (cadr exp))
(define (thunk-env exp) (caddr exp))

(define (force-thunk thunk) (xeval (thunk-exp thunk) (thunk-env thunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      dynamic expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic? exp)
  (tagged-list? exp 'dynamic))

(define (dynamic-variable exp) (cadr exp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      reference expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reference? exp)
  (tagged-list? exp 'reference))

(define (reference-var exp) (cadr exp))


(define (make-reference var env) (list 'reference var env))
(define (reference-env ref) (caddr ref))

(define (force-reference ref) (xeval (reference-var ref) (reference-env ref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Truth values and procedure objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Truth values

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))


;;; Procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (user-defined-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-first-param params) (car params))
(define (procedure-rest-params params) (cdr params))

;;; to be called after list-of-values-user-defined: the delayed, dynamic and
;;; reference tags are removed.

(define (procedure-basic-parameters p)
  (define (params-to-basic params)
    (if (null? params) '()
        (cons
         (let ((param (car params)))
           (cond ((delayed? param) (delayed-variable param))
                 ((dynamic? param) (dynamic-variable param))
                 ((reference? param) (reference-var param))
                 (else param))
           )
         (params-to-basic (cdr params)))
        )
    )       
  (params-to-basic (procedure-parameters p)))

(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Special form table & related table functions
;;;     a table for each special form symbol and function, for xeval
;;;     most arguments in this table should take exp and env as arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form-table
  (list
   (cons 'quote text-of-quotation)
   (cons 'set! eval-assignment)
   (cons 'define eval-definition)
   (cons 'if eval-if)
   (cons 'lambda make-procedure)  ;; only entry with three args, see xeval
   (cons 'begin eval-sequence)
   (cons 'cond xeval)
   )
  )

;; the table is set up as a list of pairs for Scheme's assoc.

(define (lookup-action type-of-expression)
  (let ((pair (assoc type-of-expression special-form-table)))
    (if pair
        (cdr pair) ;; the procedure related to the special form
        #f)))

;; this is just retrieving the tag of the tagged list representation, if it
;; applies. else, return garbage (returning the expression back causes a
;; variable binded to a special form to cause an error).

(define (type-of exp)
  (if (pair? exp)
      (car exp)
      '()))   ;; empty list should never be a special form, safe garbage

(define (install-special-form name action)
  (if (or (member name (frame-variables the-global-environment))
          (assoc name special-form-table))
      (s450error "NAME CONFLICT: " name)
      (begin
        (set! special-form-table
              (cons (cons name action) special-form-table))
        name)
      )
  )

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
;;;   reconstruction of the passed-in lists is required in case of delayed,
;;;   reference or dynamic expressions.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;; Extending both environments. The static environment must be returned, so
;;; setting the dynamic environment is taken care of first. The same frame (not
;;; two copies of the same variables and values) is preprended to both.

(define (xtend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (let ((frame (make-frame vars vals)))
        (set!
         the-dynamic-environment
         (cons frame the-dynamic-environment)) ;; push frame
        (cons frame base-env))
      (if (< (length vars) (length vals))
          (s450error "Too many arguments supplied " vars vals)
          (s450error "Too few arguments supplied " vars vals))))

;;; Looking up a variable in an environment

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (cond ((thunk? val) (force-thunk val))
                     ((reference? val) (force-reference val))
                     (else val))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (s450error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; Setting a variable to a new value in a specified environment.
;;; Note that it is an error if the variable is not already present
;;; (i.e., previously defined) in that environment.
;;; Can be recursive if variable is a reference.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (reference? (car vals))
                 (let ((ref-var (reference-var (car vals))))
                   (set-variable-value!
                    ref-var
                    val
                    ;; if necessary, avoid infinite loop, continue to next env
                    (if (eq? var ref-var)
                        (enclosing-environment env)
                        env)))
                 (set-car! vals val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (s450error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
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
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Defined & unbinding special forms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; all exps are symbol names here.

;;; the local cases are actually sub-cases of the general cases, thus defined?
;;; and make-unbound! are constructed in terms of these.

;;; exp here always includes a variable name, in this implementation the 2nd
;;; element of the exp list.

(define (defined/unbound-variable exp) (cadr exp))

(define (defined? exp env)
  (cond ((equal? env the-empty-environment) #f)
        ((locally-defined? exp env) #t)  ;; once found in this frame or higher
                                         ;; up, done
        (else (defined? exp (enclosing-environment env))) ;; go to higher up
                                                          ;; frame
        )
  )

(define (locally-defined? exp env)
  ;; member returns a sub-list if successful, not #t, thus the wrapping if.
  (if (member (defined/unbound-variable exp)
              (frame-variables (first-frame env)))
      #t
      #f
      )
  )

(define (make-unbound! exp env)
  (if (not (equal? env the-empty-environment))
      (begin
        (locally-make-unbound! exp env)
        (make-unbound! exp (enclosing-environment env))
        )
      )
  )

(define (locally-make-unbound! exp env)
  (let ((varname (defined/unbound-variable exp)) (frame (first-frame env)))
    (let ((frame-vars (frame-variables frame))
          (frame-vals (frame-values frame)))
      ;; return values list without the corresponding entry for exp.  Since
      ;; values are not necessarily unique, a custom search must be done. var
      ;; can only be found at most once, so once found the rest of vals can be
      ;; immediately appended.
      (define (vals-without-var vars vals var)
        (cond ((null? vars) '())  ;; var not in vars, not unbinding anything
              ((equal? var (car vars)) (cdr vals))  ;; skip car vals - remove
              (else (cons (car vals)
                          (vals-without-var (cdr vars) (cdr vals) var)))
              )
        )
     
      (set-car! frame (remove-val varname frame-vars))
      (set-cdr! frame (vals-without-var
                       frame-vars
                       frame-vals
                       varname))
      varname
      )
    )
  )

;; hw2 functions for remove-val

(define (filter z pred)
  (if (null? z)
      '()    ; Done iterating, end new list
      (if (pred (car z)) ; keeping element depends on pred
          (cons (car z) (filter (cdr z) pred))
          (filter (cdr z) pred))))

(define (remove-val value oldList)
  (filter oldList (lambda (z) (not (equal? z value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The initial environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is initialization code that is executed once, when the the
;;; interpreter is invoked.

;; the-dynamic-environment will be extended in setup-environment

(define the-dynamic-environment '())

(define (setup-environment)
  (let ((initial-env
         (xtend-environment '()
                            '()
                            the-empty-environment)))
    initial-env))

;; hw2: for-each to install each primitive procedure.

(define (my-for-each proc seq)
  (cond ((not (null? seq))   ; cond needed here for two expressions
         (proc (car seq)) (my-for-each proc (cdr seq))) ))

(define (install-primitives)
  (my-for-each
   (lambda (primitive-entry)
     (install-primitive-procedure
      (car primitive-entry)
      (cadr primitive-entry)))
   primitive-procedures
   )
  )

;;; Define the primitive procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'display display)
        (list 'newline newline)
        (list 'assoc assoc)
        (list 'member member)
        (list 'zero? zero?)
        ))

(define (install-primitive-procedure name action)
  (if (lookup-action name)  ;; special form check
      (s450error "SPECIAL FORM CANNOT BE PRIMITIVE: " name)
      (begin
        (add-binding-to-frame!
         name
         (list 'primitive action)
         (first-frame the-global-environment))
        name)
      )
  )

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;; Here is where we rely on the underlying Scheme implementation to
;;; know how to apply a primitive procedure.

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Streams
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; for a special form, exp and env must be the arguments.
;;; exp is (cons-stream elmt strm), so 2nd & 3rd values are wanted.

(define (cons-stream exp env)
  (cons (cadr exp) (make-thunk (caddr exp) env)))

(define (stream-car strm) (car strm))
(define (stream-cdr strm) (force-thunk (cdr strm)))
(define the-empty-stream '())
(define (stream-null? strm) (eq? strm the-empty-stream))

;;; for user-print
(define (cons-stream? object) (and (pair? object) (thunk? (cdr object))))

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

(define next-prompt '())
(define end 'not-continuation-yet)

(define prompt? #t)

;; the first time this is called, prompting is byapssed so the end continuation
;; is set properly. Otherwise, exiting as the first expression leads to a
;; non-s450 error.

(define (s450)
  (call/cc (lambda (here) (set! next-prompt here))) ;; recover from error here
  
  (if (not (eq? end 'not-continuation-yet))
      (begin
        (prompt-for-input input-prompt)
        (let ((input (read)))
          (let ((output (xeval input the-global-environment)))
            (user-print output)))))

  (call/cc (lambda (here) (set! end here)))  ;; skip prompting (used in (exit))
  (if prompt?
      (s450)
      (begin
        (set! prompt? #t)  ;; to reinvoke (s450) properly without needing to
                           ;; reload this file.
        (newline)
        (display "Exiting the s450 interpreter."))))

(define s450error
  (lambda args  ;; 0 or more args, all in a list
    (display "s450error: ")
    (my-for-each display args)
    (next-prompt '())  ;; argument ignored
    )
  )

;; now set the truth value to #f, skipping the next (s450) and ending the
;; program.
(define (exit) (set! prompt? #f) (end '()))

(define (prompt-for-input string)
  (newline) (newline) (display string))

;;; Note that we would not want to try to print a representation of any
;;; <env>s below -- this would in general get us into an
;;; infinite loop.

(define (user-print object)
  (cond ((user-defined-procedure? object)
         (display (list 'user-defined-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>)))
        ((thunk? object)
         (display (list 'thunk (thunk-exp object) '<thunk-env>)))
        ((cons-stream? object)
         (display
          (cons
           (car object)
           (cons 'PROMISE (thunk-exp (cdr object))))))
        (else (display object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Here we go:  define the global environment and invite the
;;;        user to run the evaluator.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define the-global-environment (setup-environment))
(install-primitives)

(install-special-form 'defined? defined?)
(install-special-form 'locally-defined? locally-defined?)
(install-special-form 'make-unbound! make-unbound!)
(install-special-form 'locally-make-unbound! locally-make-unbound!)
(install-special-form 'cons-stream cons-stream)
(install-primitive-procedure 'stream-car stream-car)
(install-primitive-procedure 'stream-cdr stream-cdr)
(install-primitive-procedure 'stream-null? stream-null?)

(display "... loaded the metacircular evaluator. (s450) runs it.")
(newline)

;;;  load.s450
;;;
;;;  install special form (load "filename") in s450 evaluator
;;;

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

(install-special-form 'load eval-load)

