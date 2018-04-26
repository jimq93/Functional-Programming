;;;  hw6/test.scm
;;;
;;;  script to test student's s450
;;;  run from student project directory; invoke as
;;;          scheme < test.scm
;;;  For best results, run this inside a transcript.  That is,
;;;
;;;      script aaa
;;;      scheme < test.scm
;;;      exit
;;;
;;;  Then run the file aaa through filter_scheme.
;;;

(begin
   (define divider "
--------------------------------------------------------
")
) ; begin

(load "s450.scm")

;;; This is just a hack until we get a clean exit in the next assignment:

(begin
  (install-special-form 'exit-hack (lambda (exp env) 
    (display "leaving s450")(newline)
    (break)))

  (display* divider "test if: <Special form: if>" #\newline))
(s450)
   if
   (exit-hack)


(display* divider "test lambda: <right-lambda>" #\newline)
(s450)
   ((lambda (x) (car x)) '(right-lambda wrong-lambda))
   (exit-hack)

(display* divider "test that global environment doesn't get overwritten.")
(begin
  (define a3gb the-global-environment)
  (install-primitive-procedure 'dummy (lambda (x) x))
  (display* "#t " (eq? the-global-environment a3gb)))

(begin
  (install-primitive-procedure 'show
     (lambda (answer _arg) (display answer) (display " ") _arg))

  (display* divider "test defined?" #\newline))
(s450)
   (begin
     (define x 3)
     (show 3 x ))
   (show "#t" (defined? x))
   (show "#f" (defined? y))
   (show "#t" ((lambda() (defined? x))))
   (show "#t" ((lambda() (define z 6) (defined? z))))
   (exit-hack)

(display* divider "test locally-defined?:" #\newline)
(s450)
   (begin
     (define x 3)
     (show 3 x ))
   (show "#t" (locally-defined? x))
   (show "#f" (locally-defined? y))
   (show "#f" ((lambda() (locally-defined? x))))
   (show "#t" ((lambda() (define z 6) (locally-defined? z))))
   (show "#t" ((lambda(arg) (locally-defined? arg)) 1))
   (exit-hack)

(display* divider "test make-unbound!" #\newline)
(s450)
   (begin
     (define x 3)
     (show "#t" (defined? x)))
   (begin
     (make-unbound! x)
     (show "#f" (defined? x)))
   (show "#t" ((lambda() (define x 6) (defined? x))))
   (begin
     (define x 9)
     (show "#t" (defined? x)))
   (show "#f" ((lambda() (make-unbound! x) (defined? x))))
   (show "#f" (defined? x))
   (exit-hack)

(display* divider "test locally-make-unbound!" #\newline)
(s450)
   (begin
     (define x 3)
     (show "#t" (defined? x)))
   (begin
     (locally-make-unbound! x)
     (show "#f" (defined? x)))
   (show "#t" ((lambda() (define x 6) (defined? x))))
   (begin
     (define x 9)
     (show "#t" (defined? x)))
   (show "#t" ((lambda() (locally-make-unbound! x) (defined? x))))
   (show "#t" (defined? x))
   (show "#f" ((lambda(arg) 
		 (locally-make-unbound! arg)
		 (locally-defined? arg)) 
	       1))
   (exit-hack)


(begin
  (install-special-form 'echo
			(lambda (exp env) (list 'called 'echo (cadr exp))))
  (display* divider "install special form;" #\newline
            "    answer should be (called echo hello)"
            #\newline))
(s450)
   (echo hello)
   (exit-hack)

;; (display* divider
;; 	  "install special form xyz inside s450 (optional);" #\newline
;; 	  "    answer should be (always return xyz)"
;; 	  #\newline)
;; (s450)
;;     (begin
;;       (install-special-form 'xyz (lambda (exp env) '(always return xyz)))
;;       (xyz a b c))
;;     (exit-hack)

(begin
  (install-primitive-procedure '*plus* +)
  (display* divider "test *plus*;" #\newline
            "    answers should be 3 6 0" #\newline))
(s450)
    (*plus* 1 2)
    (*plus* 1 2 3)
    (*plus*)
    (exit-hack)

(begin
  (define shout (lambda (x) (cons x '!)))
  (install-primitive-procedure 'shout shout)
  (display* divider "install primitive procedure shout;" #\newline
            "    answer should be <( hooray . !)>"
	    #\newline))
(s450)
    (shout 'hooray)
    (exit-hack)

;(display* divider "install primitive procedure question - inside (optional)" 
;	  #\newline
;	  "xx question ( why . ?) 3 (primitive ...)" #\newline)

;(define question (lambda (x) (cons x '?)))

;(s450)
;   (define xx 3)
;   (install-primitive-procedure 'question question)
;   (question 'why)
;   xx
;   question
;   (exit-hack)


(display* divider "redefine car;  answers should be as follows:" #\newline
"     (primitive < primitive: car Pair >)" #\newline
"     car" #\newline
"     (user-defined-procedure (x) ((cdr x)) [procedure-env])" #\newline
"     (right-newcar)" #\newline)

(s450)
   car
   (define car (lambda (x) (cdr x)))
   car
   (car '(wrong-newcar right-newcar))
   (exit-hack)

(display* divider "error -- reinstalling a special form:" #\newline)
(install-special-form 'if (lambda (exp env) 3))

(display* divider
	  "error -- giving a defined name to a new special form:"
	  #\newline)
(install-special-form 'car (lambda (exp env) 3))

(display* divider "error -- defining a special form name:" #\newline)
(s450)
   (define if 7)
   (exit-hack)

(display* divider
          "error -- installing a primitive procedure with "
          "the name of a special form:"
          #\newline)
(install-primitive-procedure 'if eval-if)
(display* divider #\newline)
