;; hw8/test.scm
;;
;; Execute as follows, inside a typescript:
;;
;;     scheme < test.scm
;;
;; You can then run the typescript through filter_scheme to eliminate
;; some of the garbage.

(begin
   (define divider "
--------------------------------------------------------
")

   (load "regsim.scm")

   (display* divider "Problem 5.8:  check for multiply defined label")

   (define check5.8
     (make-machine
      '(a)
      '()
      '(
        start
         (goto (label here))
        here
         (assign a (const 3))
         (goto (label there))
        here
         (assign a (const 4))
         (goto (label there))
        there)))
   )

(begin
  (display* divider "Problem 5.19:  breakpoints")

  (define gcd-machine
    (make-machine
     '(a b t)
     (list (list 'rem remainder) (list '= =))
     '(test-b
        (test (op =) (reg b) (const 0))
        (branch (label gcd-done))
        (assign t (op rem) (reg a) (reg b))
        (assign a (reg b))
        (assign b (reg t))
        (goto (label test-b))
       gcd-done)))

  (set-register-contents! gcd-machine 'a 206)

  (set-register-contents! gcd-machine 'b 240)
  )

(set-breakpoint gcd-machine 'test-b 4)

(start gcd-machine)

(get-register-contents gcd-machine 't)

(proceed-machine gcd-machine)

(get-register-contents gcd-machine 'a)

(set-register-contents! gcd-machine 'a 40)
(set-breakpoint gcd-machine 'test-b 1)
(cancel-breakpoint gcd-machine 'test-b 4)

(proceed-machine gcd-machine)

(get-register-contents gcd-machine 'a)

(begin
  (set-breakpoint gcd-machine 'test-b 4)
  (cancel-all-breakpoints gcd-machine)
  (proceed-machine gcd-machine)
  )
(get-register-contents gcd-machine 'a)

(begin
  (load "regsim_5.9.scm")

  (display* divider
            "Problem 5.9: using a label as an operand to a built-in operation"
            #\newline)

  (define check5.9
    (make-machine
     '(a)
     (list (list '+ +))
     '(start
       (assign a (op +) (label start) (const 1)))))
  )
