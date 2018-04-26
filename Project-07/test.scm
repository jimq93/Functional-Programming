(begin
   (define divider "
--------------------------------------------------------
")

(load "s450.scm")

(define (install-primitive-procedure name action)
  (add-binding-to-frame! name
(list 'primitive action)
(first-frame the-global-environment))
  name)


(install-primitive-procedure 'xdisplay (lambda (x)
(display x)
(newline)))

(install-primitive-procedure '*plus* +)
(install-primitive-procedure '*minus* -)
(install-primitive-procedure '*times* *)
(install-primitive-procedure '*over* /)
(install-primitive-procedure '*equals* =)

(display* divider "test delayed [20]:" #\newline)
) ; begin

(s450)
(begin
  (define (foo x (delayed y))
    (if (*equals* x 3)
1
y))
  (foo 3 (*over* 1 0))
  )
(exit)