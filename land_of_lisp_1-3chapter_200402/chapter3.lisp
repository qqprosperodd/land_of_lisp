; syntax and semantics
; syntax is legal as single sentence.
; semantics is meaningful as single sentence.

; lisp only has () as syntax.
; all code is list.

; symbol is basic data type in lisp.
; symbol never distinguish small/large text. this is far from R.
(eq 'fooo 'Fooo)

; Numbers are double and integer in lisp.
(+ 1 1.0)
(expt 53 53)
; expt is related to exp function. it is based on (b^x = e^x log b)
; exp function returns natural log.

(/ 4 6)
(/ 4.0 6)
; when you divide a(int) by b(int), lisp returns a/b.
; when you divide a(double) by b(int), lisp returns decimal.

; character
(princ "Tutti Frutti")
; read-eval-print (REPL)
; in order to use escape in "", use \" instead of ".
(princ "He yelled \"Stop the thief!\" from the busy street.")

; code mode
; form is first element is command (usually function)
; other elements are argument.
(expt 2 3)
(expt 2 (+ 3 4))

; data mode
; compiler evaluates data mode as data.
'(expt 2 3)

; list
; cons cell (cons is "construct")
(cons 'chicken 'cat)
(cons 'chicken 'nil)
; nil ends evaluating list.
(cons 'chicken ())
(cons 'pork '(beef chicken))
(cons 'pork (cons 'beef (cons 'chicken ())))

; car/cdr
(car '(pork beef chicken))
; car returns first element.
(cdr '(pork beef chicken))
; cdr returns remained elements.
(cadr '(pork beef chicken))
; 2nd -> cadr, 3rd -> caddr
(car '((peas carrots tomatos) (pork beef chicken)))
(cdr '(peas carrots tomatos))
(cdr (car '((peas carrots tomatos) (pork beef chicken))))
(cdar '((peas carrots tomatos) (pork beef chicken)))

(cddr '((peas carrots tomatos) (pork beef chicken) duck))
(caddr '((peas carrots tomatos) (pork beef chicken) duck))
(cddar '((peas carrots tomatos) (pork beef chicken) duck))
(cadadr '((peas carrots tomatos) (pork beef chicken) duck))
; cadadadadadadadadadar
; cadddddr
