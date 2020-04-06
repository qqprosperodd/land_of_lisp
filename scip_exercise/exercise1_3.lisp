; Exercise 1.3: Define a procedure that takes three numbers
; as arguments and returns the sum of the squares of the two
; larger numbers.

(defparameter *A* '(5 3 8))
(defun square (n) (* n n))
(defparameter *B* (mapcar #'square *A*))
(defparameter *C* (sort *B* #'<))
(defparameter *D* (cdr *C*))
(defparameter *E* (apply #'+ *D*))
; this is one of the solution.

(defun procedure2 (A)
  (apply #'+ (cdr (sort (mapcar (lambda (n) (* n n)) A) #'<))))
(procedure2 '(4 6 8))
; this is better?

; if argument length =3
(defun procedure3 (A)
  (cond ((eq (length A) 3) (apply #'+ (cdr (sort (mapcar (lambda (n) (* n n)) A) #'<))))
          (t '(list length must be 3.))))

(procedure3 '(4 6 8))
(procedure3 '(4 6 8 2))
