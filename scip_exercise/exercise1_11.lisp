; Exercise 1.11: A function f is defined by the rule that
; f (n) = n if n < 3, f (n − 1) + 2f (n − 2) + 3f (n − 3) if n ≥ 3.
; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.

(defun f (n)
  (cond ((< n 3) n)
        (t (apply #'+ (list (f (- n 1))
                                (* (f (- n 2)) 2)
                                (* (f (- n 3)) 3))))))
(f 4)
(f 2)
(f 18)
