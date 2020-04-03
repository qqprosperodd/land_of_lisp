; program1
; guess-my-number : select one number(1~100)
; smaller : select less number than guess-my-number
; bigger : select more number than guess-my-number

(defparameter *small* 1)
(defparameter *big* 100)
; global argument

; function (defparameter) can be overwritten.
(defparameter *foo* 5)
(defparameter *foo* 6)
; function (defvar) can't be overwritten.
(defvar *foo* 5)
(defvar *foo* 6)

; space & tab, newline are not evaluated.
; (this is same for R)

(defun guess-my-number ()
    (ash (+ *small* *big*) -1))

; 101 (decimal number) = 1100101 (binary number)
; ash (101 -1) = 110010 = 50 (decimal number)
; ash multiply 2^argument.
(ash 22 1) ;44
(ash 11 -1) ; 5

(defun smaller ()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))
(defun bigger ()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))
; 1- is function. it calculates (x - 1).
; these functions returns new *big*/guess-my-number or *small*/guess-my-number

; start-over function
; that resets global argument.

(defun start-over ()
    (defparameter *small* 1)
    (defparameter *big* 100)
    (guess-my-number))

; local argument
; (let () ~~~)
(let ((a 5) (b 6))
  (+ a b))

; local functions
; (flet ((fun ()) ~~~)  ~~~)

(flet ((f (n)
          (+ n 10)))
    (f 5))
; one flet can defun over 2 functions.

(flet ((f (n) (+ n 10)))
      ((g (n) (- n 3)))
      (g (f 5)))

; labels
(labels ((a (n) (+ n 5))
        (b (n) (+ (a n) 6)))
    (b 10))
; flet can't do this.
; flet can't refer to outer function.
; labels is called recursion function.
; flet is unnecessary?
