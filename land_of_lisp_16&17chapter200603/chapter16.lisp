; macro programming
(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))
(add 2 3)
; 5
;

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
    ,@body))
;
(let ((foo (+ 2 3)))
  (* foo foo))
(let1 foo (+ 2 3)
  (* foo foo))
; A regular Lisp function runs when you execute a program that contains the function
; A macro, on the other hand, runs before the program does,
; when the program is read and compiled by your Lisp environment

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))
;
(macroexpand '(let1 foo (+ 2 3)
                (* foo foo)))
; (LET ((FOO (+ 2 3))) (* FOO FOO))
;
(defun my-length (lst)
  (labels ((f (lst acc)
            (if lst
                (f (cdr lst) (1+ acc))
                acc)))
    (f lst 0)))
;
; Next, we make split macro.
(split '(2 3)
  (format t "This can be split into ~a and ~a." head tail)
  (format t "This cannot be split."))
;
(split '()
  (format t "This can be split into ~a and ~a." head tail)
  (format t "This cannot be split."))
;
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
        ,yes)
      ,no))
; this contains bug.
(defun my-length (lst)
  (labels ((f (lst acc)
            (split lst
              (f tail (1+ acc))
              acc)))
    (f lst 0)))
; (chapter15)
; removed (if)
; split has some bugs
(split (progn (princ "Lisp rocks!")
              '(2 3))
  (format t "This can be split into ~a and ~a." head tail)
  (format t "This cannot be split."))
;
(macroexpand '(split (progn (princ "Lisp rocks!")
                      '(2 3))
                (format t "This can be split into ~a and ~a." head tail)
                (format t "This cannot be split.")))
; princ is evaluated three times.
; update split
(defmacro split (val yes no)
  `(let1 x ,val
    (if x
       (let ((head (car x))
             (tail (cdr x)))
        ,yes)
      ,no)))
;
(split (progn (princ "Lisp rocks!")
              '(2 3))
  (format t "This can be split into ~a and ~a." head tail)
  (format t "This cannot be split."))
; princ is evaluated once.
(let1 x 100
  (split '(2 3)
    (+ x head)
    nil))
;
(macroexpand '(split '(2 3)
                  (+ x head)
                  nil))
;(LET ((X '(2 3)))
;  (IF X
;      (LET ((HEAD (CAR X)) (TAIL (CDR X)))
:        (+ X HEAD))
:      NIL))
;
; split-macro's x conflicts with x.
; not to conflict...
(gensym)
(defmacro split (val yes no)
  (let1 g (gensym)
  `(let1 ,g ,val
    (if ,g
       (let ((head (car ,g))
             (tail (cdr ,g)))
        ,yes)
      ,no))))
;
(macroexpand '(split '(2 3)
                (+ x head)
                nil))
; #:G366
; #:G367
(defun my-length (lst)
  (labels ((f (lst acc)
            (split lst
              (f tail (1+ acc))
              acc)))
    (f lst 0)))
;
; next, make recurse macro!
(recurse (n 9)
  (fresh-line)
  (if (zerop n)
    (princ "lift-off!")
    (progn (princ n)
           (setf (1- n)))))
; result
; 9
; 8
; 7
; 6
; 5
; 4
; 3
; 2
; 1
; lift-off!
(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                (if tail
                    (f (cdr tail) (cons (cons head (car tail)) acc))
                    (reverse acc))
                (reverse acc))))
    (f lst nil)))
;
(pairs '(a b c d e f))
(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
      (self ,@(mapcar #'cdr p)))))
;
(defun my-length (lst)
  (recurse (lst lst
            acc 0)
           (split lst
            (self tail (1+ acc))
            acc)))
; Lispers will do their best to use alternate techniques to macro programming whenever possible
; functional programming, which can also be used to clean up list-eater functions
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))
;  there are still many situations where creating your own Lisp dialect is exactly the right solution to a problem
