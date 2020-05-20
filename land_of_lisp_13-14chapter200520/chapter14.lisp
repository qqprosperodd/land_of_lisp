; The function always returns the same result, as long as the same arguments are passed into it. (This is often referred to as referential transparency.)
; The function never references variables that are defined outside the function, unless we are certain that these variables will remain constant.
; No variables are modified (or mutated, as functional programmers like to say) by the function.
; The purpose of the function is to do nothing other than to return a result.
; The function doesn’t do anything that is visible to the outside world, such as pop up a dialog box on the screen or make your computer go “Bing!”
; The function doesn’t take information from an outside source, such as the keyboard or the hard drive.

(sin 0.5)

; clean
(defun add-widget (database widget)
  (cons widget database))
; dirty
(defparameter *database* nil)
(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
        (setf *database* (add-widget *database* (read)))
        (format t "The database contains the following: ~a~%" *database*)))
(main-loop)
; type ctrl+C to escape infinite loop.
;;
; Higher-Order Programming
(defparameter *my-list* '(4 7 2 3))
; Imperative Code
(loop for n below (length *my-list*)
      do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))
*my-list*
; functional Programming
(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))))
(add-two '(4 7 2 3))
; Higher-order Programming
(mapcar (lambda (x)
          (+ x 2))
        '(4 7 2 3))
