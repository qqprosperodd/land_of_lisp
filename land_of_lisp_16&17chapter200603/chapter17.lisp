; DSL (Domain Specific Languages)
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
    ,@body))
;
(defmacro split (val yes no)
  (let1 g (gensym)
  `(let1 ,g ,val
    (if ,g
       (let ((head (car ,g))
             (tail (cdr ,g)))
        ,yes)
      ,no))))
;
(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                (if tail
                    (f (cdr tail) (cons (cons head (car tail)) acc))
                    (reverse acc))
                (reverse acc))))
    (f lst nil)))
; SVG format
<svg xmlns="http://www.w3.org/2000/svg">
  <circle cx="50"
          cy="50"
          r="50"
          style="fill:rgb(255,0,0);stroke:rgb(155,0,0)">
  </circle>
  <circle cx="100"
          cy="100"
          r="50"
          style="fill:rgb(0,0,255);stroke:rgb(0,0,155)">
  </circle>
</svg>
;
<mytag>
    <inner_tag>
    </inner_tag>
</mytag>
;
<mytag color="BLUE" height="9"></mytag>
;
(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))
;
(print-tag 'mytag '((color . blue) (height . 9)) nil)
; <mytag color="BLUE" height="9">
; tag macro
(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                        `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))
;
(tag mytag (color 'blue height (+ 4 5)))
;<mytag color="BLUE" height="9"></mytag>
;
(macroexpand '(tag mytag (color 'blue height (+ 4 5))))
;(PROGN
; (PRINT-TAG 'MYTAG (LIST (CONS 'COLOR 'BLUE) (CONS 'HEIGHT (+ 4 5))) NIL)
; (PRINT-TAG 'MYTAG NIL T))
(tag mytag (color 'blue size 'big)
     (tag first_inner_tag ())
     (tag second_inner_tag ()))
;<mytag color="BLUE" size="BIG"><first_inner_tag></first_inner_tag><second_inner_tag></second_inner_tag></mytag>
; tag can also make html
(tag html ()
     (tag body ()
          (princ "Hello World!")))
;<html><body>Hello World!</body></html>
; tag and body macro
(defmacro html (&body body)
  `(tag html ()
        ,@body))
;
(defmacro body (&body body)
  `(tag body ()
        ,@body))
;
(html
  (body
    (princ "Hello World!")))
;
(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
              "xmlns:xlink" "http://www.w3.org/1999/xlink" height ,height width ,width)
        ,@body))
;
(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))
;
(brightness '(255 0 0) -100)
;
(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))
;
(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))
;
(svg 150 150
     (circle '(50 . 50) 50 '(255 0 0))
     (circle '(100 . 100) 50 '(0 0 255)))
;
(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                style (svg-style color))))
;
(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))
;
(random-walk 100 10)
;
(with-open-file (*standard-output* "C:/Users/masar/OneDrive/画像/ドキュメント/land_of_lisp/random_walk.svg"
                :direction :output
                :if-exists :supersede)
  (svg 400 200 (loop repeat 10
               do (polygon (append '((0 . 200))
                                   (loop for x
                                         for y in (random-walk 100 400)
                                         collect (cons x y))
                                   '((400 . 200)))
                           (loop repeat 3
                                 collect (random 256))))))
; well done in CLISP, I can't do this in SBCL
; add new command in advanture of wizard

(load "C:/Users/masar/OneDrive/画像/ドキュメント/land_of_lisp/game/adventure_of_wizard3.lisp")
(look)
(game-repl)
quit
;
(defun have (object)
  (member object (cdr (inventory))))
;
(defparameter *chain-welded* nil)
(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))
;
(weld 'chain 'bucket)
(game-repl)
weld chain bucket
; I do not know that command.
quit
;
(pushnew 'weld *allowed-commands*)
(game-repl)
weld chain bucket
; You cannot weld like that.
quit
;
(defparameter *bucket-filled* nil)
(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
      (progn (setf *bucket-filled* 't)
             '(the bucket is now full of water))
      '(you cannot dunk like that.)))
;
(pushnew 'dunk *allowed-commands*)
;
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
                '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))
;
(defparameter *chain-welded* nil)
(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
                        '(you do not have a bucket.)))
;
(defparameter *bucket-filled* nil)
(game-action dunk bucket well garden
             (if *chain-welded*
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water.))
                        '(the water level is too low to reach.)))
;
(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket have nothing in it.))
        ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                        he is so upset he banishes you to the
                        netherworlds- you lose! the end.))
        (t '(the wizard awakens from his slumber and greets you warmly.
             he hands you the magic low-crab donut- you win! the end.))))
;;;;
(game-repl)
look
You are in the living room. A wizard is snoring loudly on the couch. There is
 a door going west from here. There is a ladder going upstairs from here. You
 see a whiskey n the floor. You see a bucket n the floor.
pickup bucket
You are now carrying the bucket
pickup whiskey
You are now carrying the whiskey
inventory
Items- whiskey bucket
walk upstairs
You are in the attic. There is a giant welding torch in the corner. There is a
 ladder going downstairs from here.
walk east
You cannot go that way.
walk downstairs
You are in the living room. A wizard is snoring loudly on the couch. There is
 a door going west from here. There is a ladder going upstairs from here.
walk west
You are in a beautiful garden. There is a well in front of you. There is a
 door going east from here. You see a frog n the floor. You see a chain n the
 floor.
dunk bucket well
the water level is too low to reach.
pickup chain
You are now carrying the chain
walk east
You are in the living room. A wizard is snoring loudly on the couch. There is
 a door going west from here. There is a ladder going upstairs from here.
splash bucket wizard
the bucket have nothing in it.
