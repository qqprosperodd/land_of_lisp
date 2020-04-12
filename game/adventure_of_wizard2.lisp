(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
            (list 'quote x)))
      (cons  (car cmd) (mapcar #'quote-it (cdr cmd))))))
;

(defparameter *allowed-commands* '(look walk pickup inventory))
;

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))
;

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
;

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                      (prin1-to-string lst))
                              'list)
                    t
                    nil)
            'string))
  (fresh-line))
;
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;;;;;
(game-repl)
look
You are in the living room. A wizard is snoring loudly on the couch. There is
a door going west from here. There is a ladder going upstairs from here. You
see a whiskey n the floor. You see a bucket n the floor.
walk east
You cannot go that way.
walk west
You are in a beautiful garden. There is a well in front of you. There is a
door going east from here. You see a frog n the floor. You see a chain n the floor.
pickup chain
You are now carrying the chain
scratch head
I do not know that command.
pickup chicken
You cannot get that.
walk east
You are in the living room. A wizard is snoring loudly on the couch. There is
a door going west from here. There is a ladder going upstairs from here. You
see a whiskey n the floor. You see a bucket n the floor.
walk upstairs
You are in the attic. There is a giant welding torch in the corner. There is a
ladder going downstairs from here.
inventory
Items- chain
walk china
You cannot go that way.
walk downstairs
You are in the living room. A wizard is snoring loudly on the couch. There is
a door going west from here. There is a ladder going upstairs from here. You
see a whiskey n the floor. You see a bucket n the floor.
pickup bucket
You are now carrying the bucket
look
You are in the living room. A wizard is snoring loudly on the couch. There is
a door going west from here. There is a ladder going upstairs from here. You
see a whiskey n the floor.
quit  
