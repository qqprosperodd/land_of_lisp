; chapter5
(defparameter *nodes* '((living-room (you are in the living room.
                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                          there is a well in front of you.))
                        (attic (you are in the attic.
                          there is a giant welding torch in the corner.))))
; *nodes* is alist (association list) (see detail at chapter7)
; why character is not used is because symbol and list is most useful in lisp programming.
(assoc 'garden *nodes*)
; assoc refer to alist. it uses eql.

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
(describe-location 'living-room *nodes*)

(defparameter *edegs* '((living-room (garden west door)
                                    (attic upstairs ladder))
                      (garden (living-room east door))
                      (attic (living-room downstairs ladder))))
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
(describe-path '(garden west door))
; ` is called quasiquote.
; ,() is code mode

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
(describe-paths 'living-room *edges*)
; describe-paths works bellow:
; 1, find edge associated
; 2, transform the edge to the description
; 3, binds those description

(cdr (assoc 'living-room *edges*))
; ((garden west door) (attic upstairs ladder))
(mapcar #'describe-path ((garden west door) (attic upstairs ladder)))
; ((there is a door going west from here.) (there is a ladder going upstairs from here.))
(mapcar #'sqrt (1 2 3 4 5))
; mapcar multiply function to arguments.
; mapcar is called higher-order function.
(mapcar #'car '((foo bar) (baz qux)))
; #' is function operator
(mapcar (function car) '((foo bar) (baz qux)))
; same
(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux))))
; first car is local argument.
; second car is function.
; Does conrad have Honda Civic?
; Scheme shares function and argument of name.
; So, Scheme can't use same-name function and argument at the same time.

(append '(mary had) '(a) '(little lamb))
; append binds lists into single list.
(apply #'append '((mary had) (a) (little lamb)))
; apply returns single value.
; mapcar returns list.
(apply #'append '((there is a door going west from here.)
                  (there is a ladder going upstairs from here.)))

; object that is visible.
(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                  (bucket living-room)
                                  (chain garden)
                                  (frog garden)))
;
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
            (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(objects-at 'living-room *objects* *object-locations*)
; (whiskey bucket)

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
              `(you see a ,obj n the floor.)))
      (apply #'append (mapcar #'describe-obj (obects-at loc objs obj-loc)))))

(describe-objects 'living-room *objects* *object-locations*)

; binds three looking function. (describe-location, describe-paths describe-objects)
(defparameter *location* 'living-room)
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
(look)
; look function refer to global argument. so this is not functional.

(defun walk (direction)
  (let ((next (find direction
                  (cdr (assoc *location* *edges*))
                  :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
                (look))
        '(you cannot go that way.))))
;
(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)
; (3 y)
; :key is
(walk 'west)

(defun pickup (object)
  (cond ((member object
                  (objects-at *location* *objects* *object-locations*))
        (push (list object 'body) *object-locations*)
        `(you are now carrying the ,object))
        (t '(you cannot get that.))))
; member function detects a element in the list.
; push function
(defparameter *foo* '(1 2 3))
(push 7 *foo*)
*foo*
; (7 1 2 3)
; push put argument in first element of list.
; = (setf *foo* (cons 7 *foo*))
; different from push in perl.
; you can reload the alist if you use push - assoc.

(walk 'east)
(pickup 'whiskey)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
(inventory)
