(loop for i
      below 5
      sum i)
; loop is different in kind.
(loop for i
      from 5
      to 10
      sum i)
; ilst
(loop for i
      in '(100 20 3)
      sum i)
; apply is better.
;
(loop for i
      below 5
      do (print i))
; if
(loop for i
      below 10
      when (oddp i)
      sum i)
; exit loop
(loop for i
      from 0
      do (print i)
      when (= i 5)
      return 'falafel)
; to list
(loop for i
      in '(2 3 4 5 6)
      collect (* i i))
; multiple for
(loop for x below 10
      for y below 10
      collect (+ x y))
;
(loop for x below 10
      collect (loop for y below 10
                    collect (+ x y)))
(loop for i
      from 0
      for day
      in '(monday tuesday wednesday thursday friday saturday sunday)
      collect (cons i day))
;;
; using loop to evolve
(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
(defstruct animal x y energy dir genes)
(defparameter *animals*
  (list (make-animal :x      (ash *width* -1)
                     :y      (ash *height* -1)
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8
                                   collecting (1+ (random 10))))))
; animals parameter
; ash : bit shift
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1)))
                                *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0)))
                                *height*))
    (decf (animal-energy animal))))
;
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
              (let ((xnu (- x (car genes))))
                (if (< xnu 0)
                    0
                    (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))
;
(defparameter *reproduction-energy* 200)
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))
; copy-structure may cause hidden bugs.
(defparameter *parent* (make-animal :x 0
                                    :y 0
                                    :energy 0
                                    :dir 0
                                    :genes (list 1 1 1 1 1 1 1 1)))
(defparameter *child* (copy-structure *parent*))
(setf (nth 2 (animal-genes *parent*)) 10)
*parent*
*child*
; *child* is also changed.
; so use copy-list for genes.
(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                              (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))
;
(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                (and (= (animal-x animal) x)
                                                     (= (animal-y animal) y)))
                                               *animals*)
                                          #\M)
                                        ((gethash (cons x y) *plants*) #\*)
                                        (t #\space))))
                  (princ "|"))))
; 2 loop. animal -> M, plant -> *
; this function is very slow because cond is evaluated height*width times.
; user interface function, evolution uses draw-world once, so this slow function never delay run time.
; user-interface
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
              (if x
                  (loop for i
                        below x
                        do (update-world)
                        if (zerop (mod i 1000))
                        do (princ #\.))
                      (update-world))
                  (evolution))))))
; first user press/type quit, enter or number.
; quit -> kill this function
; enter -> one day
; this function press . by 1000 loop.
(evolution)
100
5000000
quit
*animals*
