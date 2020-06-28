(load "C:/Users/masar/OneDrive/画像/ドキュメント/land_of_lisp/land_of_lisp_18chapter0608/dice_of_doom.lisp")
;

(load "C:/Users/masar/OneDrive/画像/ドキュメント/land_of_lisp/land_of_lisp_18chapter0608/lazy.lisp")
;

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player
                                                (1- spare-dice))
                                  (mod (1+ player) *num-players*)
                                  0
                                  t))
                 moves)))
;

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
		   (car (aref board pos)))
	   (dice (pos)
		 (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst)
                                cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board
                                                        cur-player
                                                        src
                                                        dst
                                                        (dice src))
                                          cur-player
                                          (+ spare-dice (dice dst))
                                          nil))))
                (lazy-nil)))
            (make-lazy (neighbors src)))
         (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
                      collect n)))))
;

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
	(unless (lazy-null moves)
	(let* ((move (lazy-car moves))
	(action (car move)))
	(fresh-line)
	(format t "~a. " n)
	(if action
	(format t "~a -> ~a" (car action) (cadr action))
	(princ "end turn")))
	(print-moves (lazy-cdr moves) (1+ n)))))
	(print-moves moves 1))
  (fresh-line)
  (cadr (lazy-nth (1- (read)) moves))))
;

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

;At this point, you can play a game vs a human with:
; (play-vs-human (game-tree (gen-board) 0 0 t))
;
;Now we start writing improvements for the AI...

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil)
	  (lazy-mapcar (lambda (move)
			 (list (car move)
                               (limit-tree-depth (cadr move) (1- depth))))
		       (caddr tree)))))
;

(defparameter *ai-level* 4)
;

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))
;

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
      ((zerop (car tree)) (play-vs-computer (handle-human tree)))
      (t (play-vs-computer (handle-computer tree)))))
;

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
                (if (threatened pos board)
                    1
                  2)
              -1)))
;

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (car nhex))
                    (ndice (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))
;

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))
;

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(apply (if (eq (car tree) player)
		   #'max
		 #'min)
	       (get-ratings tree player))
      (score-board (cadr tree) player))))

;You can now play a game against the computer AI:
;
;(play-vs-computer (game-tree (gen-board) 0 0 t))
;
;The rest of this file implements ab pruning.

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
           (unless (lazy-null moves)
             (let ((x (ab-rate-position (cadr (lazy-car moves))
                                        player
                                        upper-limit
                                        lower-limit)))
               (if (>= x upper-limit)
                   (list x)
                 (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))
;

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
           (unless (lazy-null moves)
             (let ((x (ab-rate-position (cadr (lazy-car moves))
                                        player
                                        upper-limit
                                        lower-limit)))
               (if (<= x lower-limit)
                   (list x)
                 (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))
;

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(if (eq (car tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
          (apply #'min (ab-get-ratings-min tree
                                           player
                                           upper-limit
                                           lower-limit)))
      (score-board (cadr tree) player))))
;

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))
;

(defparameter *board-size* 5)
;

(defparameter *board-hexnum* (* *board-size* *board-size*))
;

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
        (code-char code)
        default)))


(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#\% (cons (http-char (cadr lst) (caddr lst))
                             (f (cdddr lst))))
                  (#\+ (cons #\space (f (cdr lst))))
                  (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))


(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))


(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
          (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))


(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
         (when i
            (cons (intern  (string-upcase (subseq s 0 i)))
                  (subseq s (+ i 2)))))))
  (when h
    (cons h (get-header stream)))))


(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))


(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
      (loop (with-open-stream (stream (socket-accept socket))
              (let* ((url    (parse-url (read-line stream)))
                     (path   (car url))
                     (header (get-header stream))
                     (params (append (cdr url)
                                     (get-content-params stream header)))
                     (*standard-output* stream))
                (funcall request-handler path header params))))
      (socket-server-close socket))))

;
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
    (if (not name)
        (format t "HTTP/1.1 200 OK~%~%<html><form>What is your name?<input name='name' /></form></html>")
        (format t "HTTP/1.1 200 OK~%~%<html>Nice to meet you, ~a!</html>" (cdr name))))
    (format t "HTTP/1.1 404 Not Found~%~%Sorry... I don't know that page.")))

; see http://practical-scheme.net/wiliki/wiliki.cgi/Shiro:LandOfLisp

;
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

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))
;

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
             "xmlns:xlink" "http://www.w3.org/1999/xlink"
             height ,height
             width ,width)
	,@body))
;

(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))
;

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))
;

(defun polygon (points color)
  (tag polygon (points (format nil
			       "~{~a,~a ~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		style (svg-style color))))
;

(defparameter *board-width* 900)
;

(defparameter *board-height* 500)
;

(defparameter *board-scale* 64)
;

(defparameter *top-offset* 3)
;

(defparameter *dice-scale* 40)
;

(defparameter *dot-size* 0.05)
;

(defun draw-die-svg (x y col)
  (labels ((calc-pt (pt)
            (cons (+ x (* *dice-scale* (car pt)))
                  (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
              (polygon (mapcar #'calc-pt pol) col)))
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
        (brightness col 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
        col)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
        (brightness col -40))
    (mapc (lambda (x y)
            (polygon (mapcar (lambda (xx yy)
                              (calc-pt (cons (+ x (* xx *dot-size*))
                                             (+ y (* yy *dot-size*)))))
                             '(-1 -1 1 1)
                             '(-1 1 1 -1))
                     '(255 255 255)))
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
          -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))
;


(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop for z below 2
        do (polygon (mapcar (lambda (pt)
                              (cons (+ xx (* *board-scale* (car pt)))
                                    (+ yy (* *board-scale*
                                             (+ (cdr pt) (* (- 1 z) 0.1))))))
                            '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                              (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                    (if (eql pos chosen-tile)
                        (brightness col 100)
                        col)))
  (loop for z below (second hex)
        do (draw-die-svg (+ xx
                             (* *dice-scale*
                                0.3
                                (if (oddp (+ x y z))
                                    -0.3
                                    0.3)))
                          (- yy (* *dice-scale* z 0.8)) col)))
;

(defparameter *die-colors* '((255 63 63) (63 63 255)))
;

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
        do (loop for x below *board-size*
                for pos = (+ x (* *board-size* y))
                for hex = (aref board pos)
                for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
                for col = (brightness (nth (first hex) *die-colors*)
                                      (* -15 (- *board-size* y)))
                do (if (or (member pos legal-tiles) (eql pos chosen-tile))
                       (tag g ()
                         (tag a ("xlink:href" (make-game-link pos))
                              (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                       (draw-tile-svg x y pos hex xx yy col chosen-tile)))))
;

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))
;

;; make web interface
(defparameter *cur-game-tree* nil)
;

(defparameter *from-tile* nil)
;

(defun dod-request-handler (path header params)
  (if (equal path "game.html")
  (progn (princ "HTTP/1.1 200 OK")
      (terpri)
      (terpri)
      (princ "<!doctype html>")
             (tag center ()
                  (princ "Welcome to DICE OF DOOM!")
                  (tag br ())
                  (let ((chosen (assoc 'chosen params)))
                    (when (or (not *cur-game-tree*) (not chosen))
                      (setf chosen nil)
                      (web-initialize))
                    (cond ((lazy-null (caddr *cur-game-tree*))
                              (web-announce-winner (cadr *cur-game-tree*)))
                          ((zerop (car *cur-game-tree*))
                              (web-handle-human
                                (when chosen
                                  (read-from-string (cdr chosen)))))
                          (t (web-handle-computer))))
                  (tag br ())
                  (draw-dod-page *cur-game-tree* *from-tile*))
               (princ "</html>"))
      (princ "Sorry... I don't know that page.")))
;

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))
;

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (princ " play again")))
;

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                              (cadr (lazy-car (caddr *cur-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
              (princ "continue")))
        ((not *from-tile*) (setf *from-tile* pos)
                           (princ "Now choose destination:"))
        ((eq pos *from-tile*) (setf *from-tile* nil)
                              (princ "Move cancelled."))
        (t (setf *cur-game-tree*
                 (cadr (lazy-find-if (lambda (move)
                                       (equal (car move)
                                              (list *from-tile* pos)))
                                     (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
                (princ "pass"))
           (princ " or make another move:"))))
;

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
       (princ
        "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))
;

(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
       *board-height*
       (draw-board-svg (cadr tree)
                       selected-tile
                       (take-all (if selected-tile
                                     (lazy-mapcar
                                       (lambda (move)
                                         (when (eql (caar move)
                                                    selected-tile)
                                           (cadar move)))
                                       (caddr tree))
                                     (lazy-mapcar #'caar (caddr tree)))))))
;
