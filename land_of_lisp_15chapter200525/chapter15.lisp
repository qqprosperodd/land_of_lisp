(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

; clean
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))
; dirty
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))
;
(gen-board)
; clean
(defun player-letter (n)
  (code-char (+ 97 n)))
;
(player-letter 1)
; dirty
(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                              (second hex))))))
;
(draw-board #((0 3) (0 3) (1 3) (1 1)))
; clean
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))
; clean
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))
; clean
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
            (car (aref board pos)))
           (dice (pos)
            (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
              (mapcan (lambda (dst)
                        (when (and (not (eq (player dst) cur-player))
                              (> (dice src) (dice dst)))
                        (list
                          (list (list src dst)
                                (game-tree (board-attack board cur-player
                                                         src dst (dice src))
                                           cur-player
                                           (+ spare-dice (dice dst))
                                           nil)))))
                      (neighbors src))))
      (loop for n below *board-hexnum*
            collect n))))
; clean
(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                            (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                            (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))
;
(neighbors 2)
; clean
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))
;
(board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)
; clean
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
            (cond ((null lst) nil)
                  ((zerop n) lst)
                  (t (let ((cur-player (caar lst))
                           (cur-dice (cadar lst)))
                      (if (and (eq cur-player player) (< cur-dice *max-dice*))
                          (cons (list cur-player (1+ cur-dice))
                                (f (cdr lst) (1- n)))
                          (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))
;
(add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2)
;
(game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
; dirty
(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))
; dirty
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))
; dirty
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
              (fresh-line)
              (format t "~a. " n)
              (if action
                  (format t "~a -> ~a" (car action) (cadr action))
                  (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))
; clean
(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                          (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                        (not (eq (cdr x) best)))
                       totals))))
; dirty
(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w))))))
;
(play-vs-human (game-tree (gen-board) 0 0 t))
; human vs human


; clean
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))
; clean
(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))
; clean
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))
; dirty
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))
; human vs computer
(play-vs-computer (game-tree (gen-board) 0 0 t))
;

; make faster

(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

; closure
(defparameter *foo* (lambda ()
                      5))
;
(funcall *FOO*)
;
(defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))
;
(funcall *foo*)
;
(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))
(my-print "this")
(my-print "is")
(my-print "a")
(my-print "test")
; Because the line-number variable is captured in the closure,
; it can “live on” between calls to my-print, allowing us to count line numbers.

; Memoization
(neighbors 0)
; (3 1 4)
(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))
; symbol-function
;
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
    (setf (gethash rest previous) (apply olg-game-tree rest)))))
;
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))
;
; Tail Call Optimization
(defun my-length (lst)
  (if lst
      (1+ (my-length (cdr lst)))
      0))
(my-length '(fie foh fum))
; this function is quite inefficient.
(defparameter *biglist* (loop for i below 100000 collect 'x))
(my-length *biglist*)
; this function kills CLISP.
(defun my-length (lst)
  (labels ((f (lst acc)
            (if lst
                (f (cdr lst) (1+ acc))
                acc)))
    (f lst 0)))
; This process of hiding one variable with another through precedence is called variable shadowing
(compile 'my-length)
(my-length *biglist*)
;
;
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
            (cond ((zerop n) (append (reverse acc) lst))
                  ((null lst) (reverse acc))
                  (t (let ((cur-player (caar lst))
                           (cur-dice (cadar lst)))
                      (if (and (eq cur-player player)
                               (< cur-dice *max-dice*))
                          (f (cdr lst)
                             (1- n)
                             (cons (list cur-player (1+ cur-dice)) acc))
                          (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))
