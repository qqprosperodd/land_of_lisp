;
(load "graph-util") ; this doesn't work
; skip
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
  (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))
(make-edge-list)

; loop command
(loop repeat 10
      collect 1)
(loop for n from 1 to 10
      collect (+ 100 n))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                  (eql (car x) node))
                edge-list))
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
              (unless (member node visited)
                (push node visited)
                (mapc (lambda (edge)
                        (traverse (cdr edge)))
                      (direct-edges node edge-list)))))
      (traverse node))
  visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
              (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                    (push connected islands)
                    (when unconnected
                      (find-island unconnected)))))
    (find-island nodes))
  islands))
; find-islands finds isolated island. then purge unconnected nodes.
; set-difference returns a list of elements of list-1 that do not appear in list-2.
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))
; all island is connected by bridge.
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

; in order to finish, transform edge into alist.
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
          (edge-list (connect-all-islands nodes (make-edge-list)))
          (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                                edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))
; edge-to-alist transform edge to alist.
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                              :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))
; edge-list looks like : '((1 . 2) (2 . 1) (2 . 3) (3 . 2))
; alist looks like : '((1 (2)) (2 (1) (3)) (3 (2)))
; remove-duplicates remove duplicates.

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))
; add-cops : generate cop information
; intersection detects shared component of 2 list.
; this is result : ((1 (2)) (2 (1) (3 COPS)) (3 (2 COPS)))
; this is nested alist.
(let ((a 5)
      (b (+ a 2)))
  b)
;
(let* ((a 5)
      (b (+ a 2)))
  b)
; let* can use the argument that is defined the same time.

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))
; if the distance of two nodes < 2
; some returns true if at least one argument is shared.
; detects ((lambda (x) (within-one x b edge-list)) U (neighbors a edge-alist))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                '(glow-worm))
                                ((some (lambda (worm)
                                        (within-one n worm edge-alist))
                                      glow-worms)
                                '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))
; ((1 (2)) (2 (1) (3 COPS)) (3 (2 COPS)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))
;
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))
;
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (assoc node
                                                  *congestion-city-edges*))))
                            *visited-nodes*)))))
;
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                (if (member (car x) *visited-nodes*)
                                    x
                                    (list (car x))))
                              (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))
; mapcan and mapcon are like mapcar and maplist respectively,
; except that the results of applying function are combined into a list by the use of nconc rather than list.
(defun ingredients (order)
  (mapcan (lambda (burger)
            (case burger
              (single (list 'patty))
              (double (list 'patty 'patty))
              (double-cheese (list 'patty 'patty 'cheese))))
          order))
(ingredients '(single double-cheese double))
;
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))
;

(defun walk (pos)
  (handle-direction pos nil))
(defun charge (pos)
  (handle-direction pos t))
(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                    (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))
;
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
        (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                      (princ "You found the Wumpus!")
                                      (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))
;
; grand theft wumpus
(new-game)
#<SB-IMPL::PROCESS :EXITED 0>
* (walk 29)
NIL
* (walk 14)
NIL
* (walk 7)
NIL
* (walk 14)
NIL
* (walk 3)
NIL
* (walk 26)
You ran into the Wumpus
"You ran into the Wumpus"
* (walk 10)
You ran into the cops. Game Over.
"You ran into the cops. Game Over."
; lol
;
* (new-game)
#<SB-IMPL::PROCESS :EXITED 0>
* (walk 18)
NIL
* (walk 24)
NIL
* (walk 18)
NIL
* (walk 13)
NIL
* (walk 14)
NIL
* (walk 12)
NIL
* (walk 30)
NIL
* (walk 12)
NIL
* (walk 15)
NIL
* (walk 10)
NIL
* (walk 17)
NIL
* (walk 23)
NIL
* (walk 26)
NIL
* (walk 16)
NIL
* (walk 23)
NIL
* (walk 17)
NIL
* (walk 4)
NIL
* (walk 30)
NIL
* (walk 3)
NIL
* (walk 21)
NIL
* (walk 3)
NIL
* (walk 1)
NIL
* (walk 3)
NIL
* (walk 21)
NIL
* (charge 9)
You found the Wumpus!
"You found the Wumpus!"
; yeah!
