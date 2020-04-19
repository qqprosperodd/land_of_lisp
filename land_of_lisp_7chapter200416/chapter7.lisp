; warning : I didn't install graphviz, so I haven't write directed graph.

(cons 1 (cons 2 (cons 3 nil)))
; (1 2 3)
(cons 1 (cons 2 3 ))
; (1 2 . 3)
'(1  . (2 . (3 . nil)))
(cons 2 3)
; (2 . 3)

; circular list
(setf *print-circle* t)
; remember type this before circular list
(defparameter foo (list 1 2 3))
(setf (cdddr foo) foo)

; alist
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
(assoc 'lisa *drink-order*)
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
; see 5.2
; add new list
(assoc 'lisa *drink-order*)

; tree structure
(defparameter *house* '((walls (mortar (cement)
                                        (water)
                                        (sand))
                                (bricks))
                                (windows (glass)
                                          (frame)
                                          (curtains))
                                (roof (shingles)
                                      (chimney))))

; graph
(defparameter *wizard-nodes* '((living-room (you are in the living room.
                                a wizard is snoring loudly on the couch.))
                              (garden (you are in a beautiful garden.
                              there is a well in front of you.))
                              (attic (you are in the attic. there
                              is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                              (garden (living-room east door))
                              (attic (living-room downstairs ladder))))
; graphviz
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
; alphanumericp returns true if character is alphabet or numeric. else, false.
; complement returns true if false, false if true.
; prin1-to-string returns string from all character.
; https://ja.wikibooks.org/wiki/Lisp/%E5%9F%BA%E6%9C%AC%E3%81%8B%E3%82%89%E3%81%95%E3%82%89%E3%81%AB%E4%B8%80%E6%AD%A9%E9%80%B2%E3%82%93%E3%81%A7/%E6%96%87%E5%AD%97%E5%88%97
(dot-name 'living-room)
; "LIVING_ROOM"
(dot-name 'foo!) ; "FOO_"
(dot-name '24) ; "24"
(substitute-if #\e #'digit-char-p "I'm a l33t hack3r!") ; "I'm a leet hacker!"
; digit-char-p
; Tests whether char is a digit in the specified radix (i.e., with a weight less than radix).
; If it is a digit in that radix, its weight is returned as an integer; otherwise nil is returned.
; ??????????
; substitute-if replace matched string of list.
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8))

(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
    ""))
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))
; mapc also returns input list.
; https://sunday-lisper.hatenablog.com/entry/2018/04/10/144746

(nodes->dot *wizard-nodes*)
; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))
(edges->dot *wizard-edges*)
; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
; ((LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;  (GARDEN (LIVING-ROOM EAST DOOR)) (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER)))
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))
(graph->dot *wizard-nodes* *wizard-edges*)
; digraph{
; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}
; "}"
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
; (ext:shell (concatenate 'string "dot Tpng -0 " frame)))
  (sb-ext:run-program "dot" `("-Tpng" "-O" ,fname) :search t))
; thunk have no argument.
; this is nullary function.
; thunk is a subroutine used to inject an additional calculation into another subroutine.
(with-open-file (my-stream
                "testfile.txt"
                :direction :output
                :if-exists :supersede)
  (princ "Hello File!" my-stream))
; make stream
; (with-open-file (my-stream ...))
; define my-stream
; (let ((my-variable ...)))

; keyword symbol
(let ((cigar 5))
  cigar)
:cigar
(let ((:cigar 5))
  :cigar)
;
; dot->png
; with-open-file makes local argument

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))
; thunk
(graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

; undirected graph
(defun uedges->dot (edges)
  (maplist (lambda (lst)
            (mapc (lambda (edge)
                    (unless (assoc (car edge) (cdr lst))
                      (fresh-line)
                      (princ (dot-name (caar lst)))
                      (princ "--")
                      (princ (dot-name (car edge)))
                      (princ "[label=\"")
                      (princ (dot-label (cdr edge)))
                      (princ "\"];")))
                  (cdar lst)))
            edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
; maplist
(mapcar #'print '(a b c))
(maplist #'print '(a b c))

(ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges*)
