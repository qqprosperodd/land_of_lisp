; format
(format t "Add onion rings for only ~$ dollars more!" 1.5)
; t -> console
; nil -> char
; stream -> stream
(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))
;
(prin1 "foo")
(princ "foo")
(format t "I am printing ~s in the middle of this sentence." "foo")
(format t "I am printing ~a in the middle of this sentence." "foo")
; ~s prints "", ~a removes "".
(format t "I am printing ~10a within ten spaces of room." "foo")
; add space right of foo.
(format t "I am printing ~10@a within ten spaces of room." "foo")
; add space left of foo.
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
; 10 - 3 + 2 ???
(format t "I am printing ~,,4a in the middle of this sentence." "foo")
; third parameter ,,4
(format t "the word ~,,4 '!a feels very important." "foo")
; The fourth control sequence parameter specifies which character will be used for padding
(format t "the word ~,,4 '!@a feels very important." "foo")
;
(format t "The number 1000 in hexadecimal is ~x" 1000)
; ~x represent in hexadecimal (base-16)
(format t "The number 1000 in binary is ~b" 1000)
; ~b represent in binary (base-2)
(format t "The number 1000 in decimal is ~d" 1000)
; ~d represent in decimal (base-10)
(format t "Number with commas in them are ~:d times better." 1000000)
; ~:d print colon inside the number.
(format t "I am printing ~10d within ten spaces of room" 1000000)
; same as ~a, ~s.
(format t "I am printing ~10,'xd within ten spaces of room" 1000000)
;
(format t "PI can be estimated as ~4f" 3.141593)
(format t "PI can be estimated as ~,4f" 3.141593)
(format t "PI can be estimated as ~,4f" pi)
; ~4f the final width of 3.14 is four characters wide
(format t "Percentages are ~,,2f percent better than fractions" 0.77)
;
(format t "I wish I had ~$ dollars in my bank account." 1000000.2)
;
(progn (princ 22)
       (terpri)
       (princ 33))
;
(progn (princ 22)
       (fresh-line)
       (princ 33))
;
(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))
; 2 fresh-line -> single
;
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))
; ~% prints one line
(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))
; ~& prints no line
(format t "this will print ~5%on two lines spread for apart")
(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))
(random-animal)

(loop repeat 10
      do (format t "~5t~a ~15t~a ~25t~a~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
; t represent tab
(loop repeat 10
      do (format t "~30<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
; 30 letter in single line.
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))
(loop repeat 10 do (format t "~30:@<~a~;~a~;~a~>~%"
                           (random-animal)
                           (random-animal)
                           (random-animal)))
;
(loop repeat 10
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                   (random-animal)
                   (random-animal)
                   (random-animal)))
;
(defparameter *animals* (loop repeat 10 collect (random-animal)))
*animals*
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
; crazy
; |~%| prints the line break and vertical bars.
; ~2d prints left-justified number, two characters wide.


; Attack of the Robots!!!
(defun robots ()
  (loop named main
        with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                            (d .   1) (z .  63) (x .  64) (c . 65))
        for pos = 544
        then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                    (force-output)
                    (let* ((c (read))
                            (d (assoc c directions)))
                      (cond (d (+ pos (cdr d)))
                              ((eq 't c) (random 1024))
                              ((eq 'l c) (return-from main 'bye))
                              (t pos))))
        for monsters = (loop repeat 10
                             collect (random 1024))
        then (loop for mpos in monsters
                   collect (if (> (count mpos monsters) 1)
                               mpos
                               (cdar (sort (loop for (k . d) in directions
                                                 for new-mpos = (+ mpos d)
                                                 collect (cons (+ (abs (- (mod new-mpos 64)
                                                                          (mod pos 64)))
                                                                  (abs (- (ash new-mpos -6)
                                                                          (ash pos -6))))
                                                                new-mpos))
                                            '<
                                            :key #'car))))
        when (loop for mpos in monsters
                   always (> (count mpos monsters) 1))
        return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"
                   (loop for p
                         below 1024
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses))
                                              ((> (count p monsters) 1) #\#)
                                              (t #\A)))
                                        ((= p pos)
                                         #\@)
                                        (t
                                         #\ ))))))
; q w e
; a s d
; z x c
; t -> teleport
; l -> leave
