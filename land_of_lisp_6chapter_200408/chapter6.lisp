; print
(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))
; newline
(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))
; binds argument
(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))
(say-hello)
; print and read
(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))
(add-five)
(print '3)
(print '3.4)
(print 'foo)
(print '"foo")
(print '#\a)
(princ '3)
(princ '3.4)
(princ 'foo)
(princ '"foo")
(princ '#\a)
(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character."))
;
(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))
(say-hello)
; read-line

; code and data
'(+ 1 2) ; data mode
(+ 1 2) ; code mode
(defparameter *foo* '(+ 1 2))
(eval *foo*)
; eval can evaluate data as code.
; see uncyclopedia
; https://ja.uncyclopedia.info/wiki/LISP
; eval is too powerful to always use.
(defun game-repl ()
  (loop (print (eval (read)))))
(game-repl)
(look)
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
            (list 'quote x)))
      (cons  (car cmd) (mapcar #'quote-it (cdr cmd))))))
(game-read)
; walk east

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(game-print '(THIS IS A SENTENCE. WHAT ABOUT THIS? PROBABLY.))
; this is example of result of game-print.
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
; I'm tired.

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                      (prin1-to-string lst))
                              'list)
                    t
                    nil)
            'string))
  (fresh-line))
; coerce function transform character to list.
(game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))

; eval and read are so dangerous.
;
; lambda
; this is very important.
(defun half (n)
  (/ n 2))
#'half
; this is function operator
; equal to below:
(lambda (n) (/ n 2))
(mapcar (lambda (n) (/ n 2)) '(2 4 6))
; for lambda, lisp was borned.
; this is same as purrr package in R. map(data, function(x) {~~~})
