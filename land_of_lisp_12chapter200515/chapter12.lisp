;
(output-stream-p *standard-output*)
(write-char #\x *standard-output*)
(input-stream-p *standard-input*)

(read-char *standard-input*)
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))
;
(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))

(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))
(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))
;
(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :supersede)
  (print "my data" my-stream))
;


; IP address
; open new window of CLISP
; below, I used CLISP.
;
; server
(defparameter my-socket (socket-server 4321))

(defparameter my-stream (socket-accept my-socket))
; client
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))
; The IP address 127.0.0.1 is a special address that always points to the computer from which it’s called.
; client
(print "Yo Server!" my-stream)
; server
(read my-stream)
; server
(print "What up, Client!" my-stream)
; client
(read my-stream)
; server & client
(close my-stream)
; server
(socket-server-close my-socket)

;
; character stream
(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will also go into foo. " foo)
(get-output-stream-string foo)
;
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
