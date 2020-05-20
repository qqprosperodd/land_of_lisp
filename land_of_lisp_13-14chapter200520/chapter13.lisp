; Web Server
(error "foo")
; error message : foo

(define-condition foo () ()
  (:report (lambda (condition stream)
              (princ "Stop FOOing around, numbskull!" stream))))
(error 'foo)
; error message : Stop FOOing around, numbskull!
;
(defun bad-function ()
  (error 'foo))
(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))
; program can keep running without interruption
(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' matter what"))
;
; this is request page for firefox
GET /lolcats.html HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
;
; this is an example of request body
<html>
  <body>
    Sorry dudez, I don't have any L0LZ for you today :-(
  </body>
</html>
;
; request parameter
POST /login.html HTTP/1.1
Host: www.mywebsite.com
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
Content-Length: 39

userid=foo&password=supersecretpassword
; Added Content-Length.

;
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
        (code-char code)
        default)))
;
(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#\% (cons (http-char (cadr lst) (caddr lst))
                             (f (cdddr lst))))
                  (#\% (cons #\space (f (cdr lst))))
                  (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))
;
(decode-param "foo")
(decode-param "foo%3F")
(decode-param "foo+bar")
;
(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))
(parse-params "name=bob&age=25&gender=male")
;
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
          ((x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))
;
(parse-url "GET /locates.html HTTP/1.1")
(parse-url "GET /locates.html?extra-funny=yes HTTP/1.1")
;
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
         (when i
            (cons (intern  (string-upcase (subseq s 0 i)))
                  (subseq s (+ i 2)))))))
  (when h
    (cons h (get-header stream)))))
; If it encounters a line that doesn’t conform to a header line,
; it means we’ve reached the blank line at the end of the header and are finished.
(get-header (make-string-input-stream "foo: 1
bar: abc, 123

"))
;
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))
;
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
            (princ "<html><format>What is your name?<input name='name' />
</form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know the page.")))
;
(hello-request-handler "lolcats" '() '())
(hello-request-handler "greeting" '() '())
(hello-request-handler "greeting" '() '((name . "Bob")))

(serve #'hello-request-handler)
