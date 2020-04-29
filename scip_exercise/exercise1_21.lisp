(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

; Exercise 1.21: Use the smallest-divisor procedure to find
; the smallest divisor of each of the following numbers: 199, 1999, 19999

(defun square (n) (* n n))
(defun divides? (a b) (= (mod b a) 0))
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))
(defun smallest-divisor (n) (find-divisor n 2))

(smallest-divisor 199)
; 199
(smallest-divisor 1999)
; 1999
(smallest-divisor 19999)
; 7

; single function
(defun smallest-divisor (n)
  (labels ((divides? (a b)
             (= (mod b a) 0))
           (find-divisor (n test-divisor)
             (cond ((> (* test-divisor test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (t (find-divisor n (+ test-divisor 1))))))
    (find-divisor n 2)))
