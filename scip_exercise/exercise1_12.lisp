; Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.
; The numbers at the edge of the triangle are all 1, and each
; number inside the triangle is the sum of the two numbers above it.
; Write a procedure that computes elements of
; Pascal’s triangle by means of a recursive process.

; layer1 1
; layer2 1 1
; layer3 1 2 1
; layer4 1 3 3 1
; layer5 1 4 6 4 1
; layer6 1 5 10 10 5 1
; layer7 1 6 15 20 15 6 1

; if number = 1, number = layer : 1
; else Pascal(layer,number) = Pascal(layer-1,number) + Pascal(layer-1,number-1)

(defun pascal (layer number)
  (cond ((eql number 1) 1)
        ((eql number layer) 1)
        (t (+ (pascal (- layer 1) (- number 1)) (pascal (- layer 1) number)))))

(pascal 3 2)
(pascal 16 5)
