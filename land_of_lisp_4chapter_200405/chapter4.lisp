; condition and eval
(if '()
    'i-am-true
    'i-am-false)
; nil is false
(if '(1)
    'i-am-true
    'i-am-false)

(defun my-length (list)
  (if list
    (1+ (my-length (cdr list)))
    0))
(my-length '(list with four symbols))
; my-length counts elements of list.
; () = '() = nil = 'nil

(if (= (+ 1 2) 3)
    'yup
    'nope)
(if (= (+ 1 2) 4)
    'yup
    'nope)
; =
(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)
(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)
; (if '() ~~~ ~~~~) detects whether list is nil.

(if (oddp 5)
    'odd-number
    'even-number)
; oddp returns True if argument is odd.
; evenp returns True if argument is even.

(if (oddp 5)
    'odd-number
    (/ 1 0))
; return odd-number
; never evaluate (/ 1 0)
; if is a special argument.
; "special argument" also contains let, defun ...etc.

(if (oddp 5)
    (progn (setf *number-was-odd* t)
        'odd-number)
    'even-number)
*number-was-odd*
; if you use progn, you can use other function in if sentence.

; when or unless is hidden progn
(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)
*number-is-odd*
(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)
*number-is-odd*

; cond
; this is absolute conditional branch.
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
                            '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
                            '(i hope you choked on my pudding johnny))
        (t                    '(why you eat my pudding stranger ?))))
(pudding-eater 'johnny)
*arch-enemy*
(pudding-eater 'george-clooney)
; cond is great.
; case
(defun pudding-eater (person)
  (case person
   ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
            '(curse you lisp alien - you ate my pudding))
   ((johnny) (setf *arch-enemy* 'useless-old-johnny)
            '(i hope you choked on my pudding johnny))
   (otherwise '(why you eat my pudding stranger ?))))

; more technics of conditions
(and (oddp 5) (oddp 7) (oddp 8))
; = (A & B & C) in R
(or (oddp 4) (oddp 7) (oddp 8))
; = (A | B | C) in R

(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t)) ; T
*is-it-even* ; T
(defparameter *is-it-even* nil)
(or (oddp 5) (setf *is-it-even* t)) ; T
*is-it-even* ; nil
; or/and immediately returns T/F when it evaluate T/F condition.

(if *file-modified*
  (if (ask-user-about-saving)
      (save-file)))
; this function save if users hope to save.
; same as this:
(and *file-modified* (ask-user-about-saving) (save-file))
; this is better
(if (and *file-modified* (ask-user-about-saving))
  (save-file))

;
(if (member 1 '(3 4 1 5))
   'one-is-in-the-last
   'one-is-not-in-the-last)
(member 1 '(3 4 1 5))
; https://lisphub.jp/common-lisp/cookbook/index.cgi?%E3%83%AA%E3%82%B9%E3%83%88%E3%81%8B%E3%82%89%E8%A6%81%E7%B4%A0%E3%82%92%E6%8E%A2%E3%81%99
; member returns bellow list that matched argument.
; conrad must not like member function...
(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list)
; true, so if you type (member nil '(3 4 nil 5)), returns (nil 5)
(find-if #'oddp '(2 4 5 6))
; #' is function operator (see chapter 5.2, 7, 14)
; e.g. (mapcar #'car ((~~~)))
(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number)

(find-if #'null '(2 4 nil 6))
; null returns T when it finds nil, but sentence above returns nil, so if function using above sentence returns always nil.

; compare functions
; eq equal eql = string-equal equalp
(defparameter *fruit* 'apple)
(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))
; compare symbols (**)
; to compare others, use equal
(equal 'apple 'apple)
(equal (list 1 2 3) (list 1 2 3))
(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))
(equal 5 5)
(equal 2.5 2.5)
(equal "foo" "foo")
(equal #\a #\a)
; eql can compare symbol, number, character
(eql 'foo 'foo)
(eql 3.4 3.4)
(eql #\a #\a)
; equalp doesn't distinguish small/large text or double/integer.
(equalp "Bob Smith" "bob smith")
(equalp 0 0.0)
; = compare only numbers.
; string-equal compare only strings.
; char-equal compare only character.
