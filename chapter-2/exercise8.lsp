#|
	Give iterative and recursive definitions of a function that 
	(a) takes a positive integer and prints that many dots. 
	(b) takes a list and returns the number of times the symbol a occurs in it.
|#


;; task (a) recursive
(defun printdots (num)
    (if (= num 0)
        'done
        (progn (format t ".") (printdots (- num 1)))))


;; task (a) iterative
(defun printdots-iter (num)
    (do ((i 0 (+ i 1)))
        ((>= i num) 'done)
        (format t ".")))


;; task (b) recursive
(defun number-of-a (lst)
    (if (null lst)
        0
        (if (eq (car lst) 'a)
			(+ (number-of-a (cdr lst)) 1)
			(number-of-a (cdr lst)))))


;; task (b) iterative
(defun number-of-a-iter (lst)
    (let ((num 0))
        (dolist
            (i lst)
            (if (eq i 'a) (setf num (+ num 1))))
        num))


;; test calls

(printdots 10)
(printdots-iter 3)

(number-of-a '(a a 1 2 3))
(number-of-a '(1 a 2 a 3 a))
(number-of-a '(1 2 3))

(number-of-a-iter '(a a 1 2 3))
(number-of-a-iter '(1 a 2 a 3 a))
(number-of-a-iter '(1 2 3))
