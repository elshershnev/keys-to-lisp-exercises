#|
	Define a function that takes a list and prints it in dot notation: 
	>(showdots '(a b c)) 
	(A . (B . (C . NIL))) 
	NIL
|#


(defun print-symbol (symbol n)
    (dotimes (i n)
		(format t "~A" symbol)))


(defun showdots-helper (lst level)
    (if (null lst)
        (progn (format t " Nil ") (print-symbol ")" level))
        (progn (format t "(~A . " (car lst)) (showdots-helper (cdr lst) (+ level 1)))))


(defun showdots (lst)
    (showdots-helper lst 0))

;; test call

(showdots '(a b c d e f))
