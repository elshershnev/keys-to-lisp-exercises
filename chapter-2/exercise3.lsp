;;;; Using car and cdr, define a function to return the fourth element of a list.


(defun fourth-item (lst)
	(car (cdr (cdr (cdr lst)))))


;; test calls

(fourth-item '(1 2 3 4 5))

(fourth-item '(1 2 3))

(fourth-item '())
