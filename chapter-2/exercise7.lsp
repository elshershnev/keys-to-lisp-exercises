#|
	Using only operators introduced in this chapter, define a function that takes
	a list as an argument and returns true if one of its elements is a list.
|#

(defun has-list (lst)
    (and
		(not (null lst))
		(if (listp (car lst))
			t
			(has-list (cdr lst)))))


;; test calls

(has-list '())

(has-list '(1 2 3))

(has-list '((1) 2 3))

(has-list '(1 (2) 3))

(has-list '(1 2 (3)))
