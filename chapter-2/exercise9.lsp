#|
	A friend is trying to write a function that returns the sum of all the 
	non-nil elements in a list. He has written two versions of this function, 
	and neither of them work. Explain what's wrong with each, and give a 
	correct version: 
	(a) (defun summit (lst) 
			(remove nil lst) 
			(apply #' + lst)) 

	(b) (defun summit (lst) 
			(let ((x (car lst))) 
				(if (null x) 
					(summit (cdr lst)) 
					(+ x (summit (cdr lst))))))
|#


;;; Fixed version (a)
(defun summit1 (lst)
	;; Fried forgot to update value of lst variable after removing nil from it
	(setf lst (remove nil lst))
	(apply #' + lst))


;;; Fixed version (b)
(defun summit2 (lst)
	;; Fried forgot to stop recursion and to return something in case of empty list receiving
    (if (null lst)
        0
        (let ((x (car lst)))
            (if (null x)
                (summit2 (cdr lst))
                (+ x (summit2 (cdr lst)))))))


;; test calls

(summit1 '(Nil 1 2 Nil 3 4 Nil))

(summit2 '(Nil 1 2 Nil 3 4 Nil))
