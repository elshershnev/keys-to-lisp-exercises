;;;; Define a function that takes two arguments and returns the greater of the two. 


(defun greater-arg (a b)
	(if (> a b) a b))


;; test calls

(greater-arg 1 2)

(greater-arg 2 1)

(greater-arg 1 1)
