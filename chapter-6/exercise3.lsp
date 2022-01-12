#|
	Define a function that takes any number of arguments and returns the 
	number of arguments passed to it. 
|#

(defun arg-length (&rest args)
    (length args))

;; Test call.
(arg-length 1 2 '(3 4 5) #(6 7))