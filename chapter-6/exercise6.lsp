#|
	Define a function that takes one argument, a number, and returns the 
	greatest argument passed to it so far. 
|#
(let ((max nil))
    (defun max-arg (x)
        (if (or (not max) (> x max))
            (setf max x)
             max)))


;; Test calls.
(max-arg 1)
(max-arg 2)
(max-arg 3)
(max-arg 2)
(max-arg 1)
