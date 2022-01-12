#|
	Define a function that takes one argument, a number, and returns true 
	if it is greater than the argument passed to the function the last time it 
	was called. The function should return nil the first time it is called.
|#
(let ((previous nil))
    (defun greater-arg (x)
        (let ((prev previous))
            (setf previous x)
            (if (null prev)
                nil
                (when (> x prev) t)))))

;; Test calls.
(greater-arg 1)
(greater-arg 2)
(greater-arg 3)
(greater-arg 2)
(greater-arg 1)
(greater-arg 2)
(greater-arg 1)
