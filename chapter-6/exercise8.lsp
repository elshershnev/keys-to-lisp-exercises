#|
	Suppose expensive is a function of one argument, an integer between 
	0 and 100 inclusive, that returns the result of a time-consuming com-
	putation. Define a function frugal that returns the same answer, but 
	only calls expensive when given an argument it has not seen before.
|#

(defun expensive (arg)
    (format t "~%espensive is called with arg=~A" arg)
    (* arg 2))

(let ((cache (make-hash-table)))
    (defun frugal (arg)
        (let ((result (gethash arg cache)))
            (if result
                result
                (setf (gethash arg cache) (expensive arg))))))

;; Test calls.
(frugal 2)
(frugal 2)
(frugal 2)
(frugal 3)
(frugal 3)
(frugal 3)
