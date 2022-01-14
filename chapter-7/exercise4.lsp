#|
	Define a function that takes a two-dimensional array of floats and 
	displays it in neat columns. Each element should be printed with two 
	digits after the decimal point, in a field 10 characters wide. (Assume 
	all will fit.) You will need array-dimensions (page 361).
|#

(defun print-array (arr)
	(destructuring-bind
	    (n m) (array-dimensions arr)
		(do ((row 0 (+ row 1)))
		    ((>= row n) t)
		    (loop for col from 0 to (- m 1) do
				(format t "~10,2F" (aref arr row col)))
		    (terpri))))

;; Test call.
(print-array #2a((1.234 2.4 3.567) (-4. 5.3455 6.2)))
