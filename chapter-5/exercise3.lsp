#|
	Define a function that returns the square of its argument, and which 
	does not compute the square if the argument is a positive integer less 
	than or equal to 5.
|#

(defun square (x)
	(cond
		((= x 1) 1)
		((= x 2) 4)
		((= x 3) 9)
		((= x 4) 16)
		((= x 5) 25)
		(t (* x x))))

;; Test calls.
(square 3)
(square 8)
