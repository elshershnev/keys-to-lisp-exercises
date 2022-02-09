#|
    6. Horner's method is a trick for evaluating polynomials efficiently. To
    frndax3*+BX2+cx+dyou evaluate x(x(ax+b)+c)+d. Define a function
    that takes one or more arguments—the value of x followed by n reals
    representing the coefficients of an (n - l)th-degree polynomial—and
    calculates the value of the polynomial by Horner's method.
|#

(defun horners-method (x &rest rest)
	(reduce (lambda (one two) (+ (* x one) two)) rest))


;; Test call.
(horners-method 2 1 2 3 4) ; x = 2, x3 + 2x2 + 3x + 4 = 26
