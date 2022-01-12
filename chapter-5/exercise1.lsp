#|
	Translate the following expressions into equivalent expressions that 
	don't use let or let*, and don't cause the same expression to be 
	evaluated twice. 
	(a) (let ((x (car y))) 
			(cons x x)) 
	(b) (let* ((w (car x)) 
			(y (+ v z))) 
			(cons w y))
|#


;; Test calls.

(setf x '(1 2 3) y '(1 2 3) z 7)
;; Original expression a.
(let ((x (car y)))
      (cons x x))
;; Expression equivalent to a.
((lambda (x) 
        (setf x (car y))
        (cons x x)) y)


;; Original expression b.
(let* ((w (car x))
	   (y (+ w z)))
	(cons w y))
;; Expression equivalent to b.
((lambda (x z)
	(setf w (car x) y (+ w z))
	(cons w y)) x z)
