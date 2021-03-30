#|
	Modify the program in Figure 3.6 (see bellow) to use fewer cons cells.
	(Hint: Use dotted lists.)
	
	(defun compress (x) 
		(if (consp x) 
			(compr (car x) 1 (cdr x)) 
			x)) 

	(defun compr (elt n 1st) 
		(if (null 1st) 
			(list (n-elts elt n)) 
			(let ((next (car 1st))) 
				(if (eql next elt) 
					(compr elt (+ n 1) (cdr 1st)) 
					(cons
						(n-elts elt n) 
						(compr next 1 (cdr 1st))))))) 

	(defun n-elts (elt n) ] 
		(if (> n 1) 
			(list n elt) 
			elt))
|#


(defun compress (x)
	(if (consp x)
		(compr (car x) 1 (cdr x))
		x))


(defun compr (elt n lst)
	(if (null lst)
		(list (n-elts elt n))
		(let ((next (car lst)))
			(if (eql next elt)
				(compr elt (+ n 1) (cdr lst))
				(cons (n-elts elt n)
					(compr next 1 (cdr lst)))))))


(defun n-elts (elt n)
	(if (> n 1)
		(cons n elt)
		elt))


;; test call

(compress '(1 1 1 0 1 0 0 0 0 1))
