#|
	Read the description of reduce on page 368, then use it to define: 
	(a) copy-list 
	(b) reverse (for lists) 
|#

;;; Appends element to accumulator for copy-lst implementation. We assume here that acc is always list
(defun conc-elements1 (acc el)
    (append acc (list el)))

(defun copy-lst (lst)
    (reduce 'conc-elements1 lst :initial-value Nil))


;;; Appends element to accumulator for reverse-lst implementations. We assume here that acc can by anything.
(defun conc-elements2 (acc el)
    (cons el (if (and (atom acc) (not (listp acc))) (list acc) acc)))

;;; reverse-lst version without using initial-value
(defun reverse-lst1 (lst)
    (reduce 'conc-elements2 lst))

;;; reverse-lst version using the initial-value key argument
(defun reverse-lst2 (lst)
    (reduce 'conc-elements2 lst :initial-value Nil))

;; test calls
(copy-lst '(q w e r t y))
(reverse-lst1 '(q w e r t y))
(reverse-lst2 '(q w e r t y))
