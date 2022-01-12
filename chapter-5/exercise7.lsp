#|
	Define a function that takes a list of numbers and returns true if the 
	difference between each successive pair of them is 1, using 
		(a) recursion 
		(b) do 
		(c) mapc and return 

|#

(defun test-list-rec (lst)
    (cond
        ((null lst) nil)
        ((= (length lst) 1) nil)
        ((> (abs (- (car lst) (cadr lst))) 1) nil)
        ((null (cddr lst)) t)
        (t (test-list-rec (cddr lst)))))


(defun test-list-iter (lst)
    (let ((len (length lst)))
        (if (or (null lst) (= (mod len 2) 1))
            nil
            (do
                ((i 0 (+ i 2))
                 (j 1 (+ j 2)))
                ((>= i len) t)
                (when (> (abs (- (nth i lst) (nth j lst))) 1) (return nil))))))


(defun test-list-mapc (lst)
    (if (or (null lst) (= (mod (length lst) 2) 1))
        nil
        (let ((r nil))
             (block Nil
                 (mapc
                     (lambda (x)
                         (if (eql r nil)
                             (setf r x)
                             (if (> (abs (- x r)) 1)
                                 (return Nil)
                                 (setf r nil))))
                     lst)
                 t))))
				 
;; Test calls.
(test-list-rec '(1 0 5 6 8 9))
(test-list-rec '(1 2 5 6 8 10))

(test-list-iter '(1 0 5 6 8 9))
(test-list-iter '(1 2 5 6 8 10))

(test-list-mapc '(1 0 5 6 8 9))
(test-list-mapc '(1 2 5 6 8 10))

