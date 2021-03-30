#|
	Define a function to take a square array (an array whose dimensions are (n n)) and rotate it 90° clockwise: 
		> (quarter-turn #2A((a b) (c d))) 
		#2A((C A) (D B)) 

	You'll need array-dimensions (page 361). 
|#


;;; returns specific colomn of array as a list
(defun get-column (arr column-number)
    (let ((rows (array-dimension arr 0)))
         (let ((vec nil))
              (dotimes (i rows vec)
                  (setf vec (cons (aref arr i column-number) vec))))))


(defun quarter-turn (arr)
    (let ((n (array-dimension arr 0))
          (m (array-dimension arr 1)))
          (let ((result nil))
             (dotimes (col m)
                 (setf result (append result (list (get-column arr col)))))
             (make-array (list n m) :initial-contents result))))


;; test calls

(quarter-turn #2A((a b c) (d e f) (g h i)))

(quarter-turn #2A((a b) (c d)))
