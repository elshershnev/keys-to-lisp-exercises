#|
	Define iterative and recursive versions of a function that takes an object
	x and vector v, and returns a list of all the objects that immediately 
	precede x in v: 
	> (precedes #\a "abracadabra") 
	(#\c #\d #\r) 
|#

(defun precedes-iter (obj vector)
    (let ((len (length vector))
          (lst nil))
        (do* ((i 1 (+ i 1)))
            ((= i len ) lst)
            (let ((cur (aref vector i))
                  (prev (aref vector (- i 1)))
				  (next-i (+ i 1)))
                (when (and (eql obj cur) (not (position prev vector :start next-i)))
					(setf lst (cons prev lst)))))))


(defun precedes-rec1 (obj vector)
    (cond
        ((= (length vector) 1) nil)
        (t (let ((f (aref vector 0))
                 (s (aref vector 1))
                 (newv (subseq vector 1)))
                (if (and (eql obj s) (not (position f newv :start 1)))
                    (cons f (precedes-rec1 obj newv))
                    (precedes-rec1 obj newv))))))


(defun precedes-rec2 (obj vector)
    (if (= (length vector) 1)
        nil
        (let ((f (aref vector 0))
              (s (aref vector 1))
              (newv (subseq vector 1)))
             (if (and (eql obj s) (not (position f newv :start 1)))
                 (cons f (precedes-rec2 obj newv))
                 (precedes-rec2 obj newv)))))
				 
;; Test calls.
(precedes-iter #\a "abracadabra")
(precedes-rec1 #\a "abracadabra")
(precedes-rec2 #\a "abracadabra")
