#|
    3. A faraway planet is inhabited by two kinds of beings, wigglies and
    wobblies. Wigglies and wobblies are equally good at singing. Every
    year there is a great competition to chooses the ten best singers. Here
    are the results for the past ten years:
        YEAR      1 2 3 4 5 6 7 8 9 10
        WIGGLIES  6 5 6 4 5 5 4 5 6 5
        WOBBLIES  4 5 4 6 5 5 6 5 4 5
    Write a program to simulate such a contest. Do your results suggest
    that the committee is, in fact, choosing the ten best singers each year?
|#

(defun start-competition (months)
	(let ((result (make-array (list 3 months))))
		(dotimes (i months result)
			(let* ((viggles (+ 4 (random 3)))
					(vobbles (- 10 viggles)))
				(setf (aref result 0 i) (1+ i))
				(setf (aref result 1 i) viggles)
				(setf (aref result 2 i) vobbles)))))

(defun check-result (result)
    (let
        ((years (array-dimension result 1))
         (winners-per-year 10))
        (dotimes (i years t)
            (when (/=
                    (+ (aref result 1 i) (aref result 2 i))
                    winners-per-year)
                (return-from check-result nil)))))

;; Test calls.
(setf r (start-competition 10))
(check-result r)
(incf (aref r 1 3 ))
(check-result r)
