#|
    2. Define a function that takes an integer number of cents and returns four
    values showing how to make that number out of 25-, 10-, 5- and 1-cent
    pieces, using the smallest total number of coins
|#

(defun amount-in-coins (amount)
    (let ((coins '(25 10 5 1))
          (moneybox '(0 0 0 0))
          (total 0))
        (dotimes (i (length coins) (apply #'values moneybox))
            (let ((coin (nth i coins))
                  (rest (- amount total)))
                (setf (nth i moneybox) (floor (/ rest coin)))
                (incf total (* (nth i moneybox) coin))))))

;; Test calls.
(multiple-value-bind (a b c d) (amount-in-coins 66)
    (format t "~A ~A ~A ~A" a b c d))
