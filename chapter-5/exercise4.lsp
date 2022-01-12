#|
	4. Rewrite num-month (Figure 5.1) to use case instead of svref
|#

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

;;; Original function that is named month-num instead of num-month (a typo of Paul Graham I think) that returns
;;; the number of days up to the start of the month.
(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))


(defun days-passed (m)
    (case m
        (0 0)
        (1 31)
        (2 59)
        (3 90)
        (4 120)
        (5 151)
        (6 181)
        (7 212)
        (8 243)
        (9 273)
        (10 304)
        (11 334)
        (12 365)))


;;; New version of month-num.
(defun new-month-num (m y)
  (+ (days-passed (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

;; Test calls.
(month-num 2 2000)
(month-num 2 2001)
(month-num 3 2000)
(month-num 3 2001)

(new-month-num 2 2000)
(new-month-num 2 2001)
(new-month-num 3 2000)
(new-month-num 3 2001)
