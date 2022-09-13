#|
    2. Define if in terms of cond.
|#

(defmacro our-if (condition statement1 statement2)
    `(cond
        (,condition ,statement1)
        (t ,statement2)))
        

;; Test calls.
(our-if (< 1 2) (+ 1 2) (+ 3 4))   ; 3
(our-if (> 1 2) (+ 1 2) (+ 3 4))   ; 7
