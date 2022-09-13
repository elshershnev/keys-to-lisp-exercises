#|
    1. 1. If x is a, y is b, and z is (c d), write backquoted expressions containing 
    only variables that yield each of the following: 
        (a) ((C D) A Z) 
        (b) (X B C D) 
        (c) ((C D A) Z) 
|#

(defun print-expressions ()
    (let ((x 'A) (y 'B) (z '(C D)))
        (print `(,z ,x Z))
        (print `(X ,y ,@z))
        (print `((,@z ,x) Z))
        t))

;; Test call.
(print-expressions)
