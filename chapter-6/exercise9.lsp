#|
	Define a function like apply, but where any number printed out before 
	it returns will be printed, by default, in octal (base 8)
|#

(defun apply-oct (fn arg &rest rest)
    (let ((*print-base* 8))
         (apply fn
                (if (listp arg)
                    (append arg rest)
                    (cons arg rest)))))

;; Test function.
(defun test (&rest args)
    (print args)
    args)
	
;; Test call.
(apply-oct 'test 4 7 8 '(16 25))
