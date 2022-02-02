#|
    1. Is it possible for two symbols to have the same name but not be eql?
|#

(defun test ()
    (defpackage "TEST" (:export "A"))
    (in-package "TEST")
    (setf a '123)
    (format t "'a' in package TEST has value ~A~%" a)
    (in-package "COMMON-LISP-USER")
    (setf a '456)
    (format t "'a' in package COMMON-LISP-USER has value ~A~%" a)
    (format t "(eql TEST:a a) -> ~A~%" (eql TEST:a a))
    '"So, YES it's possible!")

;; Test call
(test)
