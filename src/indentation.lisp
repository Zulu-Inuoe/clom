(in-package #:clom)

(defmacro define-indentation (symbol rule)
  "Helper macro for optionally making use of `trivial-indent:define-indentation'"
  (with-gensyms (ti-package)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when-let (,ti-package (find-package '#:trivial-indent))
         (funcall (fdefinition `(setf ,(intern "INDENTATION" ,ti-package))) ',rule ',symbol)))))
