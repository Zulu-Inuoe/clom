(in-package #:clom)

(defun valid-iid-p (iid-str)
  "Returns true if `iid-str' is a valid iid string"
  (let ((scanner (load-time-value
                  (cl-ppcre:create-scanner "(?im)^{[0-9A-F]{8}[-]?(?:[0-9A-F]{4}[-]?){3}[0-9A-F]{12}}$"))))
    (and (stringp iid-str)
         (cl-ppcre:scan scanner iid-str)
         t)))
