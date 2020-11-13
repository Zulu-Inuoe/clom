(in-package #:clom)

(cffi:define-foreign-type safe-array ()
  ((type :reader safe-array-type :initarg :type))
  (:actual-type (:pointer safearray)))

(cffi:define-parse-method safe-array (&key (type (required-argument :type)))
  (make-instance 'safe-array :type type))

(defmethod cffi:translate-to-foreign (vector (type safe-array))
  (with-foreign-object* (sa-bound 'safearraybound)
    (with-foreign-slots* (c-elements l-lbound) sa-bound
      (setf l-lbound 0
            c-elements (length vector)))
    (win32:safe)))
