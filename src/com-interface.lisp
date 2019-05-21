(in-package #:clom)

;;; Interface API
(defun interface-method-list (interface)
  (getprop interface 'interface-method-list))

(defun (setf interface-method-list) (value interface)
  (setf (getprop interface 'interface-method-list) value))

(defun interface-base (interface)
  (getprop interface 'interface-base))

(defun (setf interface-base) (value interface)
  (setf (getprop interface 'interface-base) value))

(defun interface-iid (interface)
  (getprop interface 'interface-iid))

(defun (setf interface-iid) (value interface)
  (setf (getprop interface 'interface-iid) value))

(defun interface-struct (interface)
  (getprop interface 'interface-struct))

(defun (setf interface-struct) (value interface)
  (setf (getprop interface 'interface-struct) value))

(defun interface-vtbl-struct (interface)
  (getprop interface 'interface-vtbl-struct))

(defun (setf interface-vtbl-struct) (value interface)
  (setf (getprop interface 'interface-vtbl-struct) value))

(defun build-method-list (interface)
  "Create the full, ordered list of methods implemented by `interface' and its
base interfaces."
  (mapcan #'interface-method-list
          (loop
            ;; Using push rather than collecting since we want the list in base-first-order
            :with result := (list interface)
            :for interface := (interface-base interface)
            :while interface
            :do (push interface result)
            :finally (return result))))

(defun interface-declaring-method (interface method-name)
  "Find the interface declaring method `method-name' by looking at
`interface', and any base interfaces.
Returns as two values the interface, and the method declaration."
  (loop
    :for curr := interface :then (interface-base curr)
    :for method := (find method-name (interface-method-list interface) :key #'car)
    :when method
      :return (values interface method)))
