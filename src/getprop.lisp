(in-package #:clom)

(defun getprop (symbol property)
  "As `get', but second return value indicates whether the property was found or not."
  (multiple-value-bind (indicator value)
      (get-properties (symbol-plist symbol) (list property))
    (values value (not (null indicator)))))

(defun (setf getprop) (value symbol property)
  "Exactly as `(setf get)'. Just used for consistency with `getprop'"
  (setf (get symbol property) value))