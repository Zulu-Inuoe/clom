(in-package #:clom)

(defun tstring-to-lisp (tchar-buf &key (offset 0) count)
  "Convert from a foreign `win32:lptstr' to a lisp string"
  (values
   (cffi:foreign-string-to-lisp
    tchar-buf
    :offset (* offset (cffi:foreign-type-size 'win32:tchar))
    :count (and count (* count (cffi:foreign-type-size 'win32:tchar)))
    :encoding win32:+win32-string-encoding+)))

(defun error-code-string (hresult)
  "Retrieve the string representation for `hresult'."
  (with-foreign-object* (result '(:pointer win32:tchar))
    (let ((tchar-count
            (win32:format-message (logior win32:+format-message-allocate-buffer+
                                          win32:+format-message-ignore-inserts+
                                          win32:+format-message-from-system+)
                                  (cffi:null-pointer)
                                  hresult
                                  (win32:make-lang-id win32:+lang-neutral+ win32:+sublang-default+)
                                  result
                                  0
                                  (cffi:null-pointer))))
      (cond
        ((> tchar-count 0)
         (unwind-protect
              (cffi-let ((tstr win32:tchar (&* result)))
                ;; Cut LF
                (when (and (> tchar-count 0)
                           (= (&* tstr (- tchar-count 1)) 10))
                  (decf tchar-count)
                  ;; Cut CR
                  (when (and (> tchar-count 0)
                             (= (&* tstr (- tchar-count 1)) 13))
                    (decf tchar-count)))
                (tstring-to-lisp tstr :count tchar-count))
           (win32:local-free (&* result))))
        ((<= #x80040200 hresult #x8004FFFF)
         (format nil "IDispatch error #~D" (- hresult #x80040200)))
        (t
         (format nil "Unknown error 0x~X" hresult))))))

(define-condition com-error ()
  ((hresult
    :type (unsigned-byte 32)
    :initarg :hresult
    :initform (required-argument :hresult)
    :reader com-error-hresult)
   (string
    :type string
    :initarg :string
    :initform (required-argument :string)
    :reader com-error-string))
  (:report
   (lambda (c stream)
     (format stream "COM error ~D (~:*0x~X): \"~A\""
             (com-error-hresult c)
             (com-error-string c)))))

(defun com-error (hresult)
  "Signals an error of type `win32-error' using `code' as the error code"
  (check-type hresult (or (unsigned-byte 32) (signed-byte 32)))
  (let ((unsigned-hresult (ldb (byte 32 0) hresult)))
    (error 'com-error :hresult unsigned-hresult :string (error-code-string unsigned-hresult))))

(defun check-com-error (hresult)
  "Signals an error if `hresult' indicates a failure result (severity bit set)"
  (when (logbitp 31 hresult)
    (com-error hresult))
  hresult)
