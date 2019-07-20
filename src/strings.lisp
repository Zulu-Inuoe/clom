(in-package #:clom)

(defun string-to-ptr (buffer string encoding)
  "Convert from a CL `string' to a null-terminated native string by `encoding' bytes into `buffer'"
  (declare (type cffi:foreign-pointer buffer))
  (declare (type string string))
  (cffi:lisp-string-to-foreign string buffer (babel:string-size-in-octets string :encoding encoding) :encoding encoding)
  buffer)

(defun ptr-to-string (buffer encoding)
  "Convert from a null-terminated native string into a CL `string' by `encoding' bytes from `buffer'"
  (values (cffi:foreign-string-to-lisp buffer :encoding encoding)))

(defun lisp-string-to-foreign-no-nul (string buffer bufsize
                                      &key
                                        (start 0) end offset
                                        (encoding cffi:*default-foreign-encoding*))
  "As `cffi:lisp-string-to-foreign', but without appending nul"
  (check-type string string)
  (when offset
    (setq buffer (cffi:inc-pointer buffer offset)))
  (babel-encodings:with-checked-simple-vector
      ((string (coerce string 'babel:unicode-string))
       (start start) (end end))
    (declare (type simple-string string))
    (let ((mapping (babel-encodings:lookup-mapping cffi::*foreign-string-mappings* encoding)))
      (assert (plusp bufsize))
      (multiple-value-bind (size end)
          (funcall (babel-encodings:octet-counter mapping) string start end bufsize)
        (declare (ignore size))
        (funcall (babel-encodings:encoder mapping) string start end buffer 0)))
    buffer))

(defun lisp-to-bstr (string)
  "Convert a CL `string' into a freshly-allocated `win32:bstr'"
  (let* ((octet-len (babel:string-size-in-octets string :encoding win32:+win32-string-encoding+))
         (bstr (win32:sys-alloc-string-byte-len (cffi:null-pointer) octet-len))
         success)
    (when (cffi:null-pointer-p bstr)
      (error "Out of memory when allocating BSTR"))
    (unwind-protect
         (progn
           (unless (zerop octet-len)
             (lisp-string-to-foreign-no-nul string bstr octet-len :encoding win32:+win32-string-encoding+))
           (setf success t))
      (unless success
        (win32:sys-free-string bstr)))
    bstr))

(defun bstr-to-lisp (bstr &optional (free t))
  "Convert a `win32:bstr' into a CL `string'
  If `free' is true, free the `bstr' by `win32:sys-free-string'"
  (if (cffi:null-pointer-p bstr)
      ""
      (unwind-protect
           (values (cffi:foreign-string-to-lisp bstr :count (win32:sys-string-byte-len bstr) :encoding win32:+win32-string-encoding+))
        (when free (win32:sys-free-string bstr)))))

(defun tstring-to-lisp (tchar-buf &key (offset 0) count)
  "Convert from a foreign `win32:lptstr' to a lisp string"
  (values
   (cffi:foreign-string-to-lisp
    tchar-buf
    :offset (* offset (cffi:foreign-type-size 'win32:tchar))
    :count (and count (* count (cffi:foreign-type-size 'win32:tchar)))
    :encoding win32:+win32-string-encoding+)))
