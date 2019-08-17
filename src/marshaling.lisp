(in-package #:clom)

(defvar *param-name* nil
  "The name of the parameter being processed by `make-outparam-forms' or `make-inoutparam-forms'.
  This describes the CL variable binding.")

(defvar *param-tmp-name* nil
  "The 'temporary' parameter name being processed by `make-outparam-forms' or `make-inoutparam-forms'.
  This describes the foreign variable binding.")

(defgeneric make-outparam-forms (type)
  (:documentation
   "Create 'binding' and 'result' forms to use when unmarshalling an 'out' parameter
  `type' - The type of the parameter
  Should return two values:
    1. A binding form suitable for `cffi:with-foreign-objects' or nil, if no binding necessary
    2. A form suitable for obtaining the CL value of the output parameter."))

(defgeneric make-outparam-ptr-forms (pointer-type)
  (:documentation
   "As `make-outparam-forms', but with a type resolved to (:pointer `pointer-type')
  This allows specializing methods for pointers to specific types, such as `win32:bstr'"))

(defmethod make-outparam-forms (type)
  "Attempt to parse the `type' as a cffi type, and resolve further."
  (make-outparam-forms (cffi::parse-type type)))

(defmethod make-outparam-forms ((type cffi::foreign-type-alias))
  "Dispatch to the actual type of the alias."
  (make-outparam-forms (cffi::actual-type type)))

(defmethod make-outparam-forms ((type cffi::foreign-typedef))
  "Dispatch to the actual type of the typedef"
  (make-outparam-forms (cffi::actual-type type)))

(defmethod make-outparam-forms ((type cffi::foreign-pointer-type))
  "Resolve a pointer type via `make-outparam-ptr-forms'"
  (let ((pointer-type (cffi::unparse-type (cffi::pointer-type type))))
    (make-outparam-ptr-forms pointer-type)))

(defmethod make-outparam-forms ((type cffi::foreign-string-type))
  (error "Bare string marshalling not implemented"))

(defmethod make-outparam-ptr-forms (pointer-type)
  "Base implementation that simply retrieves the value via `cffi:mem-ref'"
  (values
   `(,*param-tmp-name* ',pointer-type)
   `(&* ,*param-tmp-name*)))

(defmethod make-outparam-ptr-forms ((pointer-type (eql 'win32:bstr)))
  "Unmarshall a `win32:bstr' into a CL string"
  (values
   `(,*param-tmp-name* 'win32:bstr)
   `(bstr-to-lisp (&* ,*param-tmp-name*))))

(defgeneric make-inoutparam-forms (type)
  (:documentation
   "Create `binding' and `result' forms to use when marshalling and unmarshalling an 'inout' parameter
  `type' - The type of the parameter
  Should return three values:
    1. A binding form suitable for `cffi:with-foreign-objects' or nil, if no binding necessary
    2. An 'init' form with which to initialize the parameter, or nil if no init necessary
    3. A form suitable for obtaining the CL value of the output parameter."))

(defgeneric make-inoutparam-ptr-forms (pointer-type)
  (:documentation
   "As `make-inoutparam-forms', but with a type resolved to (:pointer `pointer-type')
  This allows specializing methods for pointers to specific types, such as `win32:bstr'"))

(defmethod make-inoutparam-forms (type)
  "Attempt to parse the `type' as a cffi type, and resolve further."
  (make-inoutparam-forms (cffi::parse-type type)))

(defmethod make-inoutparam-forms ((type cffi::foreign-type-alias))
  "Dispatch to the actual type of the alias"
  (make-inoutparam-forms (cffi::actual-type type)))

(defmethod make-inoutparam-forms ((type cffi::foreign-typedef))
  "Dispatch to the actual type of the typedef"
  (make-inoutparam-forms (cffi::actual-type type)))

(defmethod make-inoutparam-forms ((type cffi::foreign-pointer-type))
  "Dispatch to `make-inoutparam-ptr-forms' with the pointer's type"
  (let ((pointer-type (cffi::unparse-type (cffi::pointer-type type))))
    (make-inoutparam-ptr-forms pointer-type)))

(defmethod make-inoutparam-forms ((type cffi::foreign-string-type))
  "Allocate a temporary buffer with the string's contents and coerce back to a CL string"
  (values
   `(,*param-tmp-name* ':uint8 (babel:string-size-in-octets ,*param-name* :encoding ,(cffi::encoding type)))
   `(string-to-ptr ,*param-tmp-name* ,*param-name* ,(cffi::encoding type))
   `(ptr-to-string ,*param-tmp-name* ,(cffi::encoding type))))

(defmethod make-inoutparam-ptr-forms (pointer-type)
  "Allocate a pointer initialized with the parameter's value and query its value for the result"
  (values
   `(,*param-tmp-name* ',pointer-type)
   `(setf (&* ,*param-tmp-name*) ,*param-name*)
   `(&* ,*param-tmp-name*)))

(defmethod make-inoutparam-ptr-forms ((pointer-type (eql 'win32:bstr)))
  "Allocate a `win32:bstr' initialized to the parameter's string value, and convert back to a CL string for the result."
  (values
   `(,*param-tmp-name* 'win32:bstr)
   `(setf (&* ,*param-tmp-name*) (lisp-to-bstr ,*param-name*))
   `(bstr-to-lisp (&* ,*param-tmp-name*))))