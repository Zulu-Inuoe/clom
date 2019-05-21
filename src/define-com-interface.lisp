(in-package #:clom)

(defun param-name (param)
  (car param))

(defun param-type (param)
  (cadr param))

(defun param-flag (param flag)
  (assoc-value (cddr param) flag))

(defun param-default-form (param)
  (param-flag param :default))

(defun parse-param-flags (flags)
  "Parse out a list of parameter flags into an alist of (keyword . value) pairs.
  Signals errors on unknown keywords, duplicate flags, etc."
  (loop
    :for rest := flags :then (cdr rest)
    :while rest
    :for flag := (car rest)
    :when
    (case flag
      ((:in :lcid :retval :opt :out)
       (cons flag t))
      ((:default)
       (unless (cdr rest)
         (error "Missing value for '~A'" :default))
       (setf rest (cdr rest))
       (cons flag (car rest)))
      (t
       (cerror "Continue, ignoring the unrecognized flag" "Unrecognized flag: '~A'" flag)))
    :collect :it :into result
    :finally
       (when-let ((duplicate (find-if (lambda (opt) (not (eq (assoc (car opt) result) opt))) result)))
         (cerror "Continue, ignoring the duplicates" "Duplicate flag entry: '~A'" (car duplicate))
         (setf result (delete-duplicates result :from-end t :key #'car)))
       ;; Default to :in if not present
       (unless (or (assoc :in result) (assoc :out result))
         (push '(:in . t) result))
       (return result)))

(defun parse-param (param-spec)
  "Parse out a parameter spec into `(name type . flags)', verifying them along the way."
  (destructuring-bind (name type &rest flags) param-spec
    (check-type name symbol)
    (check-type type foreign-type)
    (list* name type (parse-param-flags flags))))

(defun parse-method-params (param-specs)
  (let ((ret (mapcar #'parse-param param-specs)))
    (when-let ((retvals (remove-if-not (lambda (param) (param-flag param :retval)) ret)))
      (when (cdr retvals)
        (error "Duplicate parameters with `:retval' flag: ~A" (mapcar #'param-name retvals))))
    ret))

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

(defun build-param-forms (params)
  "Builds up forms for defining a function call based on `params'
  Returns six(!) values:
    1. List of required and optional parameters suitable for a lambda-list
    2. Initialization forms done prior to binding
    3. Binding forms suitable for `with-foreign-objects*'
    4. Initialization forms done after binding
    5. A plist suitable for `foreign-funcall'
    6. A list of result forms"
  (let (;; Required parameters
        (req-names ())
        ;; Optional parameter forms
        (opt-forms ())
        ;; Required parameters with :op
        (init-forms ())
        ;; foreign bindings
        (bind-forms ())
        ;; Out variables with input values
        (bind-init-forms ())
        ;; A list of arguments as cffi expects them
        (call-forms ())
        ;; Any out variable result forms
        (result-forms ()))
    (dolist (param (reverse params))
      (let ((name (param-name param))
            (type (param-type param)))
        (macrolet ((flag (flag) `(param-flag param ,flag)))
          (when (flag :opt)
            (push`(setf ,name (or ,name ,(param-default-form param))) init-forms)
            (unless req-names
              (push `(,name ,(param-default-form param)) opt-forms)))
          (cond
            ((and (flag :in) (flag :out))
             ;; inout
             (let ((tmp-name (gensym (format nil "~A-TMP" (string name)))))
               (multiple-value-bind (inout-bind inout-init inout-res)
                   (let ((*param-name* name)
                         (*param-tmp-name* tmp-name))
                     (make-inoutparam-forms type))
                 (push inout-bind bind-forms)
                 (push inout-init bind-init-forms)
                 (push (list type tmp-name) call-forms)
                 (push inout-res result-forms))))
            ((flag :out)
             ;; out
             (let ((tmp-name (gensym (format nil "~A-TMP" (string name)))))
               (multiple-value-bind (out-bind out-res)
                   (let* ((*param-name* name)
                          (*param-tmp-name* tmp-name))
                     (make-outparam-forms type))
                 (push out-bind bind-forms)
                 (push (list type tmp-name) call-forms)
                 (push out-res result-forms))))
            (t ;:in only
             (push (list type name) call-forms)))

          (when (and (flag :in)
                     (or req-names (not (flag :opt))))
            (push name req-names)))))
    (values
     `(,@req-names ,@(when opt-forms '(&optional)) ,@opt-forms)
     init-forms
     bind-forms
     bind-init-forms
     (mapcan #'append call-forms)
     result-forms)))

(defun create-com-method-fn (if-name if-vtbl-struct method-form)
  "Creates code for the lambda list and body for calling a COM method.'
  Separating it like this allows for a COM method to be used in:
    `lambda', `defun', `flet', and so on.
  Returns two values:
    1. The lambda list
    2. The function body"
  (destructuring-bind (method-name (&key (convention :stdcall))
                       return-type &body param-forms)
      method-form
    (check-type method-name symbol)
    (check-type convention (member :stdcall :cdecl))
    (check-type return-type foreign-type)
    (let* ((this-sym (make-symbol "THIS"))
           (params (parse-method-params param-forms)))
      (multiple-value-bind (lambda-args default-init-forms bind-forms bind-init-forms call-forms result-forms)
          (build-param-forms params)
        (values
         ;; Lambda list
         `(,this-sym ,@lambda-args)
         ;; Body
         `((declare (type cffi:foreign-pointer ,this-sym))
           ,@default-init-forms
           (with-foreign-objects* (,@bind-forms)
             ,@bind-init-forms
             ,@(let ((funcall-form
                       `(cffi:foreign-funcall-pointer
                         (cffi:foreign-slot-value (cffi:foreign-slot-value ,this-sym ',if-name 'vtbl) ',if-vtbl-struct ',method-name)
                         (:convention ,convention)
                         :pointer ,this-sym
                         ,@call-forms
                         ,return-type)))
                 (cond
                   ((and (eq return-type 'win32:hresult) result-forms)
                    ;; Return type is hresult, and we have output params.
                    ;; Check error and then return the out param values.
                    `((check-com-error ,funcall-form)
                      (values ,@result-forms)))
                   ((eq return-type 'win32:hresult)
                    ;; Return type is hresult, and no output params.
                    ;; Check hresult error and return it
                    `((check-com-error ,funcall-form)))
                   ((eq (cffi::canonicalize-foreign-type return-type) :void)
                    ;; No return type other than the result forms, if any
                    `(,funcall-form
                      (values ,@result-forms)))
                   (t
                    ;; Return type is not hresult. Return its value directly, and
                    ;; as well as any (maybe zero) out params
                    `((values
                       ,funcall-form
                       ,@result-forms))))))))))))

(defun valid-iid-p (iid-str)
  (let ((scanner (load-time-value
                  (cl-ppcre:create-scanner "(?im)^{[0-9A-F]{8}[-]?(?:[0-9A-F]{4}[-]?){3}[0-9A-F]{12}}$"))))
    (and (stringp iid-str)
         (cl-ppcre:scan scanner iid-str)
         t)))

(defmacro define-com-interface (if-name iid (&optional base) &body methods)
  (unless (valid-iid-p iid)
    (error "Invalid iid: ~A" iid))
  (let* ((if-vtbl-struct (intern (format nil "~A-VTBL" if-name)))
         (full-method-list (append (when base (build-method-list base)) methods)))
    `(progn
       ;; Generate struct and vtbl struct
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; vtbl has the full, ordered method list
         (cffi:defcstruct ,if-vtbl-struct
           ,@(mapcar (lambda (m) `(,(car m) :pointer)) full-method-list))
         (cffi:defctype ,if-vtbl-struct (:struct ,if-vtbl-struct))

         ;; Struct has pointer to vtbl
         (cffi:defcstruct ,if-name
           (vtbl (:pointer ,if-vtbl-struct)))
         (cffi:defctype ,if-name (:struct ,if-name)))

       ;; Generate lisp functions to call each vtable function
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(loop
             :for method-form :in methods
             :for (lambda-list body) := (multiple-value-list (create-com-method-fn if-name if-vtbl-struct method-form))
             :for method-name := (car method-form)
             :for fn-name := (intern (format nil "~A-~A" if-name method-name))
             :collecting `(defun ,fn-name ,lambda-list ,@body)))

       ;; Set interface attributes
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf
          (interface-base ',if-name) ',base
          (interface-method-list ',if-name) ',methods
          (interface-iid ',if-name) ',iid
          (interface-struct ',if-name) ',if-name
          (interface-vtbl-struct ',if-name) ',if-vtbl-struct))
       ',if-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when-let (ti (find-package '#:trivial-indent))
    (funcall (fdefinition `(setf ,(intern "INDENTATION" ti))) '(6 4 6 &rest (&whole 2 1 4 4 &rest 2)) 'define-com-interface)))
