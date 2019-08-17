(in-package #:clom)

(defstruct (param-spec
            (:type list)
            (:constructor make-param-spec (name type flags)))
  (name (required-argument :name)
   :type symbol)
  (type (required-argument :type)
   :type foreign-type)
  (flags (required-argument :flags)
   :type list))

(defun parse-param-spec-flags (spec-flags)
  "Parse out a list of parameter spec-flags into an alist of (keyword . value) pairs.
  Signals errors on unknown keywords, duplicate flags, etc."
  (loop
    :for rest := spec-flags :then (cdr rest)
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

(defun parse-param-spec (param-spec)
  "Parse out a parameter spec into `(name type . flags)', verifying them along the way."
  (destructuring-bind (name type &rest flags) param-spec
    (check-type name symbol)
    (check-type type foreign-type)
    (make-param-spec name type (parse-param-spec-flags flags))))

(defun parse-method-params (param-specs)
  "Parse out the parameter specs for a method declaration.
 Each param-spec is of the form
   (name type flags*)"
  (let ((ret (mapcar #'parse-param-spec param-specs)))
    (when-let ((retvals (remove-if-not (lambda (param) (assoc :retval (param-spec-flags param))) ret)))
      (when (cdr retvals)
        (error "Duplicate parameters with `:retval' flag: ~A" (mapcar #'param-spec-name retvals))))
    ret))

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
      (let ((name (param-spec-name param))
            (type (param-spec-type param)))
        (macrolet ((flag (flag) `(assoc-value (param-spec-flags param) ,flag)))
          (when (flag :opt)
            (push`(setf ,name (or ,name ,(flag :default))) init-forms)
            (unless req-names
              (push `(,name ,(flag :default)) opt-forms)))
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
  (destructuring-bind (method-name (&key (convention :stdcall) (invoke-kind :func))
                       return-type &body param-forms)
      method-form
    (check-type method-name symbol)
    (check-type convention (member :stdcall :cdecl))
    (check-type invoke-kind (member :func :prop-get :prop-put :prop-put-ref))
    (check-type return-type foreign-type)
    (let* ((this-sym (make-symbol "THIS"))
           (params (parse-method-params param-forms)))
      (multiple-value-bind (lambda-args default-init-forms bind-forms bind-init-forms call-forms result-forms)
          (build-param-forms params)
        (assert (or (eq invoke-kind :func)
                    (and (eq invoke-kind :prop-get) (= 0 (length lambda-args)))
                    (= 1 (length lambda-args))))
        (values
         ;; Name
         method-name
         (list :convention convention :invoke-kind invoke-kind)
         ;; Lambda list
         `(,this-sym ,@lambda-args)
         ;; Body
         `((declare (type cffi:foreign-pointer ,this-sym))
           ,@default-init-forms
           ,@(let ((body-forms
                     `(,@bind-init-forms
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
                                 ,@result-forms))))))))
               (if bind-forms
                   ;; Only wrap it up in `with-foreign-objects' if we have any bindings
                   `((with-foreign-objects* (,@bind-forms)
                       ,@body-forms))
                   body-forms))))))))

(defmacro define-com-interface (if-name iid (&optional base) &body methods)
  "Defines a com interface with name `if-name' and `iid', optionally inheriting from `base'.
 `methods' describes a list of method declarations of the form
 (name (flags*) return-type
   (arg-name arg-type arg-flags*))"
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
             :for (method-name method-properties lambda-list body) := (multiple-value-list (create-com-method-fn if-name if-vtbl-struct method-form))
             :for setter-p := (member (getf method-properties :invoke-kind) '(:prop-put :prop-put-ref))
             :for fn-name := (intern (format nil "~A-~A" if-name method-name))
             :for defun-name := (if (not setter-p) fn-name `(setf ,fn-name))
             :for defun-lambda-list := (if (not setter-p) lambda-list (cons (lastcar lambda-list) (butlast lambda-list)))
             :collecting `(defun ,defun-name ,defun-lambda-list ,@body)))

       ;; Set interface attributes
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf
          (interface-base ',if-name) ',base
          (interface-method-list ',if-name) ',methods
          (interface-iid ',if-name) ',iid
          (interface-struct ',if-name) ',if-name
          (interface-vtbl-struct ',if-name) ',if-vtbl-struct))
       ',if-name)))

(define-indentation define-com-interface
    (6 4 6 &rest (&whole 2 1 4 4 &rest 2)))
