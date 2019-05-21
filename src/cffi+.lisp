(in-package #:clom)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foreign-type-p (foreign-type)
    (handler-case (and (cffi::parse-type foreign-type) t)
      (error () nil))))

(deftype foreign-type ()
  "A foreign type designator known to CFFI"
  `(satisfies foreign-type-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foreign-pointer-type-p (foreign-type)
    (handler-case (eq (cffi::canonicalize-foreign-type foreign-type) :pointer)
      (error () nil))))

(deftype foreign-pointer-type ()
  "A foreign type designator which is a pointer type."
  `(satisfies foreign-pointer-type-p))

(cltl2:define-declaration cffi-type (arg-var env-var)
  (declare (ignore env-var))
  (values
   :variable
   (destructuring-bind (name cffi-type &rest variables) arg-var
     (mapcar (lambda (v)
               (list v name cffi-type))
             variables))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cffi-type (var &optional env)
    "Returns the `cffi-type' of the variable `var' in `env' or `nil' if variable has no such type."
    (multiple-value-bind (binding local-p decl)
        (cltl2:variable-information var env)
      (declare (ignore binding local-p))
      (cdr (assoc 'cffi-type decl)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cffi-type-or-error (var &optional env)
    "Returns the `cffi-type' of the variable `var' in `env', or signals an error."
    (or (cffi-type var env)
        (error "'~A' has no visible cffi type" var))))

(defmacro &* (variable &optional (index 0) &environment env)
  "As `cffi:mem-aref', but utilizes the `variable's declared `cffi-type'"
  `(cffi:mem-aref ,variable ',(cffi-type-or-error variable env) ,index))

(define-setf-expander &* (variable &optional (index 0) &environment env)
  "As `cffi:mem-aref', but utilizes the `variable's declared `cffi-type'"
  (let ((cffi-type (cffi-type-or-error variable env))
        (index-var (gensym "INDEX"))
        (value-var (gensym "VALUE")))
    (values `(,@(unless (constantp index env) (list index-var)))
            `(,@(unless (constantp index env) (list index)))
            `(,value-var)
            `(setf (cffi:mem-aref ,variable ',cffi-type ,@(list (if (constantp index env) index index-var))) ,value-var)
            `(cffi:mem-aref ,variable ',cffi-type ,@(list (if (constantp index env) index index-var))))))

(defmacro with-foreign-object* ((var type &optional (count 1)) &body body &environment env)
  "As `cffi:with-foreign-object', but additionally declares the `cffi-type' of `var'"
  (unless (constantp type env)
    (error "with-foreign-object* can make use of constant types only."))
  `(cffi:with-foreign-object (,var ,type ,count)
     (locally
         (declare (cffi-type ,(eval type) ,var))
       ,@body)))

(defmacro with-foreign-objects* (bindings &body body)
  (if bindings
      `(with-foreign-object* ,(car bindings)
         (with-foreign-objects* ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric type-pointer-type (type)
    (:documentation "Get the foreign type that `type' points to.
  eg:
    (type-pointer-type '(:pointer :int)) ; =>
    :int"))

  (defmethod type-pointer-type (type)
    (error "Type is not a pointer type: '~A'" type))

  (defmethod type-pointer-type ((type symbol))
    (type-pointer-type (cffi::parse-type type)))

  (defmethod type-pointer-type ((type list))
    (type-pointer-type (cffi::parse-type type)))

  (defmethod type-pointer-type ((type cffi::foreign-type-alias))
    (type-pointer-type (cffi::actual-type type)))

  (defmethod type-pointer-type ((type cffi::foreign-pointer-type))
    (let ((ptr-type (cffi::pointer-type type)))
      (if ptr-type
          (cffi::unparse-type ptr-type)
          :void))))

(defmacro with-foreign-slots* ((&rest slots) variable &body body &environment env)
  "Similar to `cffi:with-foreign-slots', but uses `variable's `cffi-type'
  Additionally, any `pointer' bindings, whether by the slot being a pointer,
  or by using `(:pointer slot)` syntax will have its `cffi-type' declared as well."
  (let* ((type (cffi-type-or-error variable env))
         (pointer-bindings (remove-if-not (lambda (var) (and (consp var) (eq (car var) :pointer))) slots))
         (pointer-vars (mapcar #'cadr pointer-bindings))
         (vars-with-ptr-type (remove-if-not (lambda (slot) (and (atom slot) (foreign-pointer-type-p (cffi:foreign-slot-type type slot))))
                                            slots))
         (decls (append
                 (mapcar (lambda (slot)
                           `(declare (cffi-type ,(cffi:foreign-slot-type type slot) ,slot)))
                         pointer-vars)
                 (mapcar (lambda (slot)
                           `(declare (cffi-type ,(type-pointer-type (cffi:foreign-slot-type type slot)) ,slot)))
                         vars-with-ptr-type))))
    `(cffi:with-foreign-slots (,slots ,variable ,type)
       (locally ,@decls
         ,@body))))

(defmacro cffi-let ((&rest bindings) &body body)
  "Similar to `let', but each binding is as follows:
  (var type &optional (value (cffi:null-pointer)))
  Will declare the `cffi-type' of each binding."
  (loop
    :with bind-forms := ()
    :with decl-forms := ()
    :for binding :in bindings
    :do
       (destructuring-bind (var type &optional (value '(cffi:null-pointer))) binding
         (check-type var symbol)
         (check-type type foreign-type)
         (push `(,var ,value) bind-forms)
         (push `(declare (cffi-type ,type ,var)) decl-forms))
    :finally
       (setf bind-forms (nreverse bind-forms)
             decl-forms (nreverse decl-forms))
       (when bind-forms
         (push `(declare (type cffi:foreign-pointer ,@(mapcar #'car bind-forms))) decl-forms))
       (return
         `(let ,bind-forms
            ,@decl-forms
            ,@body))))

;; (defmacro &-> (variable &rest slot-path &environment env)
;;   (loop
;;     :with type := (cffi-type-or-error variable env)
;;     :with ptr-stack := (let ((ptr-type (type-pointer-type type)))
;;                          (unless (eq ptr-type :void)
;;                            (list ptr-type)))
;;     :with exp-val := `(cffi:mem-ref ,variable ',type)
;;     :with exp-ptr := variable
;;     :for spec :in slot-path
;;     :do
;;        (cond
;;          ((atom spec)
;;           (let* ((slot-name spec)
;;                  (slot-type (cffi:foreign-slot-type type slot-name)))
;;             (unless exp-ptr
;;               (error "Error accessing slot '~A' from '~A': expression not a pointer type" slot-name exp-val))
;;             (cond
;;               ((/= 1 (cffi:foreign-slot-count type slot-name))
;;                ;; Getting array slot
;;                (setf exp-val `(cffi:foreign-slot-pointer ,exp-ptr ',type ',slot-name)
;;                      exp-ptr exp-val
;;                      type slot-type))
;;               ((eq :pointer (cffi::canonicalize-foreign-type slot-type))
;;                ;; Getting slot which is a pointer
;;                (setf exp-val `(cffi:foreign-slot-value ,exp-ptr ',type ',slot-name)
;;                      exp-ptr exp-val
;;                      type (type-pointer-type slot-type)))
;;               (t
;;                ;; Getting a 'bare' slot
;;                (setf exp-val `(cffi:foreign-slot-value ,exp-ptr ',type ',slot-name)
;;                      exp-ptr `(cffi:foreign-slot-pointer ,exp-ptr ',type ',slot-name)
;;                      type slot-type)))))
;;          ((eq (car spec) '&)
;;           (let* ((slot-name (cadr spec))
;;                  (slot-type (cffi:foreign-slot-type type slot-name)))
;;             (setf exp-val `(cffi:foreign-slot-pointer ,exp-ptr ',type ',slot-name)
;;                   exp-ptr `(cffi:foreign-slot-pointer ,exp-ptr ',type ',slot-name)
;;                   type slot-type)))
;;          ((eq (car spec) '*)
;;           (unless ptr-p
;;             (error "Error array accessing '~A': expression not a pointer type" expression))
;;           (let* ((index (cadr spec))
;;                  (res-type type))
;;             (prog1 `(cffi:mem-aref ,expression ',type ,index)
;;               (setf type slot-type)))))
;;     :finally (return exp-val)))

