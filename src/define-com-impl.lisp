(in-package #:clom)

;;; COM Classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trampoline-sym (interface method)
    (or (assoc-value (get+ interface 'com-trampolines) method)
        (let ((sym (make-symbol (string method))))
          (pushnew (cons method sym) (get+ interface 'com-trampolines))
          sym)))

  (defun implementable-methods-list (interface)
    "Get the methods implementable by `interface'."
    (let ((i-unknown-methods (interface-method-list 'i-unknown)))
      (remove-if (lambda (m) (find (car m) i-unknown-methods :key #'car))
                 (build-method-list interface))))

  (defun make-vtbl (interface)
    "Allocate and fill out a vtbl for `interface'"
    (loop
      :with vtbl-type := (interface-vtbl-struct interface)
      :with vtbl := (cffi:foreign-alloc vtbl-type)
      :for method :in (build-method-list interface)
      :for (name return-type . method-args) := method
      :for trampoline-name := (trampoline-sym interface name)
      :do (setf (cffi:foreign-slot-value vtbl vtbl-type name) (cffi:get-callback trampoline-name))
      :finally (return vtbl)))

  (defun vtbl-ptr (interface)
    "Get the vtbl-ptr for `interface', otherwise initializing it."
    (multiple-value-bind (ptr foundp)
        (get+ interface 'vtbl-ptr)
      (unless foundp
        (error "~A has no vtbl" interface))
      (when (cffi:null-pointer-p ptr)
        (setf ptr (make-vtbl interface)
              (vtbl-ptr interface) ptr))
      ptr))

  (defun (setf vtbl-ptr) (value interface)
    (setf (get+ interface 'vtbl-ptr) value)))

(defclass com-impl ()
  ((ptr
    :type cffi:foreign-pointer
    :initform (cffi:null-pointer))
   (refcount
    :type non-negative-fixnum
    :initform 0)))

(defvar *%com-instances* (make-hash-table))

(defun pointer-com-impl (pointer)
  "Get the `com-impl' instance representing the foreign pointer `pointer'"
  (gethash (cffi:pointer-address pointer) *%com-instances*))

(defun (setf pointer-com-impl) (value pointer)
  (setf (gethash (cffi:pointer-address pointer) *%com-instances*) value))

(defun remove-pointer-com-impl (pointer)
  (remhash (cffi:pointer-address pointer) *%com-instances*)
  (values))

(defun com-class-interface (class)
  "Get the COM interface defined for the class `class'."
  (unless (symbolp class)
    (setf class (class-name class)))
  (get+ class 'com-class-interface))

(defun (setf com-class-interface) (value class)
  "Set the COM interface defined for the class `class'."
  (unless (symbolp class)
    (setf class (class-name class)))
  (setf (get+ class 'com-class-interface) value))

(defun com-impl-interface (com-impl)
  "Get the COM interface for `com-impl'"
  (com-class-interface (class-of com-impl)))

(defun com-impl-pointer (com-impl)
  "Get the foreign interface pointer for `com-impl'"
  (with-slots (ptr refcount) com-impl
    (when (cffi:null-pointer-p ptr)
      (let ((interface (com-impl-interface com-impl)))
        (setf ptr (cffi:foreign-alloc (interface-struct interface))
              (cffi:mem-ref ptr :pointer) (vtbl-ptr interface)
              (pointer-com-impl ptr) com-impl
              refcount 1)))
    ptr))

(defun genargs (args)
  "From a list of cffi-style args, generate a list in which the arg names have been gensym'd.
  (genargs '((foo :int) (bar :string)))
  ; =>
  ((#:foo :int) (#:bar :string))"
  (mapcar (lambda (arg)
            (destructuring-bind (name type &rest options) arg
              (declare (ignore options))
              (list (make-symbol (string name)) type)))
          args))

(defun arg-names (args)
  "From a list of cffi-style args, generate an ordered list of their names.
  (arg-names '((foo :int) (bar :string)))
  ; =>
  (foo bar)"
  (mapcar #'car args))

(defmacro use-trampolines (interface)
  "Create trampoline functions for `interface'."
  (let ((vtbl-cleanup-sym (make-symbol (format nil "~A-VTBL-CLEANUP" interface))))
    `(progn
       ;; Create a cleanup function.
       ;; We need a unique name for each so we
       (defun ,vtbl-cleanup-sym ()
         (setf (vtbl-ptr ',interface) (cffi:null-pointer)))

       ;; If we're the first to set up the trampolines
       (unless (get+ ',interface 'vtbl-ptr)
         ;; Initialize vtbl to nullptr
         (setf (vtbl-ptr ',interface) (cffi:null-pointer))
         ;; Set the cleanup hook for the vtbl
         (exit-hooks:add-exit-hook ',vtbl-cleanup-sym))

       ,@(loop
           :with this-sym := (make-symbol "THIS")
           :with this-ptr-sym := (make-symbol "THIS-PTR")
           :for method :in (build-method-list interface)
           :for (name (&rest options) return-type . method-args) := method
           :for trampoline-name := (trampoline-sym interface name)
           :for args := (genargs method-args)
           :for arg-names := (arg-names args)
           :appending
           `((defgeneric ,trampoline-name (,this-sym ,this-ptr-sym ,@arg-names))
             (cffi:defcallback (,trampoline-name :convention :stdcall) ,return-type
                 ((,this-ptr-sym (:pointer ,interface)) ,@args)
               (prog ()
                retry
                  (restart-case
                      (let ((,this-sym (pointer-com-impl ,this-ptr-sym)))
                        (unless ,this-sym
                          (error "Cannot find com-impl for pointer '~A'" ,this-ptr-sym))
                        (return
                          (cffi:convert-to-foreign
                           (,trampoline-name ,this-sym ,this-ptr-sym ,@arg-names)
                           ',return-type)))
                    (retry-method ()
                      :report ,(format nil "Retry calling the method '~A'" trampoline-name)
                      (go retry))))))))))

;;; Setup base trampolines for i-unknown

(use-trampolines i-unknown)

(defmethod #.(trampoline-sym 'i-unknown 'add-ref) ((this com-impl) this-ptr)
  (when (= (incf (slot-value this 'refcount)) 1)
    (error "Impossible"))
  win32:+s-ok+)

(defmethod #.(trampoline-sym 'i-unknown 'query-interface) ((this com-impl) this-ptr riid ppv-obj)
  (if (cffi:null-pointer-p ppv-obj)
      win32:+e-pointer+
      (cffi:with-foreign-object (guid 'win32:iid)
        (loop
          :for interface := (com-impl-interface this) :then (interface-base interface)
          :while interface
          :for hres := (win32:iid-from-string (interface-iid interface) guid)
          :if (and (/= hres win32:+no-error+) (win32:is-equal-iid guid riid))
            :do (setf (cffi:mem-ref ppv-obj :pointer) this-ptr)
            :and :do (i-unknown-add-ref this-ptr)
            :and :return win32:+s-ok+
          :finally (return win32:+e-nointerface+)))))

(defmethod #.(trampoline-sym 'i-unknown 'release) ((this com-impl) this-ptr)
  (with-slots (ptr refcount) this
    (when (zerop (decf refcount))
      (remove-pointer-com-impl ptr)
      (cffi:foreign-free ptr)
      (setf ptr (cffi:null-pointer)))
    refcount))

(defmacro define-com-impl (name (interface) (&rest slots) &body methods)
  ;; Sanity-check the methods
  (let* ((decls (implementable-methods-list interface))
         (i-unknown-decls (interface-method-list 'i-unknown))
         (method-pairs
           (mapcar (lambda (m) (cons m (find (car m) methods :key #'car))) decls)))

    ;; Check for missing definitions
    (when-let ((undefined-decls (mapcar #'car (remove-if #'cdr method-pairs))))
      (error "com-impl ~A does not define methods: ~{~A~^, ~}" name (mapcar #'car undefined-decls)))

    ;; Check for i-unknown methods, which are unimplementable
    (when-let ((i-unknown-defs (remove-if-not (lambda (m) (find (car m) i-unknown-decls :key #'car)) methods)))
      (error "com-impl ~A defines methods for I-Unknown: ~{~A~^, ~}" name (mapcar #'car i-unknown-defs)))

    ;; Check for undeclared methods
    (when-let ((undeclared-defs (remove-if (lambda (m) (find (car m) decls :key #'car)) methods)))
      (error "com-impl ~A defines methods not declared in ~A: ~{~A~^, ~}" name interface (mapcar #'car undeclared-defs)))

    ;; Check for methods with mismatched argument counts
    (when-let ((mismatched-arg-counts
                (mapcar #'car
                        (remove-if
                         (lambda (pair)
                           (destructuring-bind (decl . def) pair
                             (= (length (cddr decl)) (length (second def)))))
                         method-pairs))))
      (error "com-impl ~A defines methods with incorrect arity: ~{~A~^, ~}" name (mapcar #'car mismatched-arg-counts)))

    ;; On to the actual definition
    (let ((this (intern "THIS")) (this-ptr (intern "THIS-PTR")))
      `(progn
         ;; Define the class
         (defclass ,name (com-impl)
           (,@slots))

         ;; Set the com interface for this class
         (setf (com-class-interface ',name) ',interface)

         ;;Ensure trampolines exist
         (use-trampolines ,interface)

         ,@(mapcar
            (lambda (pair)
              (destructuring-bind (decl . def) pair
                `(defmethod ,(trampoline-sym interface (car decl)) ((,this ,name) ,this-ptr ,@(second def))
                   ,@(cddr def))))
            method-pairs)
         ',name))))

(trivial-indent:define-indentation define-com-impl (6 4 4 (&whole 2 &rest 1) &rest (&whole 2 4 &lambda &body)))
