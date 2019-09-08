(in-package #:clom-tlb)

(defun guid-to-string (guid)
  (declare (cffi-type win32:guid guid))
  (with-foreign-slots* (win32:data1 win32:data2 win32:data3 (:pointer win32:data4))
                       guid
    (macrolet ((data4 (x) `(&* win32:data4 ,x)))
      (format nil "{~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X}"
              win32:data1 win32:data2 win32:data3
              (data4 0) (data4 1) (data4 2) (data4 3)
              (data4 4) (data4 5) (data4 6) (data4 7)))))


(defmacro with-com-object ((var com-object) &body body)
  "Binds `var' to the result of `com-object' and ensures `i-unknown-release' is called on exit."
  (multiple-value-bind (body decl)
      (parse-body body)
    `(let ((,var ,com-object))
       (declare (type cffi:foreign-pointer ,var))
       ,@decl
       (unwind-protect (progn ,@body)
         (i-unknown-release ,var)))))

(defun list-enum-flags (val enumeration)
  (if-let ((zero-val (and (zerop val) (cffi:foreign-enum-keyword enumeration 0 :errorp nil))))
    (list zero-val)
    (loop
      :for enum-sym :in (cffi:foreign-enum-keyword-list enumeration)
      :for enum-val := (cffi:foreign-enum-value enumeration enum-sym)
      :if (and (/= enum-val 0) (= (logand enum-val val) enum-val))
        :collect enum-sym)))

(defun load-type-lib* (path)
  (with-foreign-object* (p-type-lib '(:pointer i-type-lib))
    (let ((hr (win32:load-type-lib (uiop:native-namestring path) p-type-lib)))
      (check-com-error hr)
      (&* p-type-lib))))

(defmacro do-types ((var type-lib &optional result-form) &body body)
  (let ((type-lib-sym (gensym "TYPE-LIB"))
        (i-sym (gensym "I")))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(let ((,type-lib-sym ,type-lib))
         (dotimes (,i-sym (i-type-lib-get-type-info-count ,type-lib-sym)
                          ,@(when result-form `((let (,var) ,var ,result-form))))
           (with-com-object (,var (i-type-lib-get-type-info ,type-lib-sym ,i-sym))
             ,@decl
             (tagbody ,@body)))))))

(defun i-type-info-get-name* (type-info memid)
  (with-foreign-object* (bstr 'win32:bstr)
    (when (= (i-type-info-get-names type-info memid bstr 1) 1)
      (bstr-to-lisp (&* bstr)))))

(defun i-type-info-get-names* (type-info memid num)
  (with-foreign-object* (ary 'win32:bstr num)
    (loop
      :with count := (i-type-info-get-names type-info memid ary num)
      :with result-names := ()
      :for i :from 0 :below count
      :do (push (bstr-to-lisp (&* ary i)) result-names)
      :finally
         (loop :for i :from count :below num
               :do (push nil result-names))
         (return (nreverse result-names)))))

(defun i-type-info-get-impl-type-info* (type-info index)
  (i-type-info-get-ref-type-info type-info (i-type-info-get-ref-type-of-impl-type type-info index)))

(declaim (inline typeattr=))
(defun typeattr= (t1 t2)
  (declare (cffi-type typeattr t1 t2))
  (win32:is-equal-guid
   (cffi:foreign-slot-pointer t1 'typeattr 'guid)
   (cffi:foreign-slot-pointer t2 'typeattr 'guid)))

(defmacro with-typeattr ((var type-info) &body body)
  "Bind `var' to the `typeattr' of `type-info'"
  (let ((type-info-sym (gensym "TYPE-INFO")))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(let ((,type-info-sym ,type-info))
         (cffi-let ((,var typeattr (i-type-info-get-type-attr ,type-info-sym)))
           ,@decl
           (unwind-protect (progn ,@body)
             (i-type-info-release-type-attr ,type-info-sym ,var)))))))

(declaim (inline type-info=))
(defun type-info= (t1 t2)
  (declare (cffi-type i-type-info t1 t2))
  (or (cffi:pointer-eq t1 t2)
      (with-typeattr (tat1 t1)
        (with-typeattr (tat2 t2)
          (typeattr= tat1 tat2)))))

(defmacro do-vars ((var type-info &optional result-form) &body body)
  "Loop over the variables of `type-info'"
  (let ((type-info-sym (gensym "TYPE-INFO"))
        (c-vars-sym (gensym "C-VARS"))
        (typeattr-sym (gensym "TYPEATTR"))
        (i-sym (gensym "I")))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(let* ((,type-info-sym ,type-info)
              (,c-vars-sym (with-typeattr (,typeattr-sym ,type-info-sym)
                             (cffi:foreign-slot-value ,typeattr-sym 'typeattr 'c-vars))))
         (dotimes (,i-sym ,c-vars-sym ,@(when result-form `((let (,var) ,var ,result-form))))
           (cffi-let ((,var vardesc (i-type-info-get-var-desc ,type-info-sym ,i-sym)))
             ,@decl
             (unwind-protect (tagbody ,@body)
               (i-type-info-release-var-desc ,type-info-sym ,var))))))))

(defmacro do-funcs ((var type-info &optional result-form) &body body)
  (let ((type-info-sym (gensym "TYPE-INFO"))
        (c-funcs-sym (gensym "C-FUNCS"))
        (typeattr-sym (gensym "TYPEATTR"))
        (i-sym (gensym "I")))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(let* ((,type-info-sym ,type-info)
              (,c-funcs-sym (with-typeattr (,typeattr-sym ,type-info-sym)
                             (cffi:foreign-slot-value ,typeattr-sym 'typeattr 'c-funcs))))
         (dotimes (,i-sym ,c-funcs-sym ,@(when result-form `((let (,var) ,var ,result-form))))
           (cffi-let ((,var funcdesc (i-type-info-get-func-desc ,type-info-sym ,i-sym)))
             ,@decl
             (unwind-protect (tagbody ,@body)
               (i-type-info-release-func-desc ,type-info-sym ,var))))))))

(defmacro with-href-type ((var type-info href) &body body)
  `(with-com-object (,var (i-type-info-get-ref-type-info ,type-info ,href))
     ,@body))

(defmacro do-impl-types ((var type-info &optional result-form) &body body)
  (let ((type-info-sym (gensym "TYPE-INFO"))
        (c-impl-types-sym (gensym "C-IMPL-TYPES"))
        (typeattr-sym (gensym "TYPEATTR"))
        (i-sym (gensym "I")))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(let* ((,type-info-sym ,type-info)
              (,c-impl-types-sym (with-typeattr (,typeattr-sym ,type-info-sym)
                                   (cffi:foreign-slot-value ,typeattr-sym 'typeattr 'c-impl-types))))
         (dotimes (,i-sym ,c-impl-types-sym ,@(when result-form `((let (,var) ,var ,result-form))))
           (with-com-object (,var (i-type-info-get-impl-type-info* ,type-info-sym ,i-sym))
             ,@decl
             (tagbody ,@body)))))))

(defun get-type-info-name (type-info)
  (i-type-info-get-documentation type-info -1))

(defun tdesc-vt (tdesc)
  (cffi:foreign-slot-value tdesc 'typedesc 'vt))

(defun ensure-vt (tdesc &rest vts)
  (unless (member (tdesc-vt tdesc) vts)
    (cerror "Continue anyway" "VT '~A' is not in ~A" (tdesc-vt tdesc) vts)))

(defun tdesc-tdesc (tdesc)
  (ensure-vt tdesc win32:+vt-ptr+ win32:+vt-safearray+)
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer tdesc 'typedesc 'typedesc-union)
   'typedesc-union 'lptdesc))

(defun tdesc-adesc (tdesc)
  (ensure-vt tdesc win32:+vt-carray+)
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer tdesc 'typedesc 'typedesc-union)
   'typedesc-union 'lpadesc))

(defun tdesc-href (tdesc)
  (ensure-vt tdesc win32:+vt-userdefined+)
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer tdesc 'typedesc 'typedesc-union)
   'typedesc-union 'hreftype))

(defun elemdesc-tdesc (elemdesc)
  (cffi:foreign-slot-pointer elemdesc 'elemdesc 'tdesc))

(defun elemdesc-vt (elemdesc)
  (tdesc-vt (elemdesc-tdesc elemdesc)))

(defun list-impl-hrefs (type-info typeattr)
  (loop
    :with c-impl-types := (cffi:foreign-slot-value typeattr 'typeattr 'c-impl-types)
    :for i :from 0 :below c-impl-types
    :collect (i-type-info-get-ref-type-of-impl-type type-info i)))

(defun pascal-case-to-lisp-case (name)
  (when (zerop (length name))
    (return-from pascal-case-to-lisp-case ""))
  (loop
    :with lisp-case := (make-array (length name) :adjustable t :fill-pointer 0 :element-type 'character)
      :initially (vector-push-extend (char-upcase (char name 0)) lisp-case)
    :with last-char := (char name (1- (length name)))
    :for i :from 1 :below (1- (length name))
    :for curch := (char name i)
    :for to-insert := (char-upcase curch)
    :if (upper-case-p curch)
      :do (cond
            ((lower-case-p (char name (1- i))) #1=(progn (vector-push-extend #\- lisp-case)
                                                         (vector-push-extend to-insert lisp-case)))
            ((and (lower-case-p (char name (1+ i))) (alpha-char-p (char name (1- i)))) #1#)
            (t (vector-push-extend to-insert lisp-case)))
    :else
      :do (vector-push-extend to-insert lisp-case)
    :finally
       (when (> (length name) 1)
         (cond
           ((lower-case-p last-char)
            (vector-push-extend (char-upcase last-char) lisp-case))
           ((lower-case-p (char name (1- i)))
            (vector-push-extend #\- lisp-case)
            (vector-push-extend last-char lisp-case))
           (t
            (vector-push-extend last-char lisp-case))))
       (return lisp-case)))

(defun make-type-name (type-info)
  (pascal-case-to-lisp-case (get-type-info-name type-info)))

(defun make-param-name (name)
  (pascal-case-to-lisp-case name))

(defun make-method-name (funcdesc type-info)
  (pascal-case-to-lisp-case (i-type-info-get-name* type-info (cffi:foreign-slot-value funcdesc 'funcdesc 'memid))))

(defun make-enum-name (type-info vardesc)
  (pascal-case-to-lisp-case (i-type-info-get-name* type-info (cffi:foreign-slot-value vardesc 'vardesc 'memid))))

(defun typedesc-to-type (tdesc type-info)
  (let* ((vt (tdesc-vt tdesc))
         (byref (= win32:+vt-byref+ (logand win32:+vt-byref+ vt)))
         ;; (array (= win32:+vt-array+ (logand win32:+vt-array+ vt)))
         (vt (logandc1 (logior win32:+vt-byref+ win32:+vt-array+) vt))
         (base-type
           (ecase vt
             (#.win32:+vt-int+ :int)
             (#.win32:+vt-i1+ :int8)
             (#.win32:+vt-i2+ :int16)
             (#.win32:+vt-i4+ :int32)
             (#.win32:+vt-i8+ :int64)
             (#.win32:+vt-uint+ :uint)
             (#.win32:+vt-ui1+ :uint8)
             (#.win32:+vt-ui2+ :uint16)
             (#.win32:+vt-ui4+ :uint32)
             (#.win32:+vt-ui8+ :uint64)
             (#.win32:+vt-r4+ :float)
             (#.win32:+vt-r8+ :double)
             (#.win32:+vt-decimal+ 'win32:decimal)
             (#.win32:+vt-cy+ 'win32:cy)
             (#.win32:+vt-date+ 'win32:date)
             (#.win32:+vt-bstr+ 'win32:bstr)
             (#.win32:+vt-dispatch+ '(:pointer i-dispatch))
             (#.win32:+vt-error+ 'win32:scode)
             (#.win32:+vt-bool+ 'win32:bool)
             (#.win32:+vt-variant+ '(:pointer variant))
             (#.win32:+vt-unknown+ '(:pointer i-unknown))
             (#.win32:+vt-void+ :void)
             (#.win32:+vt-hresult+ 'win32:hresult)
             (#.win32:+vt-lpstr+ 'win32:lpstr)
             (#.win32:+vt-lpwstr+ 'win32:lpwstr)
             (#.win32:+vt-int-ptr+ 'win32:int-ptr)
             (#.win32:+vt-uint-ptr+ 'win32:uint-ptr)
             (#.win32:+vt-filetime+ 'win32:filetime)
             (#.win32:+vt-safearray+ (list 'safearray (typedesc-to-type (tdesc-tdesc tdesc) type-info)))
             (#.win32:+vt-carray+ (list 'safearray (typedesc-to-type (tdesc-adesc tdesc) type-info)))
             (#.win32:+vt-ptr+ (list :pointer (typedesc-to-type (tdesc-tdesc tdesc) type-info)))
             (#.win32:+vt-userdefined+
              (with-href-type (href-type type-info (tdesc-href tdesc))
                (intern (make-type-name href-type)))))))
    (if byref
        (cons :pointer (ensure-list base-type))
        base-type)))

(defun elemdesc-to-type (elemdesc type-info)
  (typedesc-to-type (elemdesc-tdesc elemdesc) type-info))

(defun construct-enum-def (type-info typeattr)
  (declare (ignore typeattr))
  (let ((vars nil)
        (enum-type nil))
    (do-vars (vardesc type-info)
      (with-foreign-slots* (memid (:pointer vardesc-union) elemdesc-var w-var-flags varkind)
                           vardesc
        (unless (eq varkind 'var-const)
          (error "Enumeration with non-constant value"))
        (let ((name (intern (make-enum-name type-info vardesc)))
              (value (%variant-to-value (cffi:mem-ref vardesc-union '(:pointer variant))))
              (type (elemdesc-to-type elemdesc-var type-info)))
          (unless (or (null enum-type) (equal enum-type type))
            (error "Conflicting variable type in enumeration: ~A vs ~A" enum-type type))
          (setf enum-type type)
          (push (list name value) vars))))
    (when (null enum-type)
      (setf enum-type :int))
    `(cffi:defcenum (,(intern (make-type-name type-info)) ,enum-type)
       ,@(nreverse vars))))

(defun construct-record-def (type-info typeattr))
(defun construct-module-def (type-info typeattr))

(defun type-info-base-name (type-info typeattr)
  (declare (cffi-type typeattr typeattr))
  (with-foreign-slots* (c-impl-types) typeattr
    (case c-impl-types
      (0 nil)
      (1 (with-com-object (base (i-type-info-get-impl-type-info* type-info 0))
           (make-type-name base)))
      (t (error "Type info has ~D bases..." c-impl-types)))))

(defun parse-wparamflags (paramdesc type-info)
  (declare (ignore type-info))
  (declare (cffi-type paramdesc paramdesc))
  (with-foreign-slots* (wparamflags pparamdescex) paramdesc
    (let ((ret ()))
      (flet ((flag-p (flag) (not (zerop (logand flag wparamflags)))))
        (when (flag-p paramflag-fhascustdata)
          (error "cust data not implemented"))
        (when (flag-p paramflag-fhasdefault)
          (with-foreign-slots* (var-default-value) pparamdescex
            (push (%variant-to-value var-default-value) ret))
          (push :default ret))
        (when (flag-p paramflag-fopt)
          (push :opt ret))
        (when (flag-p paramflag-fretval)
          (push :retval ret))
        (when (flag-p paramflag-flcid)
          (push :lcid ret))
        (when (flag-p paramflag-fout)
          (push :out ret))
        (when (flag-p paramflag-fin)
          (push :in ret)))
      ret)))

(defun construct-param-defs (funcdesc type-info)
  (declare (cffi-type funcdesc funcdesc))
  (with-foreign-slots* (memid invkind c-params lprgelemdesc-param) funcdesc
    (with-foreign-object* (names-ary 'win32:bstr (1+ c-params))
      (loop
        :with count := (i-type-info-get-names type-info memid names-ary (1+ c-params))
        :for i :from 0 :below c-params
        :for elemdesc := (&* lprgelemdesc-param i)
        :for paramdesc := (cffi:foreign-slot-pointer elemdesc 'elemdesc 'elemdesc-union)
        :for flags := (parse-wparamflags paramdesc type-info)
        :for name := (cond
                       ((< (1+ i) count)
                        (make-symbol (make-param-name (bstr-to-lisp (&* names-ary (1+ i))))))
                       ((member invkind '(invoke-propertyput invoke-propertyputref))
                        (make-symbol "VALUE"))
                       (t
                        (make-symbol "UNNAMED")))
        :for type := (elemdesc-to-type elemdesc type-info)
        :collect `(,name ,type ,@flags)))))

(defun parse-callconv (callconv)
  (ecase callconv
    (cc-stdcall :stdcall)
    (cc-cdecl :cdecl)))

(defun parse-invkind (invkind)
  (ecase invkind
    (invoke-func :func)
    (invoke-propertyget :prop-get)
    (invoke-propertyput :prop-put)
    (invoke-propertyputref :prop-put)))

(defun construct-method-def (funcdesc type-info)
  (declare (cffi-type funcdesc funcdesc))
  (with-foreign-slots* (memid lprgscode callconv invkind lprgelemdesc-param elemdesc-func
                        c-params w-func-flags)
                       funcdesc
    (let ((name (intern (make-method-name funcdesc type-info)))
          (convention (parse-callconv callconv))
          (invoke-kind (parse-invkind invkind))
          (ret-type (elemdesc-to-type elemdesc-func type-info)))
      `(,name (,@(unless (eq convention :stdcall) `(:convention ,convention))
               ,@(unless (eq invoke-kind :func) `(:invoke-kind ,invoke-kind)))
              ,ret-type
              ,@(construct-param-defs funcdesc type-info)))))

(defun construct-interface-def (type-info typeattr)
  (let ((name (make-type-name type-info))
        (base-name (type-info-base-name type-info typeattr))
        (uuid (guid-to-string (cffi:foreign-slot-pointer typeattr 'typeattr 'guid))))
    `(define-com-interface ,(intern name) ,uuid ,(when base-name (list (intern base-name)))
       ,@(let ((methods ()))
           (do-funcs (funcdesc type-info)
             (push (construct-method-def funcdesc type-info) methods))
           (nreverse methods)))))

(defun construct-dispatch-def (type-info typeattr))
(defun construct-coclass-def (type-info typeattr))
(defun construct-alias-def (type-info typeattr))
(defun construct-union-def (type-info typeattr))

(defun construct-type-definition (type-info typeattr)
  (declare (type cffi:foreign-pointer type-info typeattr))
  (case (cffi:foreign-slot-value typeattr 'typeattr 'typekind)
    (tkind-enum (construct-enum-def type-info typeattr))
    (tkind-record (construct-record-def type-info typeattr))
    (tkind-module (construct-module-def type-info typeattr))
    (tkind-interface (construct-interface-def type-info typeattr))
    (tkind-dispatch (construct-dispatch-def type-info typeattr))
    (tkind-coclass (construct-coclass-def type-info typeattr))
    (tkind-alias (construct-alias-def type-info typeattr))
    (tkind-union (construct-union-def type-info typeattr))))

(defun %construct-tlb-import (type-lib)
  (typecase type-lib
    (cffi:foreign-pointer
     (let ((forms nil))
       (do-types (type type-lib)
         (with-typeattr (typeattr type)
           (when-let ((type-def (construct-type-definition type typeattr)))
             (push type-def forms))))
       (nreverse forms)))
    (t
     (with-com-object (lib (load-type-lib* type-lib))
       (%construct-tlb-import lib)))))

(defmacro tlb-import (name)
  (let ((forms (%construct-tlb-import name)))
    `(progn ,@forms)))

(defun %variant-to-value (variant)
  (cffi:with-foreign-slots ((vt (:pointer variant-union)) variant variant)
    (macrolet ((val (type)
                 `(cffi:mem-ref variant-union ',type)))
      (if (= (logand vt win32:+vt-byref+) win32:+vt-byref+)
          (val :pointer)
          (ecase vt
            (#.win32:+vt-empty+ (values))
            (#.win32:+vt-null+ nil)
            (#.win32:+vt-i2+ (val win32:short))
            (#.win32:+vt-i4+ (val win32:long))
            (#.win32:+vt-r4+ (val win32:float))
            (#.win32:+vt-r8+ (val win32:double))
            (#.win32:+vt-cy+ (val win32:cy))
            (#.win32:+vt-date+ (val win32:date))
            (#.win32:+vt-bstr+ (bstr-to-lisp (val win32:bstr) nil))
            (#.win32:+vt-dispatch+ (val (:pointer i-dispatch)))
            (#.win32:+vt-error+ (val win32:scode))
            (#.win32:+vt-bool+ (val win32:variant-bool))
            (#.win32:+vt-variant+ (error "Unsupported"))
            (#.win32:+vt-unknown+ (val (:pointer i-unknown)))
            (#.win32:+vt-decimal+ (cffi:mem-ref variant 'win32:decimal))
            (#.win32:+vt-record+ (error "Unsupported"))
            (#.win32:+vt-i1+ (val win32:char))
            (#.win32:+vt-ui1+ (val win32:byte))
            (#.win32:+vt-ui2+ (val win32:ushort))
            (#.win32:+vt-ui4+ (val win32:ulong))
            (#.win32:+vt-i8+ (val win32:longlong))
            (#.win32:+vt-ui8+ (val win32:ulonglong))
            (#.win32:+vt-int+ (val win32:int))
            (#.win32:+vt-uint+ (val win32:uint))
            (#.win32:+vt-int-ptr+ (val win32:int-ptr))
            (#.win32:+vt-uint-ptr+ (val win32:uint-ptr))
            (#.win32:+vt-void+ (values))
            (#.win32:+vt-hresult+ (val win32:hresult))
            (#.win32:+vt-ptr+ (val :pointer))
            (#.win32:+vt-safearray+ (val (:pointer safearray)))
            (#.win32:+vt-carray+ (error "Unsupported"))
            (#.win32:+vt-lpstr+ (cffi:foreign-string-to-lisp (val win32:lpstr) :encoding :ascii))
            (#.win32:+vt-lpwstr+ (cffi:foreign-string-to-lisp (val win32:lpwstr) :encoding win32:+win32-string-encoding+)))))))

(defun noteq (x y)
  (and (not (eq x y)) x))

(defun %print-types (type-lib &aux (first t))
  (do-types (type-info type-lib)
    (unless first (terpri))
    (setf first nil)
    (with-typeattr (typeattr type-info)
      (with-foreign-slots* (memid-constructor
                            memid-destructor cb-size-instance
                            typekind c-funcs c-vars
                            c-impl-types cb-alignment w-type-flags
                            (:pointer tdesc-alias))
                           typeattr
        ;; (unless (logtest typeflag-fhidden w-type-flags))
        (let ((name (get-type-info-name type-info)))
          (format t "~@[[~{~A~^, ~}]~%~]~
                     ~A (0x~8,'0X) [~A]~@
                     ~TAttributes:~@
                     ~T~Tctor:~T~A~@
                     ~T~Tdtor:~T~A~@
                     ~T~Tsize:~T~A~@
                     ~T~Talignment:~T~A~%"
                  (list-enum-flags w-type-flags 'typeflags)
                  name (cffi:pointer-address type-info) typekind
                  memid-constructor memid-destructor cb-size-instance cb-alignment))
        (when (> c-vars 0)
          (format t "~TVariables:~%")
          (do-vars (vardesc type-info)
            (with-foreign-slots* (memid (:pointer vardesc-union) elemdesc-var w-var-flags varkind)
                                 vardesc
              (let((type (elemdesc-to-type elemdesc-var type-info))
                   (name (i-type-info-get-name* type-info memid)))
                (format t "~T~T~A ~A" type name))
              (case varkind
                (var-perinstance)
                (var-static)
                (var-const (format t " = ~A" (%variant-to-value (cffi:mem-ref vardesc-union '(:pointer variant)))))
                (var-dispatch))
              (terpri))))

        (when (> c-funcs 0)
          (format t "~TFunctions:~%")
          (do-funcs (funcdesc type-info)
            (with-foreign-slots* (memid lprgscode callconv invkind lprgelemdesc-param elemdesc-func
                                  c-params w-func-flags)
                                 funcdesc
              (loop
                :with (name . param-names) := (i-type-info-get-names* type-info memid (1+ c-params))
                  :initially
                     (format t "~@[~T~T[~{~A~^, ~}]~%~]~
                                   ~T~T~@[~A ~]~A ~@[~A ~]~A (~:[~%~;~]"
                             (list-enum-flags w-func-flags 'funcflags)
                             (noteq invkind 'invoke-func)
                             (elemdesc-to-type elemdesc-func type-info)
                             (noteq callconv  'cc-stdcall)
                             name (zerop c-params))
                :for i :from 0 :below c-params
                :for param-name :in param-names
                :for elemdesc := (&* lprgelemdesc-param i)
                :for paramdesc := (cffi:foreign-slot-pointer elemdesc 'elemdesc 'elemdesc-union)
                :for flags := (cffi:foreign-slot-value paramdesc 'paramdesc 'wparamflags)
                :do (format t "~T~T~T[~{~A~^, ~}]~@
                               ~T~T~T~A ~:[rhs~;~:*~A~]~:[~;,~%~]"
                            (list-enum-flags flags 'paramflag)
                            (elemdesc-to-type elemdesc type-info)
                            param-name (/= (1+ i) c-params))
                :finally
                   (format t "~:[~%~T~T~; ~])~%" (zerop c-params))))))

        (when (> c-impl-types 0)
          (format t "~TImplemented types:~%")
          (do-impl-types (impl-info type-info)
            (with-typeattr (typeattr impl-info)
              (let ((name (get-type-info-name impl-info)))
                (format t "~T~T~A (0x~8,'0X) [~A]~%" name (cffi:pointer-address impl-info) (cffi:foreign-slot-value typeattr 'typeattr 'typekind))))))

        (when (eq typekind 'tkind-alias)
          (format t "~TAlias for: ~A~%" (typedesc-to-type tdesc-alias type-info))
          )))))

(defun print-types (type-lib)
  (etypecase type-lib
    (cffi:foreign-pointer (%print-types type-lib))
    ((or string pathname)
     (with-com-object (lib (load-type-lib* type-lib))
       (%print-types lib)))))

#+nil
(macroexpand-1 '(tlb-import "C:\\Users\\Zulu\\Documents\\Visual Studio 2015\\Projects\\ComClassThing\\bin\\Debug\\ComClassThing.tlb"))
#+nil
(CFFI:DEFCENUM (STUFF :INT32)
   (STUFF_GOOD 0)
   (STUFF_BAD 1))
#+nil
(DEFINE-COM-INTERFACE I-INTERFACE-FROM-UNKNOWN
    "{D3CE54A2-9C8D-4EA0-AB31-2A97970F469A}"
      (I-UNKNOWN)
  (STUFF-PROPERTY (:INVOKE-KIND :PROP-GET) WIN32:HRESULT
    (#:P-RET-VAL (:POINTER STUFF) :OUT :RETVAL))
  (STUFF-PROPERTY (:INVOKE-KIND :PROP-PUT) WIN32:HRESULT
    (#:P-RET-VAL STUFF :IN))

  (RETURN-STR-BY-OUT NIL WIN32:HRESULT (#:RET-STR (:POINTER WIN32:BSTR) :OUT))
  (RETURN-STR-BY-RET NIL WIN32:HRESULT
    (#:P-RET-VAL (:POINTER WIN32:BSTR) :OUT :RETVAL))
  (IN-OUT-INT-VAL NIL WIN32:HRESULT (#:X (:POINTER :INT32) :IN :OUT))
  (OPTIONAL-INT-VAL NIL WIN32:HRESULT (#:X :INT32 :IN :OPT :DEFAULT 5))
  (OPTIONAL-STRING-VAL NIL WIN32:HRESULT
    (#:STR WIN32:BSTR :IN :OPT :DEFAULT "")))