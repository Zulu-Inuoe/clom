(in-package #:clom)

#| Forward Declarations |#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %ftype-defined-p (type)
    (handler-case (progn (cffi:foreign-type-size type) t)
      (error () nil))))

(defmacro %fdecl (type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (%ftype-defined-p ',type)
       (cffi:defctype ,type :void))
     ',type))

(%fdecl i-create-type-info)
(%fdecl i-create-type-info-2)
(%fdecl i-create-type-lib)
(%fdecl i-create-type-lib-2)
(%fdecl i-dispatch)
(%fdecl i-enum-variant)
(%fdecl i-type-comp)
(%fdecl i-type-info)
(%fdecl i-type-info-2)
(%fdecl i-type-lib)
(%fdecl i-type-lib-2)
(%fdecl i-type-change-events)
(%fdecl i-error-info)
(%fdecl i-create-error-info)
(%fdecl i-support-error-info)
(%fdecl i-type-factory)
(%fdecl i-type-marshal)
(%fdecl i-record-info)
(%fdecl i-error-log)
(%fdecl i-property-bag)

(cffi:defctype currency win32:cy)

(cffi:defcstruct safearraybound
  (c-elements win32:ulong)
  (l-lbound win32:long))

(cffi:defctype safearraybound (:struct safearraybound))
(cffi:defctype lpsafearraybound (:pointer safearraybound))

#| the following is what MIDL knows how to remote |#
(%fdecl wire-variant)
(%fdecl wire-brecord)

(cffi:defcstruct safearr-bstr
  (size win32:ulong)
  (a-bstr (:pointer win32:wire-bstr)))

(cffi:defctype safearr-bstr (:struct safearr-bstr))

(cffi:defcstruct safearr-unknown
  (size win32:ulong)
  (ap-unknown (:pointer (:pointer i-unknown))))

(cffi:defctype safearr-unknown (:struct safearr-unknown))

(cffi:defcstruct safearr-dispatch
  (size win32:ulong)
  (ap-dispatch (:pointer (:pointer i-dispatch))))

(cffi:defctype safearr-dispatch (:struct safearr-dispatch))

(cffi:defcstruct safearr-variant
  (size win32:ulong)
  (a-variant (:pointer wire-variant)))

(cffi:defctype safearr-variant (:struct safearr-variant))

(cffi:defcstruct safearr-brecord
  (size win32:ulong)
  (a-record (:pointer wire-brecord)))

(cffi:defctype safearr-brecord (:struct safearr-brecord))

(cffi:defcstruct safearr-haveiid
  (Size win32:ulong)
  (ap-unknown (:pointer (:pointer i-unknown)))
  (iid win32:iid))

(cffi:defctype safearr-haveiid (:struct safearr-haveiid))

(cffi:defcenum sf-type
  (sf-error #.win32:+vt-error+)
  (sf-i1 #.win32:+vt-i1+)
  (sf-i2 #.win32:+vt-i2+)
  (sf-i4 #.win32:+vt-i4+)
  (sf-i8 #.win32:+vt-i8+)
  (sf-bstr #.win32:+vt-bstr+)
  (sf-unknown #.win32:+vt-unknown+)
  (sf-dispatch #.win32:+vt-dispatch+)
  (sf-variant #.win32:+vt-variant+)
  (sf-record #.win32:+vt-record+)
  (sf-haveiid #.(logior win32:+vt-unknown+ win32:+vt-reserved+)))

(cffi:defcunion safearrayunion-union
  (bstr-str safearr-bstr)
  (unknown-str safearr-unknown)
  (dispatch-str safearr-dispatch)
  (variant-str safearr-variant)
  (record-str safearr-brecord)
  (have-iid-str safearr-haveiid)
  (byte-str win32:byte-sizedarr)
  (word-str win32:word-sizedarr)
  (long-str win32:dword-sizedarr)
  (hyper-str win32:hyper-sizedarr))

(cffi:defctype safearrayunion-union (:union safearrayunion-union))

(cffi:defcstruct safearrayunion
  (sf-type win32:ulong)
  (u safearrayunion-union))

(cffi:defctype safearrayunion (:struct safearrayunion))

(cffi:defcstruct wire-safearray
  (c-dims win32:ushort)
  (f-features win32:ushort)
  (cb-elements win32:ulong)
  (c-locks win32:ulong)
  (u-array-structs safearrayunion)
  (rgsabound safearraybound :count 1))

(cffi:defctype wire-safearray (:struct wire-safearray))
(cffi:defctype wire-psafearray (:pointer wire-safearray))

(cffi:defcstruct safearray
  (c-dims win32:ushort)
  (f-features win32:ushort)
  (cb-elements win32:ulong)
  (c-locks win32:ulong)
  (pv-data win32:pvoid)
  (rgsabound safearraybound :count 1))

(cffi:defctype safearray (:struct safearray))
(cffi:defctype lpsafearray (:pointer safearray))

(defconstant fadf_auto		#x1)
(defconstant fadf_static	#x2)
(defconstant fadf_embedded	#x4)
(defconstant fadf_fixedsize	#x10)
(defconstant fadf_record	#x20)
(defconstant fadf_haveiid	#x40)
(defconstant fadf_havevartype	#x80)
(defconstant fadf_bstr		#x100)
(defconstant fadf_unknown	#x200)
(defconstant fadf_dispatch	#x400)
(defconstant fadf_variant	#x800)
(defconstant fadf_reserved	#xf008)

#| VARIANT STRUCTURE
 *
 *  VARTYPE vt;
 *  win32:word wReserved1;
 *  win32:word wReserved2;
 *  win32:word wReserved3;
 *  union {
 *    win32:longwin32:long       VT_I8
 *    win32:long           VT_I4
 *    win32:byte           VT_UI1
 *    win32:short          VT_I2
 *    win32:float          VT_R4
 *    win32:double         VT_R8
 *    win32:variant-bool   VT_BOOL
 *    win32:scode          VT_ERROR
 *    win32:cy             VT_CY
 *    win32:date           VT_DATE
 *    BSTR           VT_BSTR
 *    IUnknown *     VT_UNKNOWN
 *    IDispatch *    VT_DISPATCH
 *    SAFEARRAY *    VT_ARRAY
 *    win32:byte *         VT_BYREF|VT_UI1
 *    win32:short *        VT_BYREF|VT_I2
 *    win32:long *         VT_BYREF|VT_I4
 *    win32:longwin32:long *     VT_BYREF|VT_I8
 *    win32:float *        VT_BYREF|VT_R4
 *    win32:double *       VT_BYREF|VT_R8
 *    win32:variant-bool * VT_BYREF|VT_BOOL
 *    win32:scode *        VT_BYREF|VT_ERROR
 *    win32:cy *           VT_BYREF|VT_CY
 *    win32:date *         VT_BYREF|VT_DATE
 *    BSTR *         VT_BYREF|VT_BSTR
 *    IUnknown **    VT_BYREF|VT_UNKNOWN
 *    IDispatch **   VT_BYREF|VT_DISPATCH
 *    SAFEARRAY **   VT_BYREF|VT_ARRAY
 *    variant *      VT_BYREF|VT_VARIANT
 *    win32:pvoid          VT_BYREF (Generic ByRef)
 *    win32:char           VT_I1
 *    win32:ushort         VT_UI2
 *    win32:ulong          VT_UI4
 *    win32:ulonglong      VT_UI8
 *    win32:int            VT_INT
 *    win32:uint           VT_win32:uint
 *    win32:decimal *      VT_BYREF|VT_DECIMAL
 *    win32:char *         VT_BYREF|VT_I1
 *    win32:ushort *       VT_BYREF|VT_UI2
 *    win32:ulong *        VT_BYREF|VT_UI4
 *    win32:ulonglong *    VT_BYREF|VT_UI8
 *    win32:int *          VT_BYREF|VT_INT
 *    win32:uint *         VT_BYREF|VT_win32:uint
 *  }
 |#

(%fdecl variant)

(cffi:defcstruct brec-val
  (pv-record win32:pvoid)
  (rec-info (:pointer i-record-info)))

(cffi:defctype brec-val (:struct brec-val))

(cffi:defcunion variant-union
  (ll-val win32:longlong)
  (l-val win32:long)
  (b-val win32:byte)
  (i-val win32:short)
  (flt-val win32:float)
  (dbl-val win32:double)
  (bool-val win32:variant-bool)
  (bool win32:-variant-bool)
  (scode win32:scode)
  (cy-val win32:cy)
  (date win32:date)
  (bstr-val win32:bstr)
  (punk-val (:pointer i-unknown))
  (pdisp-val (:pointer i-dispatch))
  (parray (:pointer safearray))
  (pb-val (:pointer win32:byte))
  (pi-val (:pointer win32:short))
  (pl-val (:pointer win32:long))
  (pll-val (:pointer win32:longlong))
  (pflt-val (:pointer win32:float))
  (pdbl-val (:pointer win32:double))
  (pbool-val (:pointer win32:variant-bool))
  (pbool (:pointer win32:-variant-bool))
  (pscode (:pointer win32:scode))
  (pcy-val (:pointer win32:cy))
  (pdate (:pointer win32:date))
  (pbstr-val (:pointer win32:bstr))
  (ppunk-val (:pointer (:pointer i-unknown)))
  (ppdisp-val (:pointer (:pointer i-dispatch)))
  (pparray (:pointer (:pointer safearray)))
  (pvar-val (:pointer variant))
  (byref win32:pvoid)
  (c-val win32:char)
  (ui-val win32:ushort)
  (ul-val win32:ulong)
  (ull-val win32:ulonglong)
  (int-val win32:int)
  (uint-val win32:uint)
  (pdec-val (:pointer win32:decimal))
  (pc-val (:pointer win32:char))
  (pui-val (:pointer win32:ushort))
  (pul-val (:pointer win32:ulong))
  (pull-val (:pointer win32:ulonglong))
  (pint-val (:pointer win32:int))
  (puint-val (:pointer win32:uint))
  (brec-val brec-val))

(cffi:defctype variant-union (:union variant-union))

;; Note: Original definition includes variant being in a
;; union with `decimal', but I've found no documentation for that
;; usage, so I've collapsed the struct down for less insane usage
(cffi:defcstruct variant
  (vt win32:vartype)
  (w-reserved-1 win32:word)
  (w-reserved-2 win32:word)
  (w-reserved-3 win32:word)
  (variant-union variant-union))

(cffi:defctype variant (:struct variant))
(cffi:defctype lpvariant (:pointer variant))

(cffi:defctype variantarg variant)
(cffi:defctype lpvariantarg (:pointer variant))

(cffi:defctype refvariant  (:pointer variant))

#| the following is what MIDL knows how to remote |#
(cffi:defcstruct wire-brecord
  (f-flags win32:ulong)
  (cl-size win32:ulong)
  (p-rec-info (:pointer I-Record-info))
  (p-record (:pointer win32:byte)))

(cffi:defctype wire-brecord (:struct wire-brecord))

(cffi:defcunion wire-variant-union
  (ll-val win32:longlong)
  (l-val win32:long)
  (b-val win32:byte)
  (i-val win32:short)
  (flt-val win32:float)
  (dbl-val win32:double)
  (bool-val win32:variant-bool)
  (scode win32:scode)
  (cy-val win32:cy)
  (date win32:date)
  (bstr-val win32:wire-bSTR)
  (punk-val (:pointer I-Unknown))
  (pdisp-val (:pointer I-Dispatch))
  (parray wire-pSAFEARRAY)
  (brec-val wire-bRECORD)
  (pb-val (:pointer win32:byte))
  (pi-val (:pointer win32:short))
  (pl-val (:pointer win32:long))
  (pll-val (:pointer win32:longlong))
  (pflt-val (:pointer win32:float))
  (pdbl-val (:pointer win32:double))
  (pbool-val (:pointer win32:variant-bool))
  (pscode (:pointer win32:scode))
  (pcy-val (:pointer win32:cy))
  (pdate (:pointer win32:date))
  (pbstr-val (:pointer win32:wire-bSTR))
  (ppunk-val (:pointer (:pointer I-Unknown)))
  (ppdisp-val (:pointer (:pointer I-Dispatch)))
  (pparray (:pointer wire-pSAFEARRAY))
  (pvar-val (:pointer wire-vARIANT))
  (c-val win32:char)
  (ui-val win32:ushort)
  (ul-val win32:ulong)
  (ull-val win32:ulonglong)
  (int-val win32:int)
  (uint-val win32:uint)
  (dec-val win32:decimal)
  (pdec-val (:pointer win32:decimal))
  (pc-val (:pointer win32:char))
  (pui-val (:pointer win32:ushort))
  (pul-val (:pointer win32:ulong))
  (pull-val (:pointer win32:ulonglong))
  (pint-val (:pointer win32:int))
  (puint-val (:pointer win32:uint)))

(cffi:defctype wire-variant-union (:union wire-variant-union))

(cffi:defcstruct wire-variant
  (cl-size win32:dword)
  (rpc-reserved win32:dword)
  (vt win32:ushort)
  (w-reserved1 win32:ushort)
  (w-reserved2 win32:ushort)
  (w-reserved3 win32:ushort)
  (wire-variant-union wire-variant-union))

(cffi:defctype wire-variant (:struct wire-variant))

(cffi:defctype dispid win32:long)
(cffi:defctype memberid dispid)
(cffi:defctype hreftype win32:dword)

(cffi:defcenum typekind
  tkind-enum
  tkind-record
  tkind-module
  tkind-interface
  tkind-dispatch
  tkind-coclass
  tkind-alias
  tkind-union
  tkind-max)

(%fdecl typedesc)
(%fdecl arraydesc)

(cffi:defcunion typedesc-union
  (lptdesc (:pointer typedesc))
  (lpadesc (:pointer arraydesc))
  (hreftype hreftype))

(cffi:defctype typedesc-union (:union typedesc-union))

(cffi:defcstruct typedesc
  (typedesc-union typedesc-union)
  (vt win32:vartype))

(cffi:defctype typedesc (:struct typedesc))

(cffi:defcstruct arraydesc
  (tdesk-elem typedesc)
  (c-dims win32:ushort)
  (rgbounds safearraybound :count 1))

(cffi:defctype arraydesc (:struct arraydesc))

(cffi:defcstruct paramdescex
  (c-bytes win32:ulong)
  (var-default-value variantarg))

(cffi:defctype paramdescex (:struct paramdescex))
(cffi:defctype lpparamdescex (:pointer paramdescex))

(cffi:defcstruct paramdesc
  (pparamdescex lpparamdescex)
  (wparamflags win32:ushort))

(cffi:defctype paramdesc (:struct paramdesc))
(cffi:defctype lpparamdesc (:pointer paramdesc))

(cffi:defcenum paramflag
  (paramflag-none 0)
  (paramflag-fin #x1)
  (paramflag-fout #x2)
  (paramflag-flcid #x4)
  (paramflag-fretval #x8)
  (paramflag-fopt #x10)
  (paramflag-fhasdefault #x20)
  (paramflag-fhascustdata #x40))

(cffi:defcstruct idldesc
  (dw-reserved win32:ulong-ptr)
  (w-idl-flags win32:ulong))

(cffi:defctype idldesc (:struct idldesc))
(cffi:defctype lpidldesc (:pointer idldesc))

(defconstant idlflag-none paramflag-none)
(defconstant idlflag-fin paramflag-fin)
(defconstant idlflag-fout paramflag-fout)
(defconstant idlflag-flcid paramflag-flcid)
(defconstant idlflag-fretval paramflag-fretval)

(cffi:defcunion elemdesc-union
  (idldesc idldesc)
  (paramdesc paramdesc))

(cffi:defctype elemdesc-union (:union elemdesc-union))

(cffi:defcstruct elemdesc
  (tdesc typedesc)
  (elemdesc-union elemdesc-union))

(cffi:defctype elemdesc (:struct elemdesc))
(cffi:defctype lpelemdesc (:pointer elemdesc))

(cffi:defcstruct typeattr
  (guid win32:guid)
  (lcid win32:lcid)
  (dw-reserved win32:dword)
  (memid-constructor memberid)
  (memid-destructor memberid)
  (lpstr-schema win32:lpolestr)
  (cb-size-instance win32:ulong)
  (typekind typekind)
  (c-funcs win32:word)
  (c-vars win32:word)
  (c-impl-types win32:word)
  (cb-size-vft win32:word)
  (cb-alignment win32:word)
  (w-type-flags win32:word)
  (w-major-ver-num win32:word)
  (w-minor-ver-num win32:word)
  (tdesc-alias typedesc)
  (idldesc-type idldesc))

(cffi:defctype typeattr (:struct typeattr))
(cffi:defctype lptypeattr (:pointer typeattr))

(cffi:defcstruct dispparams
  (rgvarg (:pointer variantarg))
  (rgdispid-named-args (:pointer dispid))
  (c-args win32:uint)
  (c-named-args win32:uint))

(cffi:defctype dispparams (:struct dispparams))

(cffi:defcstruct excepinfo
  (w-code  win32:word)
  (w-reserved  win32:word)
  (bstr-source  win32:bstr)
  (bstr-description  win32:bstr)
  (bstr-help-file  win32:bstr)
  (dw-help-context win32:dword)
  (pv-reserved win32:pvoid)
  (pfn-deferred-fill-in :pointer)
  (scode win32:scode))

(cffi:defctype excepinfo (:struct excepinfo))
(cffi:defctype lpexcepinfo (:pointer excepinfo))

(cffi:defcenum callconv
  (cc-fastcall 0)
  (cc-cdecl 1)
  (cc-mscpascal 2)
  (cc-pascal 2)
  (cc-macpascal 3)
  (cc-stdcall 4)
  (cc-fpfastcall 5)
  (cc-syscall 6)
  (cc-mpwcdecl 7)
  (cc-mpwpascal 8)
  (cc-max 9))

(cffi:defcenum funckind
  (func-virtual 0)
  (func-purevirtual 1)
  (func-nonvirtual 2)
  (func-static 3)
  (func-dispatch 4))

(cffi:defcenum invokekind
  (invoke-func 1)
  (invoke-propertyget 2)
  (invoke-propertyput 4)
  (invoke-propertyputref 8))

(cffi:defcstruct funcdesc
  (memid memberid)
  (lprgscode (:pointer win32:scode))
  (lprgelemdesc-param (:pointer elemdesc))
  (funckind funckind)
  (invkind invokekind)
  (callconv callconv)
  (c-params win32:short)
  (c-params-opt win32:short)
  (o-vft win32:short)
  (c-scodes win32:short)
  (elemdesc-func elemdesc)
  (w-func-flags win32:word))

(cffi:defctype funcdesc (:struct funcdesc))
(cffi:defctype lpfuncdesc (:pointer funcdesc))

(cffi:defcenum varkind
  var-perinstance
  var-static
  var-const
  var-dispatch);

(defconstant impltypeflag-fdefault		#x1)
(defconstant impltypeflag-fsource		#x2)
(defconstant impltypeflag-frestricted		#x4)
(defconstant impltypeflag-fdefaultvtable	#x8)

(cffi:defcunion vardesc-union
  (o-inst win32:ulong)
  (lpvar-value (:pointer variant)))

(cffi:defctype vardesc-union (:union vardesc-union))

(cffi:defcstruct vardesc
  (memid memberid)
  (lpstr-schema win32:lpolestr)
  (vardesc-union vardesc-union)
  (elemdesc-var elemdesc)
  (w-var-flags win32:word)
  (varkind varkind))

(cffi:defctype vardesc (:struct vardesc))
(cffi:defctype lpvardesc (:pointer vardesc))

(cffi:defcenum typeflags
  (typeflag-fappobject		#x1)
  (typeflag-fcancreate		#x2)
  (typeflag-flicensed		#x4)
  (typeflag-fpredeclid		#x8)
  (typeflag-fhidden		#x10)
  (typeflag-fcontrol		#x20)
  (typeflag-fdual		#x40)
  (typeflag-fnonextensible	#x80)
  (typeflag-foleautomation	#x100)
  (typeflag-frestricted	#x200)
  (typeflag-faggregatable	#x400)
  (typeflag-freplaceable	#x800)
  (typeflag-fdispatchable	#x1000)
  (typeflag-freversebind	#x2000)
  (typeflag-fproxy		#x4000))


(cffi:defcenum funcflags
  (funcflag-frestricted	#x1)
  (funcflag-fsource		#x2)
  (funcflag-fbindable		#x4)
  (funcflag-frequestedit	#x8)
  (funcflag-fdisplaybind	#x10)
  (funcflag-fdefaultbind	#x20)
  (funcflag-fhidden		#x40)
  (funcflag-fusesgetlasterror	#x80)
  (funcflag-fdefaultcollelem	#x100)
  (funcflag-fuidefault		#x200)
  (funcflag-fnonbrowsable	#x400)
  (funcflag-freplaceable	#x800)
  (funcflag-fimmediatebind	#x1000))

(cffi:defcenum varflags
  (varflag-freadonly		#x1)
  (varflag-fsource		#x2)
  (varflag-fbindable		#x4)
  (varflag-frequestedit	#x8)
  (varflag-fdisplaybind	#x10)
  (varflag-fdefaultbind	#x20)
  (varflag-fhidden		#x40)
  (varflag-frestricted		#x80)
  (varflag-fdefaultcollelem	#x100)
  (varflag-fuidefault		#x200)
  (varflag-fnonbrowsable	#x400)
  (varflag-freplaceable	#x800)
  (varflag-fimmediatebind	#x1000))

(cffi:defcstruct cleanlocalstorage
  (pinterface (:pointer i-unknown))
  (pstorage win32:pvoid)
  (flags win32:dword))

(cffi:defctype cleanlocalstorage (:struct cleanlocalstorage))

(cffi:defcstruct custdataitem
  (guid win32:guid)
  (var-value variantarg))

(cffi:defctype custdataitem (:struct custdataitem))
(cffi:defctype lpcustdataitem (:pointer custdataitem))

(cffi:defcstruct custdata
  (c-cust-data win32:dword)
  (prg-cust-data lpcustdataitem))

(cffi:defctype custdata (:struct custdata))
(cffi:defctype lpcustdata (:pointer custdata))

(cffi:defctype lpcreatetypeinfo #| [unique] |# (:pointer i-create-type-info))

;;TODO
;; EXTERN_C const IID IID_ICreateTypeInfo;

(define-com-interface i-create-type-info "{00020405-0000-0000-C000-000000000046}" (i-unknown)
  (set-guid () win32:hresult
    (guid win32:refguid :in))

  (set-type-flags () win32:hresult
    (u-type-flags win32:uint :in))

  (set-doc-string () win32:hresult
    (#|__rpc__in|#  p-str-doc win32:lpolestr :in))

  (set-help-context () win32:hresult
    (dw-help-context win32:dword :in))

  (set-version () win32:hresult
    (w-major-ver-num win32:word :in)
    (w-minor-ver-num win32:word :in))

  (add-ref-type-info () win32:hresult
    (p-tinfo (:pointer i-type-info) :in)
    (ph-ref-type (:pointer hreftype) :in))

  (add-func-desc () win32:hresult
    (index win32:uint :in)
    (p-func-desc (:pointer funcdesc) :in))

  (add-impl-type () win32:hresult
    (index win32:uint :in)
    (h-ref-type hreftype :in))

  (set-impl-type-flags () win32:hresult
    (index win32:uint :in)
    (impl-type-flags win32:int :in))

  (set-alignment () win32:hresult
    (cb-alignment win32:word :in))

  (set-schema () win32:hresult
    (#|__rpc__in|#  p-str-schema win32:lpolestr :in))

  (add-var-desc () win32:hresult
    (index win32:uint :in)
    (p-var-desc (:pointer vardesc) :in))

  (set-func-and-param-names () win32:hresult
    (index win32:uint :in)
    (#| [annotation][in][size_is] |#
     #|__rpc__in_ecount(cnames)|#
     rgsz-names (:pointer win32:lpolestr) :in)
    (c-names win32:uint :in))

  (set-var-name () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#  sz-name win32:lpolestr :in))

  (set-type-desc-alias () win32:hresult
    (p-tdesc-alias (:pointer typedesc) :in))

  (define-func-as-dll-entry () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     sz-dll-name win32:lpolestr :in)
    (#|__rpc__in|#
     sz-proc-name win32:lpolestr :in))

  (set-func-doc-string () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     sz-doc-string win32:lpolestr :in))

  (set-var-doc-string () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     sz-doc-string win32:lpolestr :in))

  (set-func-help-context () win32:hresult
    (index win32:uint :in)
    (dw-help-context win32:dword :in))

  (set-var-help-context () win32:hresult
    (index win32:uint :in)
    (dw-help-context win32:dword :in))

  (set-mops () win32:hresult
    (index win32:uint :in)
    (bstr-mops win32:bstr :in))

  (set-type-idldesc () win32:hresult
    (p-idl-desc (:pointer idldesc) :in))

  (lay-out () win32:hresult))

(cffi:defctype lpcreatetypeinfo2 #| [unique] |# (:pointer i-create-type-info-2))

;;TODO
;; EXTERN_C const IID IID_ICreateTypeInfo2;

(define-com-interface i-create-type-info-2 "{0002040e-0000-0000-c000-000000000046}" (i-create-type-info)
  (delete-func-desc () win32:hresult
    (index win32:uint :in))

  (delete-func-desc-by-mem-id () win32:hresult
    (memid memberid :in)
    (inv-kind invokekind :in))

  (delete-var-desc () win32:hresult
    (index win32:uint :in))

  (delete-var-desc-by-mem-id () win32:hresult
    (memid memberid :in))

  (delete-impl-type () win32:hresult
    (index win32:uint :in))

  (set-cust-data () win32:hresult
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-func-cust-data () win32:hresult
    (index win32:uint :in)
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-param-cust-data () win32:hresult
    (index-func win32:uint :in)
    (index-param win32:uint :in)
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-var-cust-data () win32:hresult
    (index win32:uint :in)
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-impl-type-cust-data () win32:hresult
    (index win32:uint :in)
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-help-string-context () win32:hresult
    (dw-help-string-context win32:ulong :in))

  (set-func-help-string-context () win32:hresult
    (index win32:uint :in)
    (dw-help-string-context win32:ulong :in))

  (set-var-help-string-context () win32:hresult
    (index win32:uint :in)
    (dw-help-string-context win32:ulong :in))

  (invalidate () win32:hresult)

  (set-name () win32:hresult
    (#|__rpc__in|#
     sz-name win32:lpolestr :in)))

(cffi:defctype lpcreatetypelib (:pointer #| [unique] |# i-create-type-lib))

;;TODO
;; EXTERN_C const IID IID_ICreateTypeLib;

(define-com-interface i-Create-Type-Lib "{00020406-0000-0000-C000-000000000046}" (i-unknown)
  (create-type-info () win32:hresult
    (#|__rpc__in|#
     sz-name win32:lpolestr :in)
    (tkind typekind :in)
    (pp-ctinfo (:pointer (:pointer i-create-type-info)) :out))

  (set-name () win32:hresult
    (#|__rpc__in|#
     sz-name win32:lpolestr :in))

  (set-version () win32:hresult
    (w-major-ver-num win32:word :in)
    (w-minor-ver-num win32:word :in))

  (set-guid () win32:hresult
    (guid win32:refguid :in))

  (set-doc-string () win32:hresult
    (#|__rpc__in|#
     sz-doc win32:lpolestr :in))

  (set-help-file-name () win32:hresult
    ( #|__rpc__in|#
     sz-help-file-name win32:lpolestr :in))

  (set-help-context () win32:hresult
    (dw-help-context win32:dword :in))

  (set-lcid () win32:hresult
    (lcid win32:lcid :in))

  (set-lib-flags () win32:hresult
    (u-lib-flags win32:uint :in))

  (save-all-changes () win32:hresult))


(cffi:defctype lpcreatetypelib2 (:pointer #| [unique] |# i-create-type-lib-2))

;;TODO
;; EXTERN_C const IID IID_ICreateTypeLib2;

(define-com-interface i-create-type-lib-2 "{0002040f-0000-0000-c000-000000000046}" (i-create-type-lib)
  (delete-type-info () win32:hresult
    (#|__rpc__in|#
     sz-name win32:lpolestr :in))

  (set-cust-data () win32:hresult
    (guid win32:refguid :in)
    (p-var-val (:pointer variant) :in))

  (set-help-string-context () win32:hresult
    (dw-help-string-context win32:ulong :in))

  (set-help-string-dll () win32:hresult
    (#|__rpc__in|#
     sz-file-name win32:lpolestr :in)))

(cffi:defctype lpdispatch (:pointer #| [unique] |#  #|__rpc_unique_pointer|# i-dispatch))

#| dispid reserved to indicate an "unknown" name |#
#| only reserved for data members (properties); reused as a method dispid below |#
(defconstant	dispid-unknown	-1)

#| DISPID reserved for the "value" property |#
(defconstant	dispid-value	0)

#| The following DISPID is reserved to indicate the param
 * that is the right-hand-side (or "put" value) of a PropertyPut
 |#
(defconstant	dispid-propertyput	-3)

#| DISPID reserved for the standard "NewEnum" method |#
(defconstant	dispid-newenum	-4)

#| DISPID reserved for the standard "Evaluate" method |#
(defconstant	dispid-evaluate	-5)

(defconstant	dispid-constructor	-6)

(defconstant	dispid-destructor	-7)

(defconstant	dispid-collect	-8)

#| The range -500 through -999 is reserved for Controls |#
#| The range #x80010000 through #x8001FFFF is reserved for Controls |#
#| The range -5000 through -5499 is reserved for ActiveX Accessability |#
#| The range -2000 through -2499 is reserved for VB5 |#
#| The range -3900 through -3999 is reserved for Forms |#
#| The range -5500 through -5550 is reserved for Forms |#
#| The remainder of the negative DISPIDs are reserved for future use |#

;;TODO
;; EXTERN_C const IID IID_IDispatch;

(define-com-interface i-dispatch "{00020400-0000-0000-c000-000000000046}" (i-unknown)
  (get-type-info-count () win32:hresult
    (#|__rpc__out|#
     pctinfo (:pointer win32:uint) :out))

  (get-type-info () win32:hresult
    (i-tinfo win32:uint :in)
    (lcid win32:lcid :in)
    (#|__rpc__deref_out_opt|#
     pp-tinfo (:pointer (:pointer i-type-info)) :out))

  (get-ids-of-names () win32:hresult
    (#|__rpc__in|#
     riid win32:refiid :in)
    (#| [size_is][in] |# #|__rpc__in_ecount_full(cnames)|#
     rgsz-names (:pointer win32:lpolestr))
    (#| [range][in] |# #|__rpc__in_range(016384)|#
     c-names win32:uint)
    (lcid win32:lcid :in)
    (#| [size_is][out] |# #|__rpc__out_ecount_full(c-names)|#
     rg-disp-id (:pointer dispid) :in))

  (invoke () win32:hresult
    (disp-id-member dispid :in)
    (riid win32:refiid :in)
    (lcid win32:lcid :in)
    (w-flags win32:word :in)
    (p-disp-params (:pointer dispparams) :in :out)
    (p-var-result (:pointer variant) :out)
    (p-excep-info (:pointer excepinfo) :out)
    (pu-arg-err (:pointer win32:uint) :out)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE IDispatch_Remote-Invoke_Proxy(
    #|__RPC__in|# i-dispatch * This,
    #| [in] |# dispid disp-Id-Member,
    #| [in] |# #|__RPC__in|# win32:refiid riid,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword dw-Flags,
    #| [in] |# #|__RPC__in|# DISPPARAMS *p-Disp-Params,
    #| [out] |# #|__RPC__out|# variant *p-Var-Result,
    #| [out] |# #|__RPC__out|# EXCEPINFO *p-Excep-Info,
    #| [out] |# #|__RPC__out|# win32:uint *p-Arg-Err,
    #| [in] |# win32:uint c-Var-Ref,
    #| [size_is][in] |# #|__RPC__in_ecount_full(c-Var-Ref)|# win32:uint *rg-Var-Ref-Idx,
    #| [size_is][out][in] |# #|__RPC__inout_ecount_full(c-Var-Ref)|# variantarg *rg-Var-Ref);


void #|__RPC_STUB|# IDispatch_Remote-Invoke_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#

(cffi:defctype lpenumvariant #| [unique] |#  #|__rpc_unique_pointer|# (:pointer i-enum-variant))

;;TODO
;; EXTERN_C const IID IID_IEnum-VARIANT;

(define-com-interface i-enum-variant "{00020404-0000-0000-C000-000000000046}" (i-unknown)
  (next () win32:hresult
    (celt win32:ulong :in)
    (#| [length_is][size_is][out] |#
     rg-var (:pointer variant))
    (p-celt-fetched (:pointer win32:ulong) :out))

  (skip () win32:hresult
    (celt win32:ulong :in))

  (reset () win32:hresult)

  (clone () win32:hresult
    (#|__rpc__deref_out_opt|#
     pp-enum (:pointer (:pointer i-enum-variant)) :out)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE IEnumVARIANT_Remote-Next_Proxy(
    #|__RPC__in|# i-Enum-VARIANT * This,
    #| [in] |# win32:ulong celt,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(celt, *p-Celt-Fetched)|# variant *rg-Var,
    #| [out] |# #|__RPC__out|# win32:ulong *p-Celt-Fetched);


void #|__RPC_STUB|# IEnum-VARIANT_Remote-Next_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#

(cffi:defctype lptypecomp #| [unique] |#  #|__rpc_unique_pointer|# (:pointer i-type-comp))

(cffi:defcenum desckind
  (desckind-none 0)
  (desckind-funcdesc 1)
  (desckind-vardesc 2)
  (desckind-typecomp 3)
  (desckind-implicitappobj 4)
  (desckind-max 5))

(cffi:defcunion bindptr
  (lpfuncdesc (:pointer funcdesc))
  (lpvardesc (:pointer vardesc))
  (lptcomp (:pointer i-type-comp)))

(cffi:defctype bindptr (:union bindptr))
(cffi:defctype lpbindptr (:pointer bindptr))

;;TODO
;; EXTERN_C const IID IID_IType-Comp;

(define-com-interface i-type-comp "{00020403-0000-0000-C000-000000000046}" (i-unknown)
  (bind () win32:hresult
    (#|__rpc__in|#
     sz-name win32:lpolestr :in)
    (l-hash-val win32:ulong :in)
    (w-flags win32:word :in)
    (pp-tinfo (:pointer (:pointer i-type-info)) :out)
    (p-desc-kind (:pointer desckind) :out)
    (p-bind-ptr (:pointer bindptr) :out))
  (bind-type () win32:hresult
    (#|__rpc__in|#  sz-name win32:lpolestr :in)
    (l-hash-val win32:ulong :in)
    (pp-tinfo (:pointer (:pointer i-type-info)) :out)
    (pp-tcomp (:pointer (:pointer i-type-comp)) :out)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeComp_Remote-Bind_Proxy(
    #|__RPC__in|# i-Type-Comp * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr sz-Name,
    #| [in] |# win32:ulong l-Hash-Val,
    #| [in] |# win32:word w-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-info **pp-TInfo,
    #| [out] |# #|__RPC__out|# DESCKIND *p-Desc-Kind,
    #| [out] |# #|__RPC__deref_out_opt|# LPFUNCDESC *pp-Func-Desc,
    #| [out] |# #|__RPC__deref_out_opt|# LPVARDESC *pp-Var-Desc,
    #| [out] |# #|__RPC__deref_out_opt|# i-Type-Comp **pp-Type-Comp,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *p-Dummy);


void #|__RPC_STUB|# IType-Comp_Remote-Bind_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeComp_Remote-Bind-Type_Proxy(
    #|__RPC__in|# i-Type-Comp * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr sz-Name,
    #| [in] |# win32:ulong l-Hash-Val,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-info **pp-TInfo);


void #|__RPC_STUB|# ITypeComp_Remote-Bind-Type_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#

(cffi:defctype LPTYPEINFO #| [unique] |#  #|__RPC_unique_pointer|# (:pointer i-type-info))

;;TODO
;; EXTERN_C const IID IID_i-type-info;

(define-com-interface i-type-info "{00020401-0000-0000-C000-000000000046}" (i-unknown)
  (get-type-attr () win32:hresult
    (pp-type-attr (:pointer (:pointer typeattr)) :out))

  (get-type-comp () win32:hresult
    (#|__rpc__deref_out_opt|#
     pp-tcomp (:pointer (:pointer i-type-comp)) :out))

  (get-func-desc () win32:hresult
    (index win32:uint :in)
    (pp-func-desc (:pointer (:pointer funcdesc)) :out))

  (get-var-desc () win32:hresult
    (index win32:uint :in)
    (pp-var-desc (:pointer (:pointer vardesc)) :out))

  (get-names () win32:hresult
    (memid memberid :in)
    (#| [length_is][size_is][out] |#
     rg-bstr-names (:pointer win32:bstr))
    (c-max-names win32:uint :in)
    (pc-names (:pointer win32:uint) :out))

  (get-ref-type-of-impl-type () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-ref-type (:pointer hreftype) :out))

  (get-impl-type-flags () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-impl-type-flags (:pointer win32:int) :out))

  (get-ids-of-names () win32:hresult
    (#| [annotation][size_is][in] |#
     #|__rpc__in_ecount(c-names)|#
     rgsz-names (:pointer win32:lpolestr))
    (c-names win32:uint :in)
    (#| [size_is][out] |#
     p-mem-id (:pointer memberid)))

  (invoke () win32:hresult
    (pv-instance win32:pvoid :in)
    (memid memberid :in)
    (w-flags win32:word :in)
    (p-disp-params (:pointer dispparams) :in :out)
    (p-var-result (:pointer variant) :out)
    (p-excep-info (:pointer excepinfo) :out)
    (pu-arg-err (:pointer win32:uint) :out))

  (get-documentation () win32:hresult
    (memid memberid :in)
    (p-bstr-name (:pointer win32:bstr) :out)
    (p-bstr-doc-string (:pointer win32:bstr) :out)
    (pdw-help-context (:pointer win32:dword) :out)
    (p-bstr-help-file (:pointer win32:bstr) :out))

  (get-dll-entry () win32:hresult
    (memid memberid :in)
    (inv-kind invokekind :in)
    (p-bstr-dll-name (:pointer win32:bstr) :out)
    (p-bstr-name (:pointer win32:bstr) :out)
    (pw-ordinal (:pointer win32:word) :out))

  (get-ref-type-info () win32:hresult
    (h-ref-type hreftype :in)
    (#|__rpc__deref_out_opt|#
     pp-tinfo (:pointer (:pointer i-type-info)) :out))

  (address-of-member () win32:hresult
    (memid memberid :in)
    (inv-kind invokekind :in)
    (ppv (:pointer win32:pvoid) :out))

  (create-instance () win32:hresult
    (p-unk-outer (:pointer i-unknown) :in)
    (riid win32:refiid :in)
    (#| [iid_is] |#
     ppv-obj (:pointer win32:pvoid) :out))

  (get-mops () win32:hresult
    (memid memberid :in)
    (#|__rpc__deref_out_opt|#
     p-bstr-mops (:pointer win32:bstr) :out))

  (get-containing-type-lib () win32:hresult
    (pp-tlib (:pointer (:pointer i-type-lib)) :out)
    (p-index (:pointer win32:uint) :out))

  (release-type-attr () :void
    (p-type-attr (:pointer typeattr) :in))

  (release-func-desc () :void
    (p-func-desc (:pointer funcdesc) :in))

  (release-var-desc () :void
    (p-var-desc (:pointer vardesc) :in)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE IType-Info_Remote-Get-Type-Attr_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [out] |# #|__RPC__deref_out_opt|# LPTYPEATTR *pp-Type-Attr,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *p-Dummy);


void #|__RPC_STUB|# IType-Info_Remote-Get-Type-Attr_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_Remote-Get-Func-Desc_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# #|__RPC__deref_out_opt|# LPFUNCDESC *pp-Func-Desc,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *p-Dummy);


void #|__RPC_STUB|# IType-Info_Remote-Get-Func-Desc_Stub(
    i-Rpc-Stub-Buffer *This,
    i-Rpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE IType-Info_Remote-Get-Var-Desc_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# #|__RPC__deref_out_opt|# LPVARDESC *pp-Var-Desc,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *p-Dummy);


void #|__RPC_STUB|# ITypeInfo_Remote-Get-Var-Desc_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE IType-Info_Remote-Get-Names_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(c-Max-Names, *pc-Names)|# win32:bstr *rg-Bstr-Names,
    #| [in] |# win32:uint c-Max-Names,
    #| [out] |# #|__RPC__out|# win32:uint *pc-Names);


void #|__RPC_STUB|# IType-Info_Remote-Get-Names_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE IType-Info_Local-Get-IDs-Of-Names_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalGetIDsOfNames_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_LocalInvoke_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalInvoke_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_RemoteGetDocumentation_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:dword ref-Ptr-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Name,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Doc-String,
    #| [out] |# #|__RPC__out|# win32:dword *pdw-Help-Context,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Help-File);


void #|__RPC_STUB|# ITypeInfo_RemoteGetDocumentation_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_RemoteGetDllEntry_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# INVOKEKIND inv-Kind,
    #| [in] |# win32:dword ref-Ptr-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Dll-Name,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Name,
    #| [out] |# #|__RPC__out|# win32:word *pw-Ordinal);


void #|__RPC_STUB|# ITypeInfo_RemoteGetDllEntry_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_LocalAddressOfMember_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalAddressOfMember_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_RemoteCreateInstance_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [in] |# #|__RPC__in|# win32:refiid riid,
    #| [iid_is][out] |# #|__RPC__deref_out_opt|# i-unknown **ppv-Obj);


void #|__RPC_STUB|# ITypeInfo_RemoteCreateInstance_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_RemoteGetContainingTypeLib_Proxy(
    #|__RPC__in|# i-type-info * This,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-lib **pp-TLib,
    #| [out] |# #|__RPC__out|# win32:uint *p-Index);


void #|__RPC_STUB|# ITypeInfo_RemoteGetContainingTypeLib_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_LocalReleaseTypeAttr_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalReleaseTypeAttr_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_LocalReleaseFuncDesc_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalReleaseFuncDesc_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_LocalReleaseVarDesc_Proxy(
    #|__RPC__in|# i-type-info * This);


void #|__RPC_STUB|# ITypeInfo_LocalReleaseVarDesc_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#

(cffi:defctype lptypeinfo-2 #| [unique] |#  #|__rpc_unique_pointer|# (:pointer i-type-info-2))

;;TODO
;; EXTERN_C const IID IID_ITypeInfo2;

(define-com-interface i-type-info-2 "{00020412-0000-0000-c000-000000000046}" (i-type-info)
  (get-type-kind () win32:hresult
    (#|__rpc__out|#
     p-type-kind (:pointer typekind) :out))

  (get-type-flags () win32:hresult
    (#|__rpc__out|#
     p-type-flags (:pointer win32:ulong) :out))

  (get-func-index-of-mem-id () win32:hresult
    (memid memberid :in)
    (inv-kind invokekind :in)
    (#|__rpc__out|#
     p-func-index (:pointer win32:uint) :out))

  (get-var-index-of-mem-id () win32:hresult
    (memid memberid :in)
    (#|__rpc__out|#
     p-var-index (:pointer win32:uint) :out))

  (get-cust-data () win32:hresult
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__out|#
     p-var-val (:pointer variant) :out))

  (get-func-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__out|#
     p-var-val (:pointer variant) :out))

  (get-param-cust-data () win32:hresult
    (index-func win32:uint :in)
    (index-param win32:uint :in)
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__out|#
     p-var-val (:pointer variant) :out))

  (get-var-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__out|#
     p-var-val (:pointer variant) :out))

  (get-impl-type-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__out|#
     p-var-val (:pointer variant) :out))

  (get-documentation2 () win32:hresult
    (memid memberid :in)
    (lcid win32:lcid :in)
    (pbstr-help-string (:pointer win32:bstr) :out)
    (pdw-help-string-context (:pointer win32:dword) :out)
    (pbstr-help-string-dll (:pointer win32:bstr) :out))

  (get-all-cust-data () win32:hresult
    (#|__rpc__out|#
     p-cust-data (:pointer custdata) :out))

  (get-all-func-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-cust-data (:pointer custdata) :out))

  (get-all-param-cust-data () win32:hresult
    (index-func win32:uint :in)
    (index-param win32:uint :in)
    (#|__rpc__out|#
     p-cust-data (:pointer custdata) :out))

  (get-all-var-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-cust-data (:pointer custdata) :out))

  (get-all-impl-type-cust-data () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-cust-data (:pointer custdata) :out)))
#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo2_RemoteGetDocumentation2_Proxy(
    #|__RPC__in|# i-type-info-2 * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword ref-Ptr-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstr-Help-String,
    #| [out] |# #|__RPC__out|# win32:dword *pdw-Help-String-Context,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstr-Help-String-Dll);

void #|__RPC_STUB|# ITypeInfo2_RemoteGetDocumentation2_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#


#| interface IType-Lib |#
#| [unique][uuid][object] |#
(cffi:defcenum syskind
  (sys-win16 0)
  (sys-win32 1)
  (sys-mac 2)
  (sys-win64 3))

#| [v1_enum] |#
(cffi:defcenum libflags
  (libflag-frestricted #x1)
  (libflag-fcontrol #x2)
  (libflag-fhidden #x4)
  (libflag-fhasdiskimage #x8))

(cffi:defctype lptypelib #| [unique] |#  #|__RPC_unique_pointer|# (:pointer i-type-lib))

(cffi:defcstruct tlibattr
  (guid win32:guid)
  (lcid win32:lcid)
  (syskind syskind)
  (w-major-ver-num win32:word)
  (w-minor-ver-num win32:word)
  (w-lib-flags win32:word))

(cffi:defctype tlibattr (:struct  tlibattr))
(cffi:defctype lptlibattr (:pointer tlibattr))

;;TODO
;; EXTERN_C const IID IID_ITypeLib;

(define-com-interface i-type-lib "{00020402-0000-0000-C000-000000000046}" (i-unknown)
  (get-type-info-count () win32:uint)

  (get-type-info () win32:hresult
    (index win32:uint :in)
    (#|__rpc__deref_out_opt|#
     pp-tinfo (:pointer (:pointer i-type-info)) :out))

  (get-type-info-type () win32:hresult
    (index win32:uint :in)
    (#|__rpc__out|#
     p-tkind (:pointer typekind) :out))

  (get-type-info-of-guid () win32:hresult
    (#|__rpc__in|#
     guid win32:refguid :in)
    (#|__rpc__deref_out_opt|#
     pp-tinfo (:pointer (:pointer i-type-info)) :out))

  (get-lib-attr () win32:hresult
    (pp-tlib-attr (:pointer (:pointer tlibattr)) :out))

  (get-type-comp () win32:hresult
    (#|__rpc__deref_out_opt|#
     pp-tcomp (:pointer (:pointer i-type-comp)) :out))

  (get-documentation () win32:hresult
    (index win32:int :in)
    (p-bstr-name (:pointer win32:bstr) :out)
    (p-bstr-doc-string (:pointer win32:bstr) :out)
    (pdw-help-context (:pointer win32:dword) :out)
    (p-bstr-help-file (:pointer win32:bstr) :out))

  (is-name () win32:hresult
    (#|__rpc__inout|#
     sz-name-buf win32:lpolestr :in :out)
    (l-hash-val win32:ulong :in)
    (pf-name (:pointer win32:bool) :out))

  (find-name () win32:hresult
    (#|__rpc__inout|#
     sz-name-buf win32:lpolestr :in :out)
    (l-hash-val win32:ulong :in)
    (#| [length_is][size_is][out] |#
     pp-tinfo (:pointer (:pointer i-type-info)))
    (#| [length_is][size_is][out] |#
     rg-mem-id (:pointer memberid))
    (pc-found (:pointer win32:ushort) :in :out))

  (release-tlib-attr () :void
    (p-tlib-attr (:pointer tlibattr) :in)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_RemoteGetTypeInfoCount_Proxy(
    #|__RPC__in|# i-type-lib * This,
    #| [out] |# #|__RPC__out|# win32:uint *pc-TInfo);


void #|__RPC_STUB|# ITypeLib_RemoteGetTypeInfoCount_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_RemoteGetLibAttr_Proxy(
    #|__RPC__in|# i-type-lib * This,
    #| [out] |# #|__RPC__deref_out_opt|# LPTLIBATTR *pp-TLib-Attr,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *p-Dummy);


void #|__RPC_STUB|# ITypeLib_RemoteGetLibAttr_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_RemoteGetDocumentation_Proxy(
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# win32:int index,
    #| [in] |# win32:dword ref-Ptr-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Name,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Doc-String,
    #| [out] |# #|__RPC__out|# win32:dword *pdw-Help-Context,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Help-File);


void #|__RPC_STUB|# ITypeLib_RemoteGetDocumentation_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_RemoteIsName_Proxy(
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr sz-Name-Buf,
    #| [in] |# win32:ulong l-Hash-Val,
    #| [out] |# #|__RPC__out|# BOOL *pf-Name,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Lib-Name);


void #|__RPC_STUB|# ITypeLib_RemoteIsName_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_RemoteFindName_Proxy(
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr sz-Name-Buf,
    #| [in] |# win32:ulong l-Hash-Val,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(*pc-Found, *pc-Found)|# i-type-info **pp-TInfo,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(*pc-Found, *pc-Found)|# MEMBERID *rg-Mem-Id,
    #| [out][in] |# #|__RPC__inout|# win32:ushort *pc-Found,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *p-Bstr-Lib-Name);


void #|__RPC_STUB|# ITypeLib_RemoteFindName_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_LocalReleaseTLibAttr_Proxy(
    #|__RPC__in|# i-type-lib * This);


void #|__RPC_STUB|# ITypeLib_LocalReleaseTLibAttr_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);

|#

#| interface IType-Lib2 |#
#| [unique][uuid][object] |#

(cffi:defctype lptypelib-2 (:pointer #| [unique] |#  #|__RPC_unique_pointer|# i-type-lib-2))

;;TODO
;; EXTERN_C const IID IID_ITypeLib2;

(define-com-interface i-type-lib-2 "{00020411-0000-0000-C000-000000000046}" (i-type-lib)
  (get-cust-data () win32:hresult
    (#|__rpc__in|# guid win32:refguid :in)
    (#|__rpc__out|# p-var-val (:pointer variant) :out))

  (get-lib-statistics () win32:hresult
    (pc-unique-names (:pointer win32:ulong) :out)
    (pcch-unique-names (:pointer win32:ulong) :out))

  (get-documentation2 () win32:hresult
    (index win32:int :in)
    (lcid win32:lcid :in)
    (pbstr-help-string (:pointer win32:bstr) :out)
    (pdw-help-string-context (:pointer win32:dword) :out)
    (pbstr-help-string-dll (:pointer win32:bstr) :out))

  (get-all-cust-data () win32:hresult
    (#|__rpc__out|# p-cust-data (:pointer custdata) :out)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_RemoteGetLibStatistics_Proxy(
    #|__RPC__in|# i-type-lib-2 * This,
    #| [out] |# #|__RPC__out|# win32:ulong *pc-Unique-Names,
    #| [out] |# #|__RPC__out|# win32:ulong *pcch-Unique-Names);


void #|__RPC_STUB|# ITypeLib2_RemoteGetLibStatistics_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_RemoteGetDocumentation2_Proxy(
    #|__RPC__in|# i-type-lib-2 * This,
    #| [in] |# win32:int index,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword ref-Ptr-Flags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstr-Help-String,
    #| [out] |# #|__RPC__out|# win32:dword *pdw-Help-String-Context,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstr-Help-String-Dll);


void #|__RPC_STUB|# ITypeLib2_RemoteGetDocumentation2_Stub(
    IRpc-Stub-Buffer *This,
    IRpc-Channel-Buffer *_p-Rpc-Channel-Buffer,
    PRPC_MESSAGE _p-Rpc-Message,
    win32:dword *_pdw-Stub-Phase);
|#


(cffi:defctype lptypechangeevents (:pointer #| [unique] |# i-type-change-events))

(cffi:defcenum changekind
  (changekind-addmember	0)
  (changekind-deletemember	1)
  (changekind-setnames		2)
  (changekind-setdocumentation	3)
  (changekind-general		4)
  (changekind-invalidate	5)
  (changekind-changefailed	6)
  (changekind-max		7))

;;TODO
;; EXTERN_C const IID IID_ITypeChangeEvents;

;;TODO
;; EXTERN_C const IID IID_ITypeChangeEvents;

(define-com-interface i-type-change-events "{00020410-0000-0000-C000-000000000046}" (i-unknown)
  (request-type-change () win32:hresult
    (change-kind changekind :in)
    (p-tinfo-before (:pointer i-type-info) :in)
    (#|__rpc__in|#  p-str-name win32:lpolestr :in)
    (pf-cancel (:pointer win32:int) :out))

  (after-type-change () win32:hresult
    (change-kind changekind :in)
    (p-tinfo-after (:pointer i-type-info) :in)
    (#|__rpc__in|#  p-str-name win32:lpolestr :in)))

(cffi:defctype lperrorinfo (:pointer #| [unique] |#  #|__RPC_unique_pointer|# i-error-info))


;;TODO
;; EXTERN_C const IID IID_IError-Info;



(define-com-interface i-error-info "{1CF2B120-547D-101B-8E65-08002B2BD119}" (i-unknown)
  (get-guid () win32:hresult
    (#|__rpc__out|# p-guid (:pointer win32:guid) :out))

  (get-source () win32:hresult
    (#|__rpc__deref_out_opt|# p-bstr-source (:pointer win32:bstr) :out))

  (get-description () win32:hresult
    (#|__rpc__deref_out_opt|# p-bstr-description (:pointer win32:bstr) :out))

  (get-help-file () win32:hresult
    (#|__rpc__deref_out_opt|# p-bstr-help-file (:pointer win32:bstr) :out))

  (get-help-context () win32:hresult
    (#|__rpc__out|# pdw-help-context (:pointer win32:dword) :out)))

#| interface ICreate-Error-Info |#
#| [unique][uuid][object] |#

(cffi:defctype lpcreateerrorinfo (:pointer #| [unique] |#  #|__rpc_unique_pointer|# i-create-error-info))


;;TODO
;; EXTERN_C const IID IID_ICreate-Error-Info;


(define-com-interface i-create-error-info "{22F03340-547D-101B-8E65-08002B2BD119}" (i-unknown)
  (set-guid () win32:hresult
    (#|__rpc__in|# rguid win32:refguid :in))

  (set-source () win32:hresult
    (#|__rpc__in|# sz-source win32:lpolestr :in))

  (set-description () win32:hresult
    (#|__rpc__in|# sz-description win32:lpolestr :in))

  (set-help-file () win32:hresult
    (#|__rpc__in|# sz-help-file win32:lpolestr :in))

  (set-help-context () win32:hresult
    (dw-help-context win32:dword :in)))



#| interface ISupport-Error-Info |#
#| [unique][uuid][object] |#

(cffi:defctype lpsupporterrorinfo (:pointer #| [unique] |#  #|__RPC_unique_pointer|# i-support-error-info))


;;TODO
;; EXTERN_C const IID IID_ISupportErrorInfo;



(define-com-interface i-support-error-info "{DF0B3D60-548F-101B-8E65-08002B2BD119}" (i-unknown)
  (interface-supports-error-info () win32:hresult
    (#|__rpc__in|# riid win32:refiid :in)))


#| interface IType-Factory |#
#| [uuid][object] |#


;;TODO
;; EXTERN_C const IID IID_IType-Factory;

(define-com-interface i-type-factory "{0000002E-0000-0000-C000-000000000046}" (i-unknown)
  (create-from-type-info () win32:hresult
    (#|__rpc__in_opt|#
     p-type-info (:pointer i-type-info) :in)
    (#|__rpc__in|#
     riid win32:refiid :in)
    (#| [iid_is] |# #|__rpc__deref_out_opt|#
     ppv (:pointer (:pointer i-unknown)) :out)))


#| interface IType-Marshal |#
#| [uuid][object][local] |#


;;TODO
;; EXTERN_C const IID IID_IType-Marshal;


(setf (assoc-value slynk:*macroexpand-printer-bindings* '*print-circle*) t)

(define-com-interface i-type-marshal "{0000002D-0000-0000-C000-000000000046}" (i-unknown)
  (size () win32:hresult
    (pv-type win32:pvoid :in)
    (dw-dest-context win32:dword :in)
    (pv-dest-context win32:pvoid :in)
    (p-size (:pointer win32:ulong) :out))

  (marshal () win32:hresult
    (pv-type win32:pvoid :in)
    (dw-dest-context win32:dword :in)
    (pv-dest-context win32:pvoid :in)
    (cb-buffer-length win32:ulong :in)
    (p-buffer (:pointer win32:byte) :out)
    (pcb-written (:pointer win32:ulong) :out))

  (unmarshal () win32:hresult
    (pv-type win32:pvoid)
    (dw-flags win32:dword :in)
    (cb-buffer-length win32:ulong :in)
    (p-buffer (:pointer win32:byte) :in)
    (pcb-read (:pointer win32:ulong) :out))

  (free () win32:hresult
    (pv-type win32:pvoid :in)))

#| interface IRecord-Info |#
#| [uuid][object][local] |#

(cffi:defctype lprecordinfo (:pointer #| [unique] |# i-record-info))


;;TODO
;; EXTERN_C const IID IID_IRecord-Info;



(define-com-interface i-record-info "{0000002F-0000-0000-C000-000000000046}" (i-unknown)
  (record-init () win32:hresult
    (pv-new win32:pvoid))

  (record-clear () win32:hresult
    (pv-existing win32:pvoid))

  (record-copy () win32:hresult
    (pv-existing win32:pvoid)
    (pv-new win32:pvoid))

  (get-guid () win32:hresult
    (pguid (:pointer win32:guid) :out))

  (get-name () win32:hresult
    (pbstr-name (:pointer win32:bstr) :out))

  (get-size () win32:hresult
    (pcb-size (:pointer win32:ulong) :out))

  (get-type-info () win32:hresult
    (pp-type-info (:pointer (:pointer i-type-info)) :out))

  (get-field () win32:hresult
    (pv-data win32:pvoid)
    (sz-field-name win32:lpcolestr)
    (pvar-field (:pointer variant) :out))

  (get-field-no-copy () win32:hresult
    (pv-data win32:pvoid)
    (sz-field-name win32:lpcolestr)
    (pvar-field (:pointer variant) :out)
    (ppv-data-carray (:pointer win32:pvoid) :out))

  (put-field () win32:hresult
    (w-flags win32:ulong)
    (pv-data win32:pvoid)
    (sz-field-name win32:lpcolestr)
    (pvar-field (:pointer variant)))

  (put-field-no-copy () win32:hresult
    (w-flags win32:ulong)
    (pv-data win32:pvoid)
    (sz-field-name win32:lpcolestr)
    (pvar-field (:pointer variant)))

  (get-field-names () win32:hresult
    (pc-names (:pointer win32:ulong) :in :out)
    (#| [length_is][size_is][out] |# rg-bstr-names (:pointer win32:bstr)))

  (is-matching-type () win32:bool
    (p-record-info (:pointer i-record-info)))

  (record-create () win32:pvoid)

  (record-create-copy () win32:hresult
    (pv-source win32:pvoid)
    (ppv-dest (:pointer win32:pvoid) :out))

  (record-destroy () win32:hresult
    (pv-record win32:pvoid)))


#| interface IError-Log |#
#| [unique][uuid][object] |#

(cffi:defctype lperrorlog (:pointer i-error-log))

;;TODO
;; EXTERN_C const IID IID_IError-Log;

(define-com-interface i-error-log "{3127CA40-446E-11CE-8135-00AA004BB851}" (i-unknown)
  (add-error () win32:hresult
    (#|__rpc__in|# psz-prop-name win32:lpcolestr :in)
    (#|__rpc__in|# p-excep-info (:pointer excepinfo) :in)))


#| interface IProperty-Bag |#
#| [unique][uuid][object] |#

(cffi:defctype lppropertybag (:pointer i-property-bag))

;;TODO
;; EXTERN_C const IID IID_IProperty-Bag;

(define-com-interface i-property-bag "{55272A00-42CB-11CE-8135-00AA004BB851}" (i-unknown)
  (read () win32:hresult
    (psz-prop-name win32:lpcolestr :in)
    (p-var (:pointer variant) :in :out)
    (#| [unique][in] |# p-error-log (:pointer i-error-log)))

  (write () win32:hresult
    (#|__rpc__in|# psz-prop-name win32:lpcolestr :in)
    (#|__rpc__in|# p-var (:pointer variant) :in)))

#|

#| [call_as] |# win32:hresult STDMETHODCALLTYPE IPropertyBag_RemoteRead_Proxy( 
    #|__RPC__in|# IPropertyBag * This,
    #| [in] |# #|__RPC__in|# LPCOLESTR pszPropName,
    #| [out] |# #|__RPC__out|# variant *pVar,
    #| [unique][in] |# #|__RPC__in_opt|# IErrorLog *pErrorLog,
    #| [in] |# win32:dword varType,
    #| [in] |# #|__RPC__in_opt|# i-unknown *pUnkObj);

void #|__RPC_STUB|# IPropertyBag_RemoteRead_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    win32:dword *_pdwStubPhase);

#| Additional Prototypes for ALL interfaces |#

unsigned long             #|__RPC_USER|#  BSTR_UserSize(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# win32:bstr * ); 
unsigned char * #|__RPC_USER|#  BSTR_UserMarshal(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# win32:bstr * ); 
unsigned char * #|__RPC_USER|#  BSTR_UserUnmarshal(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# win32:bstr * ); 
void                      #|__RPC_USER|#  BSTR_UserFree(     #|__RPC__in|# unsigned long *, #|__RPC__in|# win32:bstr * ); 

unsigned long             #|__RPC_USER|#  CLEANLOCALSTORAGE_UserSize(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# CLEANLOCALSTORAGE * ); 
unsigned char * #|__RPC_USER|#  CLEANLOCALSTORAGE_UserMarshal(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# CLEANLOCALSTORAGE * ); 
unsigned char * #|__RPC_USER|#  CLEANLOCALSTORAGE_UserUnmarshal(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# CLEANLOCALSTORAGE * ); 
void                      #|__RPC_USER|#  CLEANLOCALSTORAGE_UserFree(     #|__RPC__in|# unsigned long *, #|__RPC__in|# CLEANLOCALSTORAGE * ); 

unsigned long             #|__RPC_USER|#  VARIANT_UserSize(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# variant * ); 
unsigned char * #|__RPC_USER|#  VARIANT_UserMarshal(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# variant * ); 
unsigned char * #|__RPC_USER|#  VARIANT_UserUnmarshal(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# variant * ); 
void                      #|__RPC_USER|#  VARIANT_UserFree(     #|__RPC__in|# unsigned long *, #|__RPC__in|# variant * ); 

unsigned long             #|__RPC_USER|#  BSTR_UserSize64(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# win32:bstr * ); 
unsigned char * #|__RPC_USER|#  BSTR_UserMarshal64(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# win32:bstr * ); 
unsigned char * #|__RPC_USER|#  BSTR_UserUnmarshal64(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# win32:bstr * ); 
void                      #|__RPC_USER|#  BSTR_UserFree64(     #|__RPC__in|# unsigned long *, #|__RPC__in|# win32:bstr * ); 

unsigned long             #|__RPC_USER|#  CLEANLOCALSTORAGE_UserSize64(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# CLEANLOCALSTORAGE * ); 
unsigned char * #|__RPC_USER|#  CLEANLOCALSTORAGE_UserMarshal64(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# CLEANLOCALSTORAGE * ); 
unsigned char * #|__RPC_USER|#  CLEANLOCALSTORAGE_UserUnmarshal64(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# CLEANLOCALSTORAGE * ); 
void                      #|__RPC_USER|#  CLEANLOCALSTORAGE_UserFree64(     #|__RPC__in|# unsigned long *, #|__RPC__in|# CLEANLOCALSTORAGE * ); 

unsigned long             #|__RPC_USER|#  VARIANT_UserSize64(     #|__RPC__in|# unsigned long *, unsigned long            , #|__RPC__in|# variant * ); 
unsigned char * #|__RPC_USER|#  VARIANT_UserMarshal64(  #|__RPC__in|# unsigned long *, #|__RPC__inout_xcount(0)|# unsigned char *, #|__RPC__in|# variant * ); 
unsigned char * #|__RPC_USER|#  VARIANT_UserUnmarshal64(#|__RPC__in|# unsigned long *, #|__RPC__in_xcount(0)|# unsigned char *, #|__RPC__out|# variant * ); 
void                      #|__RPC_USER|#  VARIANT_UserFree64(     #|__RPC__in|# unsigned long *, #|__RPC__in|# variant * ); 

#| [local] |# win32:hresult STDMETHODCALLTYPE i-dispatch_Invoke_Proxy( 
    i-dispatch * This,
    #| [annotation][in] |# 
    _In_  dispid dispIdMember,
    #| [annotation][in] |# 
    _In_  win32:refiid riid,
    #| [annotation][in] |# 
    _In_  win32:lcid lcid,
    #| [annotation][in] |# 
    _In_  win32:word wFlags,
    #| [annotation][out][in] |# 
    _In_  DISPPARAMS *pDispParams,
    #| [annotation][out] |# 
    _Out_opt_  variant *pVarResult,
    #| [annotation][out] |# 
    _Out_opt_  EXCEPINFO *pExcepInfo,
    #| [annotation][out] |# 
    _Out_opt_  win32:uint *puArgErr);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE i-dispatch_Invoke_Stub( 
    #|__RPC__in|# i-dispatch * This,
    #| [in] |# dispid dispIdMember,
    #| [in] |# #|__RPC__in|# win32:refiid riid,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword dwFlags,
    #| [in] |# #|__RPC__in|# DISPPARAMS *pDispParams,
    #| [out] |# #|__RPC__out|# variant *pVarResult,
    #| [out] |# #|__RPC__out|# EXCEPINFO *pExcepInfo,
    #| [out] |# #|__RPC__out|# win32:uint *pArgErr,
    #| [in] |# win32:uint cVarRef,
    #| [size_is][in] |# #|__RPC__in_ecount_full(cVarRef)|# win32:uint *rgVarRefIdx,
    #| [size_is][out][in] |# #|__RPC__inout_ecount_full(cVarRef)|# variantarg *rgVarRef);

#| [local] |# win32:hresult STDMETHODCALLTYPE IEnumVARIANT_Next_Proxy( 
    IEnumVARIANT * This,
    #| [in] |# win32:ulong celt,
    #| [length_is][size_is][out] |# variant *rgVar,
    #| [out] |# win32:ulong *pCeltFetched);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE IEnumVARIANT_Next_Stub( 
    #|__RPC__in|# IEnumVARIANT * This,
    #| [in] |# win32:ulong celt,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(celt, *pCeltFetched)|# variant *rgVar,
    #| [out] |# #|__RPC__out|# win32:ulong *pCeltFetched);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeComp_Bind_Proxy( 
    ITypeComp * This,
    #| [annotation][in] |# 
    #|__RPC__in|#  win32:lpolestr szName,
    #| [in] |# win32:ulong lHashVal,
    #| [in] |# win32:word wFlags,
    #| [out] |# i-type-info **ppTInfo,
    #| [out] |# DESCKIND *pDescKind,
    #| [out] |# BINDPTR *pBindPtr);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeComp_Bind_Stub( 
    #|__RPC__in|# ITypeComp * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr szName,
    #| [in] |# win32:ulong lHashVal,
    #| [in] |# win32:word wFlags,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-info **ppTInfo,
    #| [out] |# #|__RPC__out|# DESCKIND *pDescKind,
    #| [out] |# #|__RPC__deref_out_opt|# LPFUNCDESC *ppFuncDesc,
    #| [out] |# #|__RPC__deref_out_opt|# LPVARDESC *ppVarDesc,
    #| [out] |# #|__RPC__deref_out_opt|# ITypeComp **ppTypeComp,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *pDummy);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeComp_BindType_Proxy( 
    ITypeComp * This,
    #| [annotation][in] |# 
    #|__RPC__in|#  win32:lpolestr szName,
    #| [in] |# win32:ulong lHashVal,
    #| [out] |# i-type-info **ppTInfo,
    #| [out] |# ITypeComp **ppTComp);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeComp_BindType_Stub( 
    #|__RPC__in|# ITypeComp * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr szName,
    #| [in] |# win32:ulong lHashVal,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-info **ppTInfo);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetTypeAttr_Proxy( 
    i-type-info * This,
    #| [out] |# TYPEATTR **ppTypeAttr);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetTypeAttr_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [out] |# #|__RPC__deref_out_opt|# LPTYPEATTR *ppTypeAttr,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *pDummy);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetFuncDesc_Proxy( 
    i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# FUNCDESC **ppFuncDesc);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetFuncDesc_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# #|__RPC__deref_out_opt|# LPFUNCDESC *ppFuncDesc,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *pDummy);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetVarDesc_Proxy( 
    i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# VARDESC **ppVarDesc);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetVarDesc_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# win32:uint index,
    #| [out] |# #|__RPC__deref_out_opt|# LPVARDESC *ppVarDesc,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *pDummy);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetNames_Proxy( 
    i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [length_is][size_is][out] |# win32:bstr *rgBstrNames,
    #| [in] |# win32:uint cMaxNames,
    #| [out] |# win32:uint *pcNames);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetNames_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(cMaxNames, *pcNames)|# win32:bstr *rgBstrNames,
    #| [in] |# win32:uint cMaxNames,
    #| [out] |# #|__RPC__out|# win32:uint *pcNames);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetIDsOfNames_Proxy( 
    i-type-info * This,
    #| [annotation][size_is][in] |# 
    #|__RPC__in_ecount(cNames)|#  win32:lpolestr *rgszNames,
    #| [in] |# win32:uint cNames,
    #| [size_is][out] |# MEMBERID *pMemId);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetIDsOfNames_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_Invoke_Proxy( 
    i-type-info * This,
    #| [in] |# win32:pvoid pvInstance,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:word wFlags,
    #| [out][in] |# DISPPARAMS *pDispParams,
    #| [out] |# variant *pVarResult,
    #| [out] |# EXCEPINFO *pExcepInfo,
    #| [out] |# win32:uint *puArgErr);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_Invoke_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetDocumentation_Proxy( 
    i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [out] |# win32:bstr *pBstrName,
    #| [out] |# win32:bstr *pBstrDocString,
    #| [out] |# win32:dword *pdwHelpContext,
    #| [out] |# win32:bstr *pBstrHelpFile);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetDocumentation_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:dword refPtrFlags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrName,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrDocString,
    #| [out] |# #|__RPC__out|# win32:dword *pdwHelpContext,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrHelpFile);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetDllEntry_Proxy( 
    i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# INVOKEKIND invKind,
    #| [out] |# win32:bstr *pBstrDllName,
    #| [out] |# win32:bstr *pBstrName,
    #| [out] |# win32:word *pwOrdinal);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetDllEntry_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# INVOKEKIND invKind,
    #| [in] |# win32:dword refPtrFlags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrDllName,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrName,
    #| [out] |# #|__RPC__out|# win32:word *pwOrdinal);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_AddressOfMember_Proxy( 
    i-type-info * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# INVOKEKIND invKind,
    #| [out] |# win32:pvoid *ppv);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_AddressOfMember_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_CreateInstance_Proxy( 
    i-type-info * This,
    #| [in] |# i-unknown *pUnkOuter,
    #| [in] |# win32:refiid riid,
    #| [iid_is][out] |# win32:pvoid *ppvObj);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_CreateInstance_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [in] |# #|__RPC__in|# win32:refiid riid,
    #| [iid_is][out] |# #|__RPC__deref_out_opt|# i-unknown **ppvObj);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetContainingTypeLib_Proxy( 
    i-type-info * This,
    #| [out] |# ITypeLib **ppTLib,
    #| [out] |# win32:uint *pIndex);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_GetContainingTypeLib_Stub( 
    #|__RPC__in|# i-type-info * This,
    #| [out] |# #|__RPC__deref_out_opt|# i-type-lib **ppTLib,
    #| [out] |# #|__RPC__out|# win32:uint *pIndex);

#| [local] |# void STDMETHODCALLTYPE ITypeInfo_ReleaseTypeAttr_Proxy( 
    i-type-info * This,
    #| [in] |# TYPEATTR *pTypeAttr);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_ReleaseTypeAttr_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# void STDMETHODCALLTYPE ITypeInfo_ReleaseFuncDesc_Proxy( 
    i-type-info * This,
    #| [in] |# FUNCDESC *pFuncDesc);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_ReleaseFuncDesc_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# void STDMETHODCALLTYPE ITypeInfo_ReleaseVarDesc_Proxy( 
    i-type-info * This,
    #| [in] |# VARDESC *pVarDesc);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo_ReleaseVarDesc_Stub( 
    #|__RPC__in|# i-type-info * This);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeInfo2_GetDocumentation2_Proxy( 
    i-type-info-2 * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:lcid lcid,
    #| [out] |# win32:bstr *pbstrHelpString,
    #| [out] |# win32:dword *pdwHelpStringContext,
    #| [out] |# win32:bstr *pbstrHelpStringDll);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeInfo2_GetDocumentation2_Stub( 
    #|__RPC__in|# i-type-info-2 * This,
    #| [in] |# MEMBERID memid,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword refPtrFlags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstrHelpString,
    #| [out] |# #|__RPC__out|# win32:dword *pdwHelpStringContext,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstrHelpStringDll);

#| [local] |# win32:uint STDMETHODCALLTYPE ITypeLib_GetTypeInfoCount_Proxy( 
    i-type-lib * This);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_GetTypeInfoCount_Stub( 
    #|__RPC__in|# i-type-lib * This,
    #| [out] |# #|__RPC__out|# win32:uint *pcTInfo);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib_GetLibAttr_Proxy( 
    i-type-lib * This,
    #| [out] |# TLIBATTR **ppTLibAttr);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_GetLibAttr_Stub( 
    #|__RPC__in|# i-type-lib * This,
    #| [out] |# #|__RPC__deref_out_opt|# LPTLIBATTR *ppTLibAttr,
    #| [out] |# #|__RPC__out|# CLEANLOCALSTORAGE *pDummy);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib_GetDocumentation_Proxy( 
    i-type-lib * This,
    #| [in] |# win32:int index,
    #| [out] |# win32:bstr *pBstrName,
    #| [out] |# win32:bstr *pBstrDocString,
    #| [out] |# win32:dword *pdwHelpContext,
    #| [out] |# win32:bstr *pBstrHelpFile);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_GetDocumentation_Stub( 
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# win32:int index,
    #| [in] |# win32:dword refPtrFlags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrName,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrDocString,
    #| [out] |# #|__RPC__out|# win32:dword *pdwHelpContext,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrHelpFile);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib_IsName_Proxy( 
    i-type-lib * This,
    #| [annotation][out][in] |# 
    #|__RPC__inout|#  win32:lpolestr szNameBuf,
    #| [in] |# win32:ulong lHashVal,
    #| [out] |# BOOL *pfName);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_IsName_Stub( 
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr szNameBuf,
    #| [in] |# win32:ulong lHashVal,
    #| [out] |# #|__RPC__out|# BOOL *pfName,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrLibName);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib_FindName_Proxy( 
    i-type-lib * This,
    #| [annotation][out][in] |# 
    #|__RPC__inout|#  win32:lpolestr szNameBuf,
    #| [in] |# win32:ulong lHashVal,
    #| [length_is][size_is][out] |# i-type-info **ppTInfo,
    #| [length_is][size_is][out] |# MEMBERID *rgMemId,
    #| [out][in] |# win32:ushort *pcFound);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_FindName_Stub( 
    #|__RPC__in|# i-type-lib * This,
    #| [in] |# #|__RPC__in|# win32:lpolestr szNameBuf,
    #| [in] |# win32:ulong lHashVal,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(*pcFound, *pcFound)|# i-type-info **ppTInfo,
    #| [length_is][size_is][out] |# #|__RPC__out_ecount_part(*pcFound, *pcFound)|# MEMBERID *rgMemId,
    #| [out][in] |# #|__RPC__inout|# win32:ushort *pcFound,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pBstrLibName);

#| [local] |# void STDMETHODCALLTYPE ITypeLib_ReleaseTLibAttr_Proxy( 
    i-type-lib * This,
    #| [in] |# TLIBATTR *pTLibAttr);


#| [nocode][call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib_ReleaseTLibAttr_Stub( 
    #|__RPC__in|# i-type-lib * This);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_GetLibStatistics_Proxy( 
    i-type-lib-2 * This,
    #| [out] |# win32:ulong *pcUniqueNames,
    #| [out] |# win32:ulong *pcchUniqueNames);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_GetLibStatistics_Stub( 
    #|__RPC__in|# i-type-lib-2 * This,
    #| [out] |# #|__RPC__out|# win32:ulong *pcUniqueNames,
    #| [out] |# #|__RPC__out|# win32:ulong *pcchUniqueNames);

#| [local] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_GetDocumentation2_Proxy( 
    i-type-lib-2 * This,
    #| [in] |# win32:int index,
    #| [in] |# win32:lcid lcid,
    #| [out] |# win32:bstr *pbstrHelpString,
    #| [out] |# win32:dword *pdwHelpStringContext,
    #| [out] |# win32:bstr *pbstrHelpStringDll);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE ITypeLib2_GetDocumentation2_Stub( 
    #|__RPC__in|# i-type-lib-2 * This,
    #| [in] |# win32:int index,
    #| [in] |# win32:lcid lcid,
    #| [in] |# win32:dword refPtrFlags,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstrHelpString,
    #| [out] |# #|__RPC__out|# win32:dword *pdwHelpStringContext,
    #| [out] |# #|__RPC__deref_out_opt|# win32:bstr *pbstrHelpStringDll);

#| [local] |# win32:hresult STDMETHODCALLTYPE IPropertyBag_Read_Proxy( 
    IPropertyBag * This,
    #| [in] |# LPCOLESTR pszPropName,
    #| [out][in] |# variant *pVar,
    #| [unique][in] |# IErrorLog *pErrorLog);


#| [call_as] |# win32:hresult STDMETHODCALLTYPE IPropertyBag_Read_Stub( 
    #|__RPC__in|# IPropertyBag * This,
    #| [in] |# #|__RPC__in|# LPCOLESTR pszPropName,
    #| [out] |# #|__RPC__out|# variant *pVar,
    #| [unique][in] |# #|__RPC__in_opt|# IErrorLog *pErrorLog,
    #| [in] |# win32:dword varType,
                                                                       #| [in] |# #|__RPC__in_opt|# i-unknown *pUnkObj);
|#

