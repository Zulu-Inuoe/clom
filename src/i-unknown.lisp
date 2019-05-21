(in-package #:clom)

(define-com-interface i-unknown "{00000000-0000-0000-C000-000000000046}" ()
  (query-interface () win32:hresult
    (riid win32:refiid)
    (ppv-obj (:pointer (:pointer :void)) :out :retval))

  (add-ref () win32:ulong)

  (release () win32:ulong))
