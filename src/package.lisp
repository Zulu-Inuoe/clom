(defpackage #:clom
  (:use
   #:alexandria
   #:cl)
  (:export
   #:com-error
   #:com-error-hresult
   #:com-error-string
   #:check-com-error

   #:define-com-interface

   #:i-unknown
   #:i-unknown-add-ref
   #:i-unknown-query-interface
   #:i-unknown-release))
