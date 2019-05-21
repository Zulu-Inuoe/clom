(defpackage #:clom
  (:use
   #:alexandria
   #:cl)
  (:export
   #:define-com-interface

   #:i-unknown
   #:i-unknown-add-ref
   #:i-unknown-query-interface
   #:i-unknown-release

   #:define-com-impl))
