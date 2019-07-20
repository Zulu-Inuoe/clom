(defsystem #:clom
  :version "0.1.0"
  :description "A library implementing Component Object Model(COM) support."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "package")
   (:file "cffi+")
   (:file "strings")
   (:file "com-error")
   (:file "getprop")
   (:file "com-interface")
   (:file "define-com-interface")
   (:file "i-unknown")
   (:file "oaidl"))
  :depends-on
  (#:alexandria
   #:babel
   #:cffi
   #:cl-ppcre
   #:win32
   #:trivial-cltl2
   #:trivial-indent
   #:exit-hooks))
