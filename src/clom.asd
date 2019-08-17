(defsystem #:clom
  :version "0.1.0"
  :description "A library implementing Component Object Model(COM) support."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "cffi+")
   (:file "package")
   (:file "getprop")
   (:file "strings")
   (:file "iid")
   (:file "indentation")
   (:file "com-error")
   (:file "com-interface")
   (:file "marshaling")
   (:file "define-com-interface")
   (:file "i-unknown"))
  :depends-on
  (#:alexandria
   #:babel
   #:cffi
   #:cl-ppcre
   #:win32
   #:trivial-cltl2))
