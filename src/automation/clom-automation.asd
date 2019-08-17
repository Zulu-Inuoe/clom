(defsystem #:clom-automation
  :version "0.1.0"
  :description "'Automation' support for CLOM."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "package")
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
