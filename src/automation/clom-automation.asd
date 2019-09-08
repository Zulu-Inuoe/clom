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
   #:cffi
   #:clom
   #:win32))
