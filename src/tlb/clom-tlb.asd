(defsystem #:clom-tlb
  :version "0.1.0"
  :description "TLB (Type Library) Importer"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "package")
   (:file "clom-tlb"))
  :depends-on
  (#:clom
   #:clom-automation
   #:alexandria
   #:win32
   #:trivial-indent
   #:exit-hooks))
