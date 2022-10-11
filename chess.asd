(asdf:defsystem "chess"
  :depends-on (:sketch)
  :serial t
  :components ((:file "package")
               (:file "move")
               (:file "util")
               (:file "chess")))
