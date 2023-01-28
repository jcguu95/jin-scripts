(asdf:defsystem "jin.dependencies"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("alexandria"
               "local-time"
               "log4cl"
               "rutils"
               "unix-opts"
               "ironclad"
               "xmls"
               "babel"
               "cl-ppcre")
  :components ())
