(asdf:defsystem "jin.macos-notification"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("jin.dependencies")
  :components ((:file "package")
               (:file "main")))
