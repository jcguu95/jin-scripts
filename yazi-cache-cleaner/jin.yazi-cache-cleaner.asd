(asdf:defsystem "jin.yazi-cache-cleaner"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("jin.dependencies"
               "jin.macos-notification")
  :components ((:file "package")
               (:file "main")))
