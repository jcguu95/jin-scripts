(asdf:defsystem "jin.hello"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("jin.dependencies")
  :components ((:file "package")
               (:file "hello")))