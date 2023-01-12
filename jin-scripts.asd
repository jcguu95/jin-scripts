#-asdf3.1 (error "`jin' requires ASDF 3.1")

(asdf:defsystem "jin-scripts"
  :description "Jin's Scripts"
  :author "Jin <jcguu95@gmail.com>"
  :licence "MIT"
  :version "0.0"
  :properties ((#:author-email . "jcguu95@gmail.com"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (
               "alexandria"
               "unix-opts"
               "ironclad"
               "babel"
               )
  :components ((:file "package")
               (:file "util")

               (:file "hello")
               (:file "sha1-rename")

               (:file "main")))
