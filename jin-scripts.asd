#-asdf3.1 (error "`jin-scripts' requires ASDF 3.1")
(declaim (optimize (debug 3) (safety 3)))

(asdf:defsystem "jin-scripts"
  :description "Jin's Scripts"
  :author "Jin <jcguu95@gmail.com>"
  :licence "MIT"
  :version "0.0"
  :properties ((#:author-email . "jcguu95@gmail.com"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (
               "alexandria"
               "local-time"
               "log4cl"
               "rutils"
               "unix-opts"
               "ironclad"
               "xmls"
               "babel"
               "cl-ppcre"

               ;;
               "jin.dependencies"
               "jin.hello"

               )
  :components ((:file "package")
               (:file "util")

               ;; (:file "hello")
               (:file "sha1-rename")
               (:file "arxiv-rename")
               (:file "macos-screenshot-rename")
               (:file "backup")

               (:file "main")))
