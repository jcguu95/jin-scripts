(in-package :jin-scripts)
(named-readtables:in-readtable rutils:rutils-readtable)

;; TODO Expose an option to build and install from command line
;; `cl` too.
;;
;; TODO Move utililities to their own file.
;;
;; TODO Separate different facilities into their own packages and
;; systems. If a system fails to load, skip it and emit a warning.
;;
;; TODO Understand how to use options.

(defparameter *registered-commands*
  #h(equal
     "hello"             #'jin.hello:main
     "sha1-rename"       #'jin.sha1-rename:main
     "backup"            #'jin.backup:main
     "arxiv-rename"      #'jin.arxiv-rename:main
     "screenshot-rename" #'jin.macos-screenshot-rename:main))

(unix-opts:define-opts
    (:name :help
     :description "Print this help text"
     :short #\h
     :long "help"))

(defun generic-response (free-args options)
  (format t "OPTIONS   :: ~%~{~3d: ~s~^~%~}~%"
          (loop for i from 0 to (1- (length options))
                collect i
                collect (nth i options)))
  (format t "FREE-ARGS :: ~%~{~3d: ~s~^~%~}~%"
          (loop for i from 0 to (1- (length free-args))
                collect i
                collect (nth i free-args))))

(defun print-docstring (fn)
  (format t "Docstring ::~%~a~%"
          (or (documentation fn t) "")))

(defun main ()
  "Entry point for JIN-SCRIPTS."
  (multiple-value-bind (options free-args)
      (unix-opts:get-opts)
    (let ((help? (getf options :help)))
      (if (and help? (not free-args))
          (unix-opts:describe
           :prefix "A unified program to call JIN-SCRIPTS."
           :args "WHOM")                ; TODO What is this?
          (let ((command (car free-args)))
            (aif (gethash command *registered-commands*)
                 (let ((fn it))
                   (if help?
                       (progn
                         (print-docstring fn)
                         (generic-response free-args options))
                       (funcall fn free-args options)))
                 (progn
                   (format t "Unknown Operator :: ~s~%" (nth 0 free-args))
                   (generic-response free-args options)))))))
  (uiop:quit))




(defun current-lisp-argv ()
  "Tells me how this lisp was started."
  (or
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*))
