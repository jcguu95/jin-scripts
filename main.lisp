(in-package :jin-scripts)
(named-readtables:in-readtable rutils:rutils-readtable)

;; TODO Expose an option to build and install from command line `cl` too.

(defparameter *registered-commands*
  #h(equal
     "hello"        #'hello-world
     "sha1-rename"  #'sha1-rename
     "backup"       #'backup!
     "arxiv-rename" #'arxiv-rename))

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



;;; TODO Under Construction
(defun mac-notify! (msg &key (title "") (subtitle "") (sound "Submarine"))
  (let ((cmd (format
              nil
              "osascript -e 'display notification ~s with title ~s subtitle ~s sound name ~s'"
              msg title subtitle sound)))
    (uiop:launch-program cmd :output *standard-output*)))

(defun mac-button-ok? (reply)
  (string= reply (format nil "button returned:OK~%")))

(defun mac-dialog! (msg)
  "Synchronously run a mac dialog window. Return user's response."
  ;; TODO Make it more sophisticated by differing genuine error
  ;; and CANCEL response.
  (let* ((cmd (format nil "osascript -e 'display dialog ~s'" msg))
         (reply (ignore-errors (uiop:run-program cmd :output :string))))
    (mac-button-ok? reply)))

;; TODO Learn how to use progressbar in macos..

;; (mac-notify! "he")
;; (mac-dialog! "he")

(defun current-lisp-argv ()
  "Tells me how this lisp was started."
  (or
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*))




;; (ql:quickload :cl-git) ; not loading properly because of grovel on mac..
