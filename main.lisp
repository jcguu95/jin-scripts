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
     "hello"              #'jin.hello:main
     "sha1-rename"        #'jin.sha1-rename:main
     "backup"             #'jin.backup:main
     "arxiv-rename"       #'jin.arxiv-rename:main
     "yazi-cache-cleaner" #'jin.yazi-cache-cleaner:main
     "screenshot-rename"  #'jin.macos-screenshot-rename:main))

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
  (let ((docstring (or (documentation fn t) "")))
    (log:info docstring)))

(defun build! ()
  "Build jin-scripts."
  (let ((cmd (format nil "ros run --non-interactive --load \"~a\" --load \"~a\""
                     (merge-pathnames (pathname ".sbclrc")
                                      (user-homedir-pathname))
                     (merge-pathnames (pathname "build.lisp")
                                      (asdf:system-source-directory "jin-scripts")))))
    (format t cmd)
    (uiop:run-program cmd :output *standard-output*
                          :error-output *error-output*
                          :ignore-error-status t)))

(defun main ()
  "Entry point for JIN-SCRIPTS. The built binary should also start from here."
  (let* ((dir #P"/tmp/")
         (log-file   (merge-pathnames (pathname (format nil "cl.jin-scripts.log"))   dir))
         (error-file (merge-pathnames (pathname (format nil "cl.jin-scripts.error")) dir)))
    (with-open-file (log-stream log-file :direction :output :if-does-not-exist :create :if-exists :append)
      (with-open-file (error-stream error-file :direction :output :if-does-not-exist :create :if-exists :append)
        ;; Log everything from now on..
        (let ((*standard-output* (make-broadcast-stream log-stream   *standard-output*))
              (*error-output*    (make-broadcast-stream error-stream *error-output*)))
          (log:config :info :stream *standard-output*)
          (log:config :sane)            ; avoid duplicated message in terminal (has to be put after the previous config form)
          (multiple-value-bind (options free-args)
              (unix-opts:get-opts)
            (let* ((help? (getf options :help))
                   (system (asdf:find-system "jin-scripts"))
                   (version (asdf:component-version system))
                   (description (asdf::component-description system)))
              (declare (ignorable system version description))
              (format t "~%~%")
              (log:info "Calling the main function..") ; FIXME this doesn't go into *standard-output*, so not logged..
              (format t "System: ~a~%Version: ~a~%Description: ~a~%-------------------------------~%~%" system version description)

              (if (and help? (not free-args))
                  (unix-opts:describe
                   :prefix "A unified program to call JIN-SCRIPTS."
                   :args "WHOM")        ; TODO What is this?
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
          (uiop:quit))))))

(defun current-lisp-argv ()
  "Tells me how this lisp was started."
  (or
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*))
