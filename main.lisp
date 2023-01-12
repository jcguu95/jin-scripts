(in-package :jin-scripts)

;; TODO Expose an option to build and install from command line `cl` too.

(defparameter *registered-commands*
  `(("hello"       . ,#'hello-world)
    ("sha1-rename" . ,#'sha1-rename)))

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
            (aif (assoc command *registered-commands* :test #'string=)
                 (let ((fn (cdr it)))
                   (if help?
                       (progn
                         (print-docstring fn)
                         (generic-response free-args options))
                       (funcall fn free-args options)))
                 (progn
                   (format t "Unknown Operator :: ~s~%" (nth 0 free-args))
                   (generic-response free-args options)))))))
  (uiop:quit))
