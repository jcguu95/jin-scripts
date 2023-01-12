(in-package :jin-scripts)

;; Use the following to build.
;; ros run --non-interactive --load ~/.sbclrc --load build.lisp

;; TODO Expose an option to build and install from command line `cl` too.

(defparameter *registered-commands*
  `(("hello"       . ,#'hello-world)
    ("sha1-rename" . ,#'sha1-rename)))

(defun generic-response (free-args options)
  (format t "Unknown Operator :: ~s~%" (nth 0 free-args))
  (format t "OPTIONS          :: ~%~{~3d: ~s~^~%~}"
          (loop for i from 0 to (1- (length options))
                collect i
                collect (nth i options)))
  (format t "FREE-ARGS        :: ~%~{~3d: ~s~^~%~}~%"
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
    (if (and (getf options :help)       ; FIXME Broken.
             (not free-args))
        (unix-opts:describe
         :prefix "A unified program to call JIN-SCRIPTS."
         :args "WHOM")                  ; TODO What is this?
        (let ((command (car free-args)))
          (aif (assoc command *registered-commands* :test #'string=)
               (let ((fn (cdr it)))
                 (print-docstring fn)   ; TODO Only print this when -h is on.
                 (funcall fn free-args options))
               (generic-response free-args options)))))
  (uiop:quit))
