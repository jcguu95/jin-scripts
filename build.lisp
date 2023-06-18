(ql:quickload :jin-scripts)

(defparameter *main-program-name* "cl")

(defparameter *target*
  (merge-pathnames ".local/bin" (user-homedir-pathname)))

;; Create a shell wrapper for each exposed command, a workaround
;; to enable tab completion in shell.
(let ((commands (alexandria:hash-table-keys
                 jin-scripts::*registered-commands*)))
  (loop for command in commands do
    (let ((file-path (format nil "~a/~a.~a" *target* *main-program-name* command)))
      (with-open-file (file file-path
                            :direction         :output
                            :if-exists         :overwrite ; can make it safer?
                            :if-does-not-exist :create)
        (format file "#!/bin/bash~%~%")
        (format file "~a ~a \"$@\"~%~%" *main-program-name* command)
        (format file "exit"))
      (uiop:run-program
       (format nil "chmod +x ~a" file-path)
       :output t :error-output t))))

(log:info t "Saving lisp and die..!~%")

(sb-ext:save-lisp-and-die
 (format nil "~a/~a" *target* *main-program-name*)
 :toplevel 'jin-scripts:main
 :executable t)
