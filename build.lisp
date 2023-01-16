(ql:quickload :jin-scripts)

;; TODO Abstract the directory /Users/jin/.local/bin/

(let ((commands (alexandria:hash-table-keys
                 jin-scripts::*registered-commands*)))
  (loop for command in commands do
    (let ((file-path (format nil "/Users/jin/.local/bin/cl.~a" command)))
      (with-open-file (file file-path
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite)
        (format file "#!/bin/bash~%~%")
        (format file "cl ~a \"$@\"~%~%" command)
        (format file "exit"))
      (uiop:run-program
       (format nil "chmod 744 ~a" file-path)
       :output t :error-output t))))

(sb-ext:save-lisp-and-die
 "/Users/jin/.local/bin/cl"
 :toplevel 'jin-scripts:main
 :executable t)
