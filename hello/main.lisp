(in-package :jin.hello)

(defun hello-world (free-args options)
  (declare (ignore free-args options))
  (format t "Hello World!~%"))

(setf (fdefinition 'main) #'hello-world)
