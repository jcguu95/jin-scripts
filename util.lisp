(in-package :jin-scripts)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
