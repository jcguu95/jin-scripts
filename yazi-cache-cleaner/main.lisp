(in-package :jin.yazi-cache-cleaner)

;; Usage
;;
;; This script cleans up the yazi cache folder (default to
;; ~/.cache/yazi/) and reports its action.

(defparameter *title* "Yazi Cache Cleaner (yazi-cache-cleaner.lisp)")
(defparameter *yazi-cache-folder* "~/.cache/yazi/")

(defun main (free-args options)
  "Main function. It cleans up the yazi cache folder and reports its action."
  (declare (ignore free-args options))
  (let* ((files (uiop:directory-files
                 (uiop:native-namestring *yazi-cache-folder*)))
         (count (length files))
         (new-count))
    (mapcar #'delete-file files)
    (setf new-count (length
                     (uiop:directory-files
                      (uiop:native-namestring *yazi-cache-folder*))))
    (case new-count
      (0 (jin.macos-notification:mac-notify!
          (format nil "Yazi cache cleared successfully: ~a files are removed." count)
          :title *title*))
      (t (jin.macos-notification:mac-notify!
          (format nil "Yazi cache clearer error: ~a files are removed, ~a files remained."
                  (- count new-count) new-count)
          :title *title*)))))
