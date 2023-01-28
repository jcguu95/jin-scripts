(in-package :jin.macos-notification)

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
