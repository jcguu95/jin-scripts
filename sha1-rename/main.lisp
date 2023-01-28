(in-package :jin.sha1-rename)

(defun file-content-sha1 (file-path)
  (assert (probe-file file-path))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (babel:string-to-octets
     (uiop:read-file-string file-path)))))

;; TODO Make it take a list of path, and make the change atomic
;; (by passing to /tmp).
;;
;; BUG Fix TODO
;; debugger invoked on a SB-INT:STREAM-DECODING-ERROR in thread
;; #<THREAD "main thread" RUNNING {7006460003}>:
;;   :UTF-8 stream decoding error on
;;   #<SB-SYS:FD-STREAM for "file /private/tmp/PDT Candidate Questionnaire - Research.docx" {7006432BB3}>:
;;     the octet sequence #(220 217) cannot be decoded.
(defun rename-file-with-sha1 (path)
  (let* ((path   (probe-file path))
         (source path)
         (name   (pathname-name path))
         (type   (or (pathname-type path) ""))
         (dir    (cdr (pathname-directory path))))
    (assert source)
    (if name
        (let ((target (format nil "~a/~a.~a.~a"
                              (format nil "~{/~a~}" dir)
                              name
                              (file-content-sha1 path)
                              type)))
          (format t "Renaming~%   ~a~%->~%   ~a~%" source target)
          (uiop:rename-file-overwriting-target source target))
        (format t "Skip directory~%   ~a~%" source))))

(defun sha1-rename (free-args options)
  "Rename the file specified by PATH by appending the sha1sum of
its content after its name and before its ext."
  (declare (ignore options))
  (let ((paths (cdr free-args)))
    ;; TODO Test on directories
    (loop for path in paths do
      (rename-file-with-sha1 path))))

(setf (fdefinition 'main) #'sha1-rename)

;; TODO * CL script: hash checker , hash updater
