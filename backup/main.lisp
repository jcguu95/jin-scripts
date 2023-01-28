(in-package :jin.backup)

;; TODO split run-program output stream to stdout AND a file.

(defparameter *name* "macbook-air-m2-2022")

(defparameter *rclone-repo-name* "gdrive-sbu")

(defparameter *local-mount-point*
  (uiop:native-namestring "~/.backup"))

(defparameter *exclude-dirs*
  (mapcar #'uiop:native-namestring
          `(,*local-mount-point*
            "~/Desktop"
            "~/Documents"
            "~/Downloads"
            "~/Library"
            "~/Movies"
            "~/Music"
            "~/Pictures"
            "~/Public"
            "~/.local"
            "~/.cache"
            "~/.Trash"
            "~/.no-backup"
            "~/.+PLUGS/tilde-local/stow/zcy"
            "~/.+PLUGS/tilde-local/stow/to-watch")))

;;; Util

(defun timestamp ()
  (local-time:format-timestring
   nil (local-time:now) :format
   '((:year 4) (:month 2) (:day 2) "-" (:hour 2) (:min 2) (:sec 2))))

;;;

(defun borg-create (comment exclude-dirs)
  "Create deduplicated backups with borg."
  (let* ((repo (format nil "~a/~a::~a-~a"
                       *local-mount-point*
                       *name*
                       (uiop:hostname)
                       (timestamp)))
         (target (uiop:native-namestring "~/"))
         (cmd `("borg" "create"
                "--info" "--json" "--show-rc"
                "--progress" "--exclude-caches"
                "--comment" ,(format nil "~a" comment)
                ,@(loop for dir in exclude-dirs
                        collect "--exclude"
                        collect (format nil "~a/*" dir))
                ,repo ,target)))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output t :error-output t)))))

(defun borg-prune (&key (hour 24) (day 7) (week 4) (month 1))
  "Prune redundant borg archives."
  (let ((cmd `("borg" "prune"
               "--list" "--show-rc"
               "--keep-hourly"  ,(format nil "~d" hour)
               "--keep-daily"   ,(format nil "~d" day)
               "--keep-weekly"  ,(format nil "~d" week)
               "--keep-monthly" ,(format nil "~d" month)
               ,(format nil "~a/~a" *local-mount-point* *name*))))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output t :error-output t)))))

(defun rclone-backup ()
  "Backup with rclone."
  (let ((cmd `("rclone" "--verbose" "sync"
               ,(format nil "~a/~a/" *local-mount-point* *name*)
               ,(format nil "~a:~a/" *rclone-repo-name* *name*))))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output t :error-output t :ignore-error-status t)))))

(defun backup! (free-args options)
  "Main function."
  (declare (ignore free-args options))
  (let ((result `(,(borg-create "Automatic Backup" *exclude-dirs*)
                  ,(borg-prune)
                  ,(rclone-backup)))
        (message))
    (if (equal result '(0 0 0))
        (setf message "success")
        (setf message (format nil "Something went wrong; code: ~a" result)))
    (mac-notify! message :title "Automatic Backup (backup.lisp)")
    result))

(setf (fdefinition 'main) #'backup!)
