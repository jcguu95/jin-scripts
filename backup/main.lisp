(in-package :jin.backup)

;; TODO split run-program output stream to stdout AND a file.

;; TODO Implement handy #'borg-delete to be called in the repl
;; without recalling the command.
;; e.g. "borg delete ~/.backup/macbook-air-m2-2022/ guu.local-20230318-090000"

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

(defun borg-create (comment exclude-dirs &key (timestamp (timestamp)))
  "Create deduplicated backups with borg."
  (let* ((repo (format nil "~a/~a::~a-~a"
                       *local-mount-point*
                       *name*
                       (uiop:hostname)
                       timestamp))
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
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

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
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

(defun rclone-backup ()
  "Backup with rclone."
  (let ((cmd `("rclone" "--verbose" "sync"
               ,(format nil "~a/~a/" *local-mount-point* *name*)
               ,(format nil "~a:~a/" *rclone-repo-name* *name*))))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

(defun backup! (free-args options)
  (declare (ignore free-args options))
  (let ((code (borg-create "Automatic Backup" *exclude-dirs* :timestamp (timestamp))))
    (case code
      (0 (jin.macos-notification:mac-notify!
          "Borg archive created."
          :title "Automatic Backup (backup.lisp)"))
      (1 (jin.macos-notification:mac-notify!
          "Warning during borg-create. Please read log."
          :title "Automatic Backup (backup.lisp)"))
      (t (jin.macos-notification:mac-notify!
          "Error or killed during borg-create. Please read log."
          :title "Automatic Backup (backup.lisp)")
       (log:info "Aborting..")
       (uiop:quit code))))

  (let ((code (borg-prune)))
    (case code
      (0 (jin.macos-notification:mac-notify!
          "Borg pruned."
          :title "Automatic Backup (backup.lisp)"))
      (1 (jin.macos-notification:mac-notify!
          "Warning during #'borg-prune. Please read log."
          :title "Automatic Backup (backup.lisp)"))
      (t (jin.macos-notification:mac-notify!
          "Error or killed during #'borg-prune. Please read log."
          :title "Automatic Backup (backup.lisp)")
       (log:info "Aborting..")
       (uiop:quit code))))

  (let ((code (rclone-backup)))
    (case code
      (0 (jin.macos-notification:mac-notify!
          "Rclone pushed successfully!"
          :title "Automatic Backup (backup.lisp)"))
      (t (jin.macos-notification:mac-notify!
          "Rclone error. Please read log."
          :title "Automatic Backup (backup.lisp)")
       (log:info "Aborting..")
       (uiop:quit code)))))

;; Main function.
(setf (fdefinition 'main) #'backup!)
