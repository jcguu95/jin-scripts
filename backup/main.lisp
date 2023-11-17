(in-package :jin.backup)

;; TODO split run-program output stream to stdout AND a file.

;; TODO Implement handy #'borg-delete to be called in the repl
;; without recalling the command.
;; e.g. "borg delete ~/.backup/macbook-air-m2-2022/ guu.local-20230318-090000"

;; Usage
;;
;; 1. Customize *name*, and run (uiop:run-program (format nil "borg init \"~a\"" *repo*)).
;; 2. Setup rclone and the gdrive rclone repo with the correct name as *rclone-repo-name*.
;; 3. Run main function.

(defparameter *name* "macbook-air-m2-2022")

(defparameter *rclone-repo-name* "gdrive-umass-amherst")

(defparameter *local-mount-point*
  (uiop:native-namestring "~/.backup"))

(defparameter *repo* (format nil "~a/~a" *local-mount-point* *name*))

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
            "~/.Trash"
            "~/.dropbox"
            "~/.no-backup"

            "~/.cache"
            "~/.emacs.d/.local/cache"
            "~/.+PLUGS/Dropbox/.dropbox.cache"

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
  (log:info "Creating backup in borg..")
  (log:info exclude-dirs)
  (let* ((archive-name (format nil "~a-~a" (uiop:hostname) timestamp))
         (archive (format nil "~a::~a" *repo* archive-name))
         (target (uiop:native-namestring "~/"))
         (cmd `("borg" "create"
                "--info" "--json" "--show-rc"
                "--progress" "--exclude-caches"
                "--comment" ,(format nil "~a" comment)
                ,@(loop for dir in exclude-dirs
                        collect "--exclude"
                        collect (format nil "~a/*" dir))
                ,archive ,target)))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

(defun borg-prune (&key (second 2) (hour 24) (day 7) (week 4) (month 1))
  "Prune redundant borg archives."
  ;; NOTE This won't size down the archive. Use `borg compact` for that.
  (log:info "Pruning in borg archive..")
  (let ((cmd `("borg" "prune"
               "--list" "--show-rc"
               "--stats"
               "--keep-secondly" ,(format nil "~d" second) ; keep 2 so we can print a sane borg diff later
               "--keep-hourly"   ,(format nil "~d" hour)
               "--keep-daily"    ,(format nil "~d" day)
               "--keep-weekly"   ,(format nil "~d" week)
               "--keep-monthly"  ,(format nil "~d" month)
               ,*repo*)))
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

;; TODO Make a series of info dumper.
(defun borg-info (&key (last 1))
  (uiop:run-program (format nil "borg info ~a --last ~a" *repo* last)
                    :output *standard-output* :error *error-output* :ignore-error-status t))
;; (borg-info)

(defun borg-list ()
  "List all archives in the borg repo *REPO*, from the latest to the oldest."
  (reverse
   (cl-ppcre:split
    "\\n"
    (with-output-to-string (stream)
      (uiop:run-program (format nil "borg list --format {archive}{NL} ~a" *repo*)
                        :output stream :error nil :ignore-error-status t)))))
;; (borg-list)

(defun borg-diff (&key (new 0) (old 1))
  (let ((b-list (borg-list)))
    (uiop:run-program (format nil "borg diff ~a::~a ~a"
                              *repo*
                              (nth old b-list)
                              (nth new b-list))
                      :output *standard-output*
                      :error *error-output*
                      :ignore-error-status t)))

(defun borg-inspect ()
  (borg-info)
  (log:info (borg-list))
  (borg-diff))

(defun rclone-backup ()
  "Backup with rclone."
  (log:info "Backing up through rclone.")
  (let ((cmd `("rclone" "--verbose" "sync"
                        ,(format nil "~a/" *repo*)
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

  (log:info "Printing borg diff.. before attempting to upload.")
  (borg-inspect)

  (let ((code (rclone-backup)))
    (case code
      (0 (jin.macos-notification:mac-notify!
          "Rclone pushed successfully!"
          :title "Automatic Backup (backup.lisp)"))
      (t (jin.macos-notification:mac-notify!
          "Rclone error. Please read log."
          :title "Automatic Backup (backup.lisp)")
       (log:info "Aborting..")
       (uiop:quit code))))

  (log:info "Succeed! You may now quit the process."))

;; Main function.
(setf (fdefinition 'main) #'backup!)
