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

(defparameter *rclone-repo-name* "gdrive-jcguu95")

(defparameter *local-mount-point*
  (uiop:native-namestring "~/.backup"))

(defparameter *repo* (format nil "~a/~a" *local-mount-point* *name*))

(defparameter *exclude-entries*
  (cons
   (concatenate 'string (uiop:native-namestring "~/Desktop") "/Screenshot*")
   (mapcar
    #'(lambda (path-string)
        (concatenate 'string path-string "/*"))
    (mapcar #'uiop:native-namestring
            `(,*local-mount-point*
              ;; "~/Desktop"
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
              "~/.python"

              "~/.cache"
              "~/.emacs.d/.local/cache"
              "~/.+PLUGS/Dropbox/.dropbox.cache"

              "~/.+PLUGS/tilde-local/stow/zcy"
              "~/.+PLUGS/tilde-local/stow/to-watch")))))

;;; Util

(defun timestamp ()
  (local-time:format-timestring
   nil (local-time:now) :format
   '((:year 4) (:month 2) (:day 2) "-" (:hour 2) (:min 2) (:sec 2))))

;;;

(defun borg-create (comment exclude-entries &key (timestamp (timestamp)))
  "Create deduplicated backups with borg."
  (log:info "Creating backup in borg..")
  (log:info exclude-entries)
  (let* ((archive-name (format nil "~a-~a" (uiop:hostname) timestamp))
         (archive (format nil "~a::~a" *repo* archive-name))
         (target (uiop:native-namestring "~/"))
         (cmd `("borg" "create"
                "--info" "--json" "--show-rc"
                "--progress" "--exclude-caches"
                "--comment" ,(format nil "~a" comment)
                ,@(loop for entry in exclude-entries
                        collect "--exclude"
                        collect (format nil "~a" entry))
                ,archive ,target)))
    (log:info cmd)
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

(defun borg-prune (&key (second 2) (hour 24) (day 7) (week 4) (month 24) (year 5))
  "Prune redundant borg archives."
  ;; FIXME This won't size down the archive. Use `borg compact` for that.
  (log:info "Pruning in borg archive..")
  (let ((cmd `("borg" "prune"
               "--list" "--show-rc"
               "--stats"
               "--keep-secondly" ,(format nil "~d" second) ; keep 2 so we can print a sane borg diff later
               "--keep-hourly"   ,(format nil "~d" hour)
               "--keep-daily"    ,(format nil "~d" day)
               "--keep-weekly"   ,(format nil "~d" week)
               "--keep-monthly"  ,(format nil "~d" month)
               "--keep-yearly"   ,(format nil "~d" year)
               ,*repo*)))
    (log:info cmd)
    (nth 2 (multiple-value-list
            (uiop:run-program cmd :output *standard-output*
                                  :error-output *error-output*
                                  :ignore-error-status t)))))

(defun borg-compact ()
  "Compactify borg archive."
  (log:info "Compacting borg archive..")
  (let ((cmd `("borg" "compact" ,*repo*)))
    ;; TODO log the actual command to run as well
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

;; FIXME This only works on macOS.
(defun check-wifi-status ()
  "Executes a Python script using CoreWLAN to reliably determine the current Wi-Fi status on macOS.
   The Python subprocess handles the CoreWLAN dependency check.

   Returns:
     :HOTSPOT    - Connected to a Personal Hotspot.
     :WIFI       - Connected to a standard Wi-Fi network.
     :NOT-WIFI   - Not connected to any Wi-Fi network.
     :NO-DEP     - The required Python package (pyobjc-framework-CoreWLAN) is missing.
     :UNKNOWN    - The Python process failed to execute."
  (let* (
         (python-code
           ;; The Python script is constructed as a single string.
           ;; It uses try/except blocks to print UNKNOWN if the CoreWLAN package is missing.
           "import sys
try:
    from CoreWLAN import CWInterface
    
    # Get the active Wi-Fi interface
    interface = CWInterface.interface()
    
    if interface is None:
        # Wi-Fi is likely disabled or the interface is not ready
        print('NOT_WIFI')
        sys.exit(0)

    # Get the last network joined (None if disconnected)
    network = interface.lastNetworkJoined()
    
    if network is None:
        print('NOT_WIFI')
    elif network.isPersonalHotspot():
        print('HOTSPOT')
    else:
        print('WIFI')
        
except ImportError:
    # Explicitly catch the missing dependency (pyobjc-framework-CoreWLAN)
    print('NO_DEP')
    
except Exception as e:
    # Catch any other runtime error during CoreWLAN access
    # You might log 'e' here for debugging, but we return UNKNOWN as requested.
    print('UNKNOWN')
")
         (result-string (handler-case
                            ;; Run the Python interpreter and execute the code string
                            ;; :ignore-error-status t prevents Lisp from crashing if Python exits non-zero
                            (uiop:run-program
                             (list "python3" "-c" python-code)
                             :output :string
                             :error-output :interactive
                             :ignore-error-status t)
                          ;; Catch failure to launch python3 itself (e.g., python3 not in PATH)
                          (error (c)
                            (format *error-output* "~&[Warning] Failed to execute python3: ~A~%" c)
                            "UNKNOWN\n")))
    
         (cleaned-result (string-trim '(#\Newline #\Return #\Space) result-string)))

    ;; Map the clean Python output string to the required Common Lisp keyword
    (cond
      ((string= cleaned-result "HOTSPOT") :HOTSPOT)
      ((string= cleaned-result "WIFI") :WIFI)
      ((string= cleaned-result "NOT_WIFI") :NOT-WIFI)
      ((string= cleaned-result "NO_DEP")
       (format *error-output* "[ERROR] Please consider installing the python package by `pip3 install pyobjc-framework-CoreWLAN`.")
       :NO-DEP)
      ;; Any other output, including "UNKNOWN" from Python or execution failure
      (t :UNKNOWN)))) 

;; FIXME Assumes SBCL.
(defun prompt-for-confirmation (prompt)
  "Prints a prompt and reads user input. Returns T if the user confirms (y/Y), 
   and NIL otherwise (default option is [N]). Assumes *query-io* is available."
  (format *query-io* "~A [y/N]: " prompt)
  (finish-output *query-io*)
  (let ((input (sb-sys:with-deadline (:seconds 60)
                 (read-line *query-io* nil ""))))
    (unless (string-equal input "")
      (char-equal (char input 0) #\y))))

(defun rclone-backup ()
  "Backup with rclone."
  (log:info "Backing up through rclone.")
  (let ((wifi-status (check-wifi-status))
        (cmd `("rclone" "--verbose" "sync"
               ,(format nil "~a/" *repo*)
               ,(format nil "~a:~a/" *rclone-repo-name* *name*))))
    (log:info cmd wifi-status)
    (if (or (eq wifi-status :WIFI)
            (prompt-for-confirmation "Cannot detect WI-FI. Confirm to proceed backing up with rclone? "))
        (nth 2 (multiple-value-list
                (uiop:run-program cmd :output *standard-output*
                                      :error-output *error-output*
                                      :ignore-error-status t)))
        (log:info "Skip backing up with rclone."))))

(defun backup! (free-args options)
  (declare (ignore free-args options))

  (let ((code (borg-create "Automatic Backup" *exclude-entries* :timestamp (timestamp))))
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

  (borg-compact)

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
