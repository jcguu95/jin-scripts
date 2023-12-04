(in-package :jin.macos-screenshot-rename)

;; A typical screenshot in macos has its name in the format, for
;; example,
;;
;;   "Screen Shot 2022-01-07 at 20.17.08 .png",
;;   "Screen Shot 2022-11-07 at 09.17.08 .png".
;;
;; This program renames them to
;;
;;   "20220107-201708.png"
;;   "20221107-091708.png".

;; TODO Make the program safer (e.g. backups, logs .. etc)

;; TODO Compress screenshots with `sips`, `pngquant`,
;; `png->jpeg`. .. after making this a subcomponent of a larger
;; system. ... First back up a file `whatever.png` to
;; `whatever.${now}.backup.png`.

(defun parse (input)
  "
\"Screen Shot 2022-01-07 at 20.17.08 .png\"   -> (2022  1 7 20 17 8)
\"Screen Shot 2022-11-07 at 09.17.08 .png\"   -> (2022 11 7  9 17 8)
\"Screen Shot 2022-13-07 at 09.17.08 .png\"   -> NIL
\"Screen Shto 2022-11-07 at 09.17.08 .png\"   -> NIL
"
  (check-type input string)
  (let* ((date   (subseq input 12 22))
         (time   (subseq input 26 34))
         (year   (read-from-string (subseq date 0 4)))
         (month  (read-from-string (subseq date 5 7)))
         (day    (read-from-string (subseq date 8 10)))
         (hour   (read-from-string (subseq time 0 2)))
         (minute (read-from-string (subseq time 3 5)))
         (second (read-from-string (subseq time 6 8)))
         (expected-format-p
           (and (string= "Screen Shot " (subseq input 0 12))
                (string= " at "         (subseq input 22 26))
                (<= 0 year   2100)
                (<= 1 month  12)
                (<= 1 day    31)
                (<= 0 hour   23)
                (<= 0 minute 59)
                (<= 0 second 59))))
    (when expected-format-p
      (list year month  day
            hour minute second))))

(defun new-name<-parse (parse)
  (when parse
    (destructuring-bind (year month day hour minute second)
        parse
      (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
              year month  day
              hour minute second))))

(defun new-name (file)
  (format nil "~a.~a"
          (new-name<-parse (parse (pathname-name file)))
          (pathname-type file)))

(defun screenshot-rename! (file)
  (if (ignore-errors (parse (pathname-name file)))
      (rename-file file (new-name file))
      (log:info "Skip file due to unexpected format:~%  file = ~a~%" file)))

(defun screenshot-rename-main-fn (free-args options)
  (declare (ignore options))
  (let ((files (cdr free-args)))
    (loop for file in files do
      (screenshot-rename! file))))

(setf (fdefinition 'main) #'screenshot-rename-main-fn)

;;; Tests

(assert
 (and (equal (parse "Screen Shot 2022-11-07 at 20.17.08 .png")
             (list 2022 11 7 20 17 8))
      (equal (parse "Screen Shot 2022-01-07 at 09.17.08 .png")
             (list 2022  1 7  9 17 8))))
(assert
 (and (string= "20221107-201708"
               (new-name<-parse '(2022 11 7 20 17 8)))
      (string= "00010101-010101"
               (new-name<-parse '(   1  1 1  1  1 1)))))
(assert
 (string= "20221107-201708.png"
          (new-name "~/Screen Shot 2022-11-07 at 20.17.08 .png")))

;;;; TODO Iphone Photos Renamer (Under construction)
;;;; The main use case of this file can be summarized in the
;;;; following test case:
;;;;
;;;;   (mapcar
;;;;    (lambda (cons)
;;;;      (assert (string= (car cons) (parse (cdr cons)))))
;;;;    `(("0302-080649"    . "Mar 02, 8 06 49 AM")
;;;;      ("0302-200649"    . "Mar 02, 8 06 49 PM")
;;;;      ("0306-154906-1"  . "Mar 06, 3 49 06 PM (1)")
;;;;      ("0306-154906-2"  . "Mar 06, 3 49 06 PM (2)")
;;;;      ("0306-154906-3"  . "Mar 06, 3 49 06 PM (3)")
;;;;      ("0306-154906-4"  . "Mar 06, 3 49 06 PM (4)")
;;;;      ("0306-154906-10" . "Mar 06, 3 49 06 PM (10)")
;;;;      ("0306-154906-99" . "Mar 06, 3 49 06 PM (99)")
;;;;      ("0307-114149"    . "Mar 07, 11 41 49 AM")
;;;;      ("0307-234149"    . "Mar 07, 11 41 49 PM")))
;;;;
;;;; For example,
;;;;   (string= "0302-154906-4" (parse "Mar 02, 3 49 06 PM (4)"))
;;;;
;;;;
;;;; This file contains a side-effective function #'rename!! I used
;;;; it on [2023-08-05 Sat] to bulk rename my iphone photos.
;;;;
;;;;   (let ((dir #P"/Users/jin/tmp/"))
;;;;     (iter:iter (iter:for file in (cl-fad:list-directory dir))
;;;;       (rename!! file :dry-run nil)))
;;;;
;;;; Notice that iphone namings do not contain their year. So I
;;;; have to manually fix the year.

;; (defun parse (str)
;;   (let* ((tokens (cl-ppcre:split " " str))
;;          (month  (nth 0 tokens))
;;          (day    (nth 1 tokens))
;;          (hour   (nth 2 tokens))
;;          (minute (nth 3 tokens))
;;          (second (nth 4 tokens))
;;          (am/pm  (nth 5 tokens))
;;          (number (nth 6 tokens)))
;;     ;; process month
;;     (setf month
;;           (case (intern (string-upcase month))
;;             (JAN 1) (FEB 2) (MAR 3) (APR 4)  (MAY 5)  (JUN 6)
;;             (JUL 7) (AUG 8) (SEP 9) (OCT 10) (NOV 11) (DEC 12)))
;;     ;; process day
;;     (setf day (parse-integer (car (cl-ppcre:split "," day))))
;;     ;; process hour
;;     (setf hour (parse-integer hour))
;;     (setf hour (if (= hour 12) 0 hour))
;;     (cond ((string= am/pm "PM")
;;            (setf hour (+ 12 hour)))
;;           ((not (string= am/pm "AM"))
;;            (error "Parse error: AM/PM.")))
;;     ;; process minute
;;     (setf minute (parse-integer minute))
;;     ;; process second
;;     (setf second (parse-integer second))
;;     ;; process number
;;     (if number
;;         (setf number (format nil "-~a" (parse-integer (string-trim `(#\( #\)) number))))
;;         (setf number ""))
;;     (log:debug month day hour minute second number)
;;     (format nil "~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d~a" month day hour minute second number)))

;; ;; TEST
;; ;; Test cases
;; (mapcar
;;  (lambda (cons)
;;    (assert (string= (car cons) (parse (cdr cons)))))
;;  `(("0102-080649"    . "Jan 02, 8 06 49 AM")
;;    ("0202-200649"    . "Feb 02, 8 06 49 PM")
;;    ("0306-154906-1"  . "Mar 06, 3 49 06 PM (1)")
;;    ("0406-154906-2"  . "Apr 06, 3 49 06 PM (2)")
;;    ("0506-154906-3"  . "May 06, 3 49 06 PM (3)")
;;    ("0606-154906-4"  . "Jun 06, 3 49 06 PM (4)")
;;    ("0706-154906-10" . "Jul 06, 3 49 06 PM (10)")
;;    ("0806-154906-99" . "Aug 06, 3 49 06 PM (99)")
;;    ("0907-114149"    . "Sep 07, 11 41 49 AM")
;;    ("1007-234149"    . "Oct 07, 11 41 49 PM")
;;    ("1107-114149"    . "Nov 07, 11 41 49 AM")
;;    ("1207-234149"    . "Dec 07, 11 41 49 PM")))

;; (defun main-transformer (str)
;;   (parse (subseq str (1+ (cl-ppcre:scan " " str)))))

;; (setf (fdefinition 'main) #'main-transformer)

;; ;; TEST
;; (assert (string= "1207-234149-10"
;;                  (main "Photo Dec 07, 11 41 49 PM (10)")))

;; ;; (cl-fad:list-directory #P"/Users/jin/memories_organizing/2020/10/") ; TODO How to make it recursive?

;; ;; The main side-effective function.
;; (defun rename!! (file &key (dry-run t))
;;   "The main side-effective function. Use with care!"
;;   (let* ((pathname (file-namestring file))
;;          (name (pathname-name pathname))
;;          (type (pathname-type pathname))
;;          (new-name (format nil "~a.~a" (main-transformer name) type)))
;;     (if dry-run
;;         (log:info file new-name "Turn off dry-run to execute the renaming.")
;;         (rename-file file new-name))))

;; ;; TEST
;; ;; Side-effective test for the side-effective function #'RENAME!! .
;; (let ((root-dir (format nil  "/tmp/lisp-renamer-test-~a~a" (random 100000) (random 100000)))
;;       (old-name "Photo Dec 12, 3 04 05 PM (7).jpg")
;;       (new-name "1212-150405-7"))
;;   (progn
;;     (uiop:run-program (format nil "mkdir -p ~a/" root-dir)
;;                       :output *standard-output*
;;                       :error-output *standard-output*)
;;     (uiop:run-program (format nil "touch \"~a/~a\"" root-dir old-name)
;;                       :output *standard-output*
;;                       :error-output *standard-output*)
;;     (log:info "Testing dry-run: T.")
;;     (rename!! (pathname (format nil "~a/~a" root-dir old-name)))
;;     (log:info "Testing dry-run: NIL.")
;;     (rename!! (pathname (format nil "~a/~a" root-dir old-name)) :dry-run nil)
;;     (let ((files (cl-fad:list-directory (pathname root-dir))))
;;       (assert (= 1 (length files)))
;;       (assert (string= new-name (pathname-name (car files))))
;;       (assert (string= "jpg"    (pathname-type (car files)))))
;;     (uiop:run-program (format nil "rm -rf ~a/" root-dir)
;;                       :output *standard-output*
;;                       :error-output *standard-output*)
;;     (log:info "Success.")))
