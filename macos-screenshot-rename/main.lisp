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
  (if (parse (pathname-name file))
      (rename-file file (new-name file))
      (log:info "Skipped due to unexpected format: ~a~%" file)))

(defun screenshot-rename-main-fn (free-args options)
  (declare (ignore options))
  (let ((files (cdr free-args)))
    (loop for file in files do
      (screenshot-rename! file))))

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
