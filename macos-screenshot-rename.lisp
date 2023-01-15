(in-package :jin-scripts)

;; A typical screenshot in macos has its name in the e.g. format
;;
;;   "Screen Shot 2022-01-07 at 20.17.08 .png".
;;   "Screen Shot 2022-11-07 at 09.17.08 .png".
;;
;; This program aims to rename them to
;;
;;   "20220107-201708.png"
;;   "20221107-091708.png"

;; TODO If filename is not in the expected format, emit warning
;; and pass it.

;; TODO Make the program safer (e.g. backups, logs .. etc)

;; TODO Compress screenshots with `sips`, `pngquant`, `png->jpeg`.


(defun parse (input)
  "
\"Screen Shot 2022-01-07 at 20.17.08 .png\"   -> (2022  1 7 20 17 8)
\"Screen Shot 2022-11-07 at 09.17.08 .png\"   -> (2022 11 7  9 17 8)
"
  (check-type input string)
  (assert (string= "Screen" (subseq input 0 6)))
  (assert (string= "Shot"   (subseq input 7 11)))
  (let* ((date-string (subseq input 12 22))
         (time-string (subseq input 26 34))
         (year        (read-from-string (subseq date-string 0 4)))
         (month       (read-from-string (subseq date-string 5 7)))
         (day         (read-from-string (subseq date-string 8 10)))
         (hour        (read-from-string (subseq time-string 0 2)))
         (minute      (read-from-string (subseq time-string 3 5)))
         (second      (read-from-string (subseq time-string 6 8))))
    (assert (and (<= 0 year   2100)
                 (<= 1 month  12)
                 (<= 1 day    31)
                 (<= 0 hour   23)
                 (<= 0 minute 59)
                 (<= 0 second 59)))
    (list year month day
          hour minute second)))

(assert
 (and (equal (parse "Screen Shot 2022-11-07 at 20.17.08 .png")
             (list 2022 11 7 20 17 8))
      (equal (parse "Screen Shot 2022-01-07 at 09.17.08 .png")
             (list 2022  1 7  9 17 8))))

(defun new-name<-parse (parse)
  (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
          (nth 0 parse) (nth 1 parse) (nth 2 parse)
          (nth 3 parse) (nth 4 parse) (nth 5 parse)))

(assert
 (and (string= "20221107-201708"
               (new-name<-parse '(2022 11 7 20 17 8)))
      (string= "00010101-010101"
               (new-name<-parse '(   1  1 1  1  1 1)))))

(defun new-name (file)
  (format nil "~a.~a"
          (new-name<-parse (parse (pathname-name file)))
          (pathname-type file)))

(string= "20221107-201708.png"
         (new-name "~/Screen Shot 2022-11-07 at 20.17.08 .png"))

(defun screenshot-rename! (file)
  (rename-file file (new-name file)))

(defun screenshot-rename-main-fn (free-args options)
  (declare (ignore options))
  (let ((files (cdr free-args)))
    (loop for file in files do
      (screenshot-rename! file))))
