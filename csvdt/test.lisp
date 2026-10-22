;;;; csvdt/test.lisp --- The csvdt package: arguments, whole records, and runs of a real csvdt.
;;;; Copyright (C) 2025-2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The runs need a csvdt: $CSVDT, else the one on the PATH; without one,
;;;; they are skipped.

(in-package #:tuatara.tests)

(define-test csvdt
  :description "The csvdt package: command lines split as a shell splits them, options read from csvdt itself,
selections widened to whole records, the header carried, and csvdt run over a buffer, a selection and banked lines.")

(defparameter *csvdt-test-program*
  (or (uiop:getenv "CSVDT") (tuatara.io:find-program "csvdt"))
  "The csvdt the tests run, or NIL when there is none.")

(defparameter *csvdt-test-source*
  (merge-pathnames "csvdt.lisp" (or *load-truename* *default-pathname-defaults*))
  "The package's source, beside these tests.")

(defmacro with-csvdt (() &body body)
  "Run BODY with *CSVDT-PROGRAM* the csvdt the tests run, or skip it without one."
  `(if (null *csvdt-test-program*)
       (skip "No csvdt to run: set CSVDT or put csvdt on the PATH" (true nil))
       (let ((csvdt::*csvdt-program* *csvdt-test-program*)
             (csvdt::*csvdt-options-cache* nil)
             (csvdt::*csvdt-history* '()))
         ,@body)))

(defun csvdt-test-rope (text)
  "TEXT as a rope."
  (tuatara.text:string->rope text))

(define-test (csvdt csvdt-split-arguments)
  (flet ((split (line) (csvdt::csvdt-split-arguments line)))
    ;; a comma stays inside its argument
    (is equal '("-H" "-p" "-a" "1,0" "-i" "0,replace") (split "-H -p -a 1,0 -i 0,replace"))
    (is equal '("-o0,+02:00") (split "-o0,+02:00"))
    ;; either quote, each holding what the other cannot
    (is equal '("--print-delimiter" " ") (split "--print-delimiter \" \""))
    (is equal '("--comment" "#") (split "--comment '#'"))
    (is equal '("--print-delimiter" "'") (split "--print-delimiter \"'\""))
    (is equal '("--print-delimiter" "\"") (split "--print-delimiter '\"'"))
    ;; the shell's escapes in double quotes, none in single ones
    (is equal (list (string #\Tab)) (split "\"\\t\""))
    (is equal '("C:\\data") (split "'C:\\data'"))
    (fail (split "\"C:\\data\""))
    ;; a backslash outside quotes is itself
    (is equal '("C:\\Users\\x") (split "C:\\Users\\x"))
    ;; every space a shell splits on, a pasted newline too
    (is equal '("-H" "--arr") (split (format nil "-H~%--arr~%")))
    (fail (split "-H \"open"))))

(define-test (csvdt csvdt-option-lists)
  (let ((records (csvdt::csvdt-parse-option-list
                  (format nil "# csvdt-option-list 1~%# short	long	value	separator	value-name~%-H	--has-header	none	none	~%-f	--flexible	optional	equals	keep~%"))))
    (is = 2 (length records))
    (is equal "--flexible" (getf (second records) :long))
    (is equal "equals" (getf (second records) :separator)))
  ;; a format not known, or no list at all: declined, for --help to be read instead
  (false (csvdt::csvdt-parse-option-list (format nil "# csvdt-option-list 2~%-H	--has-header	none	none~%")))
  (false (csvdt::csvdt-parse-option-list ""))
  ;; the --help fallback: options at the start of a line, the first of each
  (let ((records (csvdt::csvdt-parse-help
                  (format nil "Options:~%  -H, --has-header  first row~%      --trim <T>  how~%      --trim again in prose~%"))))
    (is equal '("--has-header" "--trim") (mapcar (lambda (r) (getf r :long)) records))
    (is equal "-H" (getf (first records) :short))
    (false (getf (first records) :value))))

(define-test (csvdt csvdt-whole-records)
  (let ((rope (csvdt-test-rope (format nil "h,when~%\"x~%y\",2026-07-25T12:00:00+02:00~%z,2026-07-26T12:00:00+02:00~%"))))
    ;; a range starting inside a quoted field is widened back to its record
    (is equal (cons 7 41) (csvdt::csvdt-whole-records rope 10 41 34 44))
    (true (csvdt::csvdt-inside-quoted-field-p rope 10 34 44))
    (false (csvdt::csvdt-inside-quoted-field-p rope 41 34 44))
    ;; a quote not where a field begins is data
    (false (csvdt::csvdt-inside-quoted-field-p (csvdt-test-rope (format nil "a\"b,c~%d,e~%")) 7 34 44)))
  ;; single quotes and another delimiter, as the arguments name them
  (is equal '(39 59) (multiple-value-list (csvdt::csvdt-dialect '("--single-quote" "--read-delimiter" ";"))))
  (is equal '(34 9) (multiple-value-list (csvdt::csvdt-dialect (list (format nil "--read-delimiter=~c" #\Tab)))))
  (is equal '(34 44) (multiple-value-list (csvdt::csvdt-dialect '("-p" "--" "--single-quote"))))
  ;; part of a line takes the whole line; an end at a line's start stops there
  (let ((rope (csvdt-test-rope (format nil "ab,c~%de,f~%gh,i~%"))))
    (is equal (cons 0 10) (csvdt::csvdt-whole-lines rope 2 7))
    (is equal (cons 5 10) (csvdt::csvdt-whole-lines rope 5 10))
    ;; two lines widening to one record are sent once
    (is equal '((0 . 10)) (csvdt::csvdt-drop-covered '((0 . 10) (5 . 10))))))

(define-test (csvdt csvdt-header-options)
  (with-csvdt ()
    (flet ((header-p (line) (csvdt::csvdt-header-requested-p (csvdt::csvdt-split-arguments line))))
      (true (header-p "-H -p"))
      (true (header-p "--has-header"))
      (true (header-p "-Hp"))
      (true (header-p "-pH"))
      ;; an H inside another option's value, or past --, is no header option
      (false (header-p "-u0 -iH"))
      (false (header-p "-p -- -H")))
    ;; every option this package names is one csvdt reports
    (let ((reported (csvdt::csvdt-options)))
      (dolist (option (append csvdt::*csvdt-header-options* csvdt::*csvdt-single-quote-options*
                              csvdt::*csvdt-read-delimiter-options*))
        (true (member option reported :test #'string=) "~a is not an option csvdt reports" option))
      ;; an option whose value must be joined with = is offered with it
      (true (member "--flexible=" reported :test #'string=)))))

(define-test (csvdt csvdt-names-no-other-option)
  ;; options are csvdt's to report: none but the three settings' is named in the package
  (with-csvdt ()
    (let* ((source (alexandria:read-file-into-string *csvdt-test-source*))
           (allowed (append '("--list-options" "--help" "--version")
                            csvdt::*csvdt-header-options* csvdt::*csvdt-single-quote-options*
                            csvdt::*csvdt-read-delimiter-options*)))
      (dolist (option (csvdt::csvdt-options))
        (let ((option (string-right-trim "=" option)))
          (unless (or (member option allowed :test #'string=) (< (length option) 3))
            (false (cl-ppcre:scan (format nil "[^A-Za-z0-9-]~a[^A-Za-z0-9-]" (cl-ppcre:quote-meta-chars option)) source)
                   "~a is named in csvdt.lisp" option)))))))

(defparameter *csvdt-test-text*
  (format nil "ts,x~%2026-07-25T12:00:00+02:00,a~%2026-07-26T12:00:00+02:00,b~%2026-07-27T12:00:00+02:00,c~%")
  "A buffer with a header and a timestamp: without a conversion, -H -p and no
arguments at all would give the same output.")

(defparameter *csvdt-test-utc*
  (format nil "to_utc,x~%2026-07-25T10:00:00+00:00,a~%2026-07-26T10:00:00+00:00,b~%2026-07-27T10:00:00+00:00,c~%")
  "*CSVDT-TEST-TEXT* with its timestamps put in UTC, as -H -p -u0 -i 0,replace gives it.")

(defun csvdt-test-output ()
  "The text of *csvdt output*, once the runs started have ended and been taken in."
  (loop while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*) do (sleep 0.01))
  (let ((out (tuatara.buffer:find-buffer "*csvdt output*")))
    (and out (tuatara.buffer:buffer-string out))))

(defun csvdt-test-forget-output ()
  "Kill *csvdt output*, so that a run is seen to make it."
  (let ((old (tuatara.buffer:find-buffer "*csvdt output*")))
    (when old (tuatara.buffer:kill-buffer old))))

(defun csvdt-test-run (keys &rest options)
  "Run KEYS in a session (RUN-KEYS-IN-TURNS takes OPTIONS), *csvdt output*
killed first; the frontend, the editor and the output."
  (csvdt-test-forget-output)
  (multiple-value-bind (f editor) (apply #'run-keys-in-turns (list keys) :text *csvdt-test-text* options)
    (values f editor (csvdt-test-output))))

(define-test (csvdt csvdt-run-buffer)
  (with-csvdt ()
    ;; the arguments offered, selected, typed over
    (multiple-value-bind (f editor out) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
      (declare (ignore editor))
      (is string= *csvdt-test-utc* out)
      (true (tuatara.frontend.fake:fake-frame-showing f "csvdt -H -p -u0 -i 0,replace: 4 lines")))
    ;; RET runs the arguments offered as they are
    (multiple-value-bind (f editor out) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET RET")
      (declare (ignore f editor))
      (is string= *csvdt-test-text* out))
    ;; the right arrow keeps them, to add to
    (multiple-value-bind (f editor out) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET <right> SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
      (declare (ignore f editor))
      (true (search "10:00:00+00:00" out)))
    ;; the source is never changed
    (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET - H SPC - u 0 RET")
    (is string= *csvdt-test-text* (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*")))))

(define-test (csvdt csvdt-keys-in-csv-mode)
  (with-csvdt ()
    ;; C-c C-v r in a CSV buffer runs over the whole of it
    (multiple-value-bind (f editor out)
        (csvdt-test-run "C-c C-v r - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET" :mode 'tuatara:csv-mode)
      (declare (ignore f editor))
      (is string= *csvdt-test-utc* out)
      (true (typep (tuatara:buffer-major-mode (tuatara.buffer:find-buffer "*csvdt output*")) 'tuatara:csv-mode)))))

(define-test (csvdt csvdt-completion-and-history)
  (with-csvdt ()
    ;; TAB completes the option before the cursor from those csvdt reports
    (multiple-value-bind (f editor out) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET - - h a s TAB SPC - p RET")
      (declare (ignore editor))
      (is string= *csvdt-test-text* out)
      (true (tuatara.frontend.fake:fake-frame-showing f "csvdt --has-header -p: 4 lines")))
    ;; M-p brings back the line run before
    (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
    (multiple-value-bind (f editor out) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET M-p RET")
      (declare (ignore f editor))
      (is string= *csvdt-test-utc* out))
    (is equal "-H -p -u0 -i 0,replace" (first csvdt::*csvdt-history*))))

(define-test (csvdt csvdt-run-region-carries-header)
  (with-csvdt ()
    ;; from the middle of the first row to the middle of the second: both rows, whole, the header in front
    (multiple-value-bind (f editor out)
        (csvdt-test-run "M-x c s v d t - r u n - r e g i o n RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET" :point 12 :mark 40)
      (declare (ignore editor))
      (is string= (format nil "to_utc,x~%2026-07-25T10:00:00+00:00,a~%2026-07-26T10:00:00+00:00,b~%") out)
      (true (tuatara.frontend.fake:fake-frame-showing f "selection widened to whole lines")))
    ;; without a header asked for, nothing is put in front
    (multiple-value-bind (f editor out)
        (csvdt-test-run "M-x c s v d t - r u n - r e g i o n RET - u 0 SPC - i SPC 0 , r e p l a c e RET" :point 33 :mark 61)
      (declare (ignore f editor))
      (is string= (format nil "2026-07-26T10:00:00+00:00,b~%") out))))

(define-test (csvdt csvdt-crlf)
  (with-csvdt ()
    ;; a buffer ending its lines with CR LF reaches csvdt with LF alone
    (csvdt-test-forget-output)
    (run-keys-in-turns (list "M-x c s v d t - r u n - b u f f e r RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
                       :text (format nil "ts,x~a~%2026-07-25T12:00:00+02:00,a~a~%" #\Return #\Return) :crlf t)
    (is string= (format nil "to_utc,x~%2026-07-25T10:00:00+00:00,a~%") (csvdt-test-output))))

(define-test (csvdt csvdt-run-banked)
  (with-csvdt ()
    (csvdt-test-forget-output)
    ;; the first and third rows banked: sent as one file, the header in front, the row between left out
    (multiple-value-bind (f editor)
        (run-keys (list (lambda ()
                          (let ((b (tuatara.command:current-buffer)))
                            (tuatara.defaults::bank-lines b (cons 5 33))
                            (tuatara.defaults::bank-lines b (cons 61 89))))
                        "M-x c s v d t - r u n - b a n k e d RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
                  :text *csvdt-test-text* :width 120)
      (declare (ignore editor))
      (is string= (format nil "to_utc,x~%2026-07-25T10:00:00+00:00,a~%2026-07-27T10:00:00+00:00,c~%") (csvdt-test-output))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 lines from 2 stretches"))
      ;; the lines stay banked
      (is = 2 (length (tuatara:banked-spans (tuatara.buffer:find-buffer "*scratch*")))))
    ;; dwim takes the banked lines and the selection together, a line in both sent once
    (multiple-value-bind (f editor)
        (run-keys (list (lambda ()
                          (let ((b (tuatara.command:current-buffer)))
                            (tuatara.defaults::bank-lines b (cons 5 33))
                            (setf (tuatara.buffer:buffer-mark b) 20 (tuatara.buffer:buffer-point b) 40
                                  (tuatara.buffer:buffer-mark-active-p b) t)))
                        "M-x c s v d t - r u n - d w i m RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
                  :text *csvdt-test-text* :width 120)
      (declare (ignore f editor))
      (is string= (format nil "to_utc,x~%2026-07-25T10:00:00+00:00,a~%2026-07-26T10:00:00+00:00,b~%") (csvdt-test-output)))
    ;; describe-run counts what a run would cover without running
    (multiple-value-bind (f editor)
        (run-keys (list (lambda () (tuatara.defaults::bank-lines (tuatara.command:current-buffer) (cons 5 33)))
                        "M-x c s v d t - d e s c r i b e - r u n RET")
                  :text *csvdt-test-text* :width 120)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "csvdt would run over 1 line in 1 stretch")))))

(define-test (csvdt csvdt-output-buffer)
  (with-csvdt ()
    ;; a run over *csvdt output* itself: its banks let go, its text replaced, still writable
    (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET RET")
    (let ((out (tuatara.buffer:find-buffer "*csvdt output*")))
      (tuatara.defaults::bank-lines out (cons 5 33))
      (run-keys (list (lambda () (tuatara.command:show-buffer out))
                      "M-x c s v d t - r u n - b u f f e r RET - H SPC - p SPC - u 0 SPC - i SPC 0 , r e p l a c e RET")
                :width 120)
      (is string= *csvdt-test-utc* (csvdt-test-output))
      (is eq out (tuatara.buffer:find-buffer "*csvdt output*"))
      (false (tuatara:buffer-banks out))
      (finish (tuatara:check-writable out)))))

(define-test (csvdt csvdt-refusals)
  (with-csvdt ()
    ;; a file named among the arguments would take the lines' place
    (with-test-directory (dir)
      (let ((file (namestring (merge-pathnames "sales.csv" dir))))
        (alexandria:write-string-into-file "a,b" file)
        (let ((b (tuatara.buffer:make-buffer "refused" :text "a,b")))
          (unwind-protect (fail (csvdt::csvdt-run b '((0 . 3)) (list "-H" file)))
            (tuatara.buffer:kill-buffer b)))))
    ;; a value apart from its option is no file, though a file of that name is there
    (with-test-directory (dir)
      (alexandria:write-string-into-file "a,b" (merge-pathnames "7" dir))
      (is equal '() (csvdt::csvdt-file-arguments '("-u" "7") (namestring dir)))
      (is equal '("7") (csvdt::csvdt-file-arguments '("-H" "7") (namestring dir))))
    ;; csvdt's own reason, with its exit status
    (multiple-value-bind (f editor) (csvdt-test-run "M-x c s v d t - r u n - b u f f e r RET - H SPC - u 9 RET")
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "csvdt exited")))
    ;; a program that is not there says which setting to set
    (let ((csvdt::*csvdt-program* "/no/such/csvdt"))
      (fail (csvdt::csvdt-program "/")))))

(defun csvdt-test-take-in-consed (rows)
  "Octets consed per octet of output when a run of csvdt over ROWS rows is
put in *csvdt output* and its lines counted, as its take-in does; and the
line total."
  (let* ((text (with-output-to-string (s)
                 (format s "ts,x~%")
                 (dotimes (i rows) (format s "2026-07-25T12:00:00+02:00,row ~d~%" i))))
         (b (tuatara.buffer:make-buffer "csvdt take-in" :text text)))
    (unwind-protect
         (let ((job (csvdt::make-csvdt-job
                     :buffer b :program *csvdt-test-program* :directory "/tmp/" :arguments (list "-H" "-p")
                     :rope (tuatara.buffer:buffer-text b) :ranges (list (cons 0 (tuatara.buffer:buffer-size b))))))
           (csvdt::csvdt-work job)
           (let* ((before (sb-ext:get-bytes-consed))
                  (out (csvdt::csvdt-fill-output (csvdt::csvdt-job-output job)))
                  (lines (csvdt::csvdt-line-total (csvdt::csvdt-job-output job))))
             (values (/ (- (sb-ext:get-bytes-consed) before) (max 1 (tuatara.buffer:buffer-size out)))
                     lines)))
      (tuatara.buffer:kill-buffer b))))

(define-test (csvdt csvdt-take-in-conses-little)
  ;; the output reaches *csvdt output* as csvdt wrote it, never made a string on the editor's thread
  (with-csvdt ()
    ;; once before, for what entering the mode costs only the first time
    (csvdt-test-take-in-consed 100)
    (multiple-value-bind (small small-lines) (csvdt-test-take-in-consed 20000)
      (multiple-value-bind (large large-lines) (csvdt-test-take-in-consed 80000)
        (is = 20001 small-lines)
        (is = 80001 large-lines)
        (true (< small 1.8) "~,3f octets consed per octet of output, 20,000 rows" small)
        (true (< large 1.8) "~,3f octets consed per octet of output, 80,000 rows" large)))))
