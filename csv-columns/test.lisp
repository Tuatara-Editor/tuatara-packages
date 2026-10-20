;;;; csv-columns/test.lisp --- The csv-columns package: columns rearranged.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test csv-columns
  :description "The csv-columns package: columns rearranged, counted from 0; the same in memory and on disk in pieces.")

(defun csv-columns-keys (text)
  "TEXT as the keys that type it, a space as SPC."
  (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) text)))

(defun csv-columns-after (keys text &key on-disk (mode "csv-mode") crlf)
  "The text after typing KEYS on TEXT in MODE, with CRLF its lines ending
with a carriage return and a newline. With ON-DISK, the work goes to disk in
tiny pieces, a few lines each, so that a small text exercises it."
  (flet ((run ()
           (session-text (nth-value 1 (run-keys-in-turns (list (format nil "M-x ~a RET ~a" (csv-columns-keys mode) keys) "")
                                                         :text text :width 100 :crlf crlf)))))
    (if on-disk
        (let ((tuatara.defaults::*rewrite-piece* 16)
              (tuatara.defaults::*rewrite-checkpoint* 8))
          (run))
        (run))))

(defparameter *csv-columns-sample*
  (format nil "NAME,CITY,NOTE~%Ann,Auckland,tea~%Bob,\"Napier, Hawke's Bay\",coffee~%Cat,Nelson,\"said \"\"hi\"\"\"~%Dan,Auckland~%")
  "Records: a quoted field holding the separator, one holding doubled quotes, one short a field.")

(define-test (csv-columns layout-read)
  (is equal '((0) (1) (3) (4 "cent") ("hello")) (csv-columns::parse-layout "$0,$1,$3,$4\"cent\",\"hello\""))
  (is equal '((:last) (:record) (2 " " 0)) (csv-columns::parse-layout "$NF, $* , $2 \" \" $0"))
  (is equal '(("a\"b\\")) (csv-columns::parse-layout "\"a\\\"b\\\\\""))
  (is equal '((0) () (1)) (csv-columns::parse-layout "$0,,$1"))
  (fail (csv-columns::parse-layout "$x"))
  (fail (csv-columns::parse-layout "\"open"))
  (fail (csv-columns::parse-layout "$0 + $1")))

(define-test (csv-columns record-rearranged)
  (flet ((arranged (spec record)
           (let ((new (csv-columns::rearranged-record record (length record) 44 (csv-columns::parse-layout spec))))
             (if (eq new t) record new))))
    ;; five columns: the third left out, cent joined to the fifth's value, hello added
    (is string= "a,b,d,ecent,hello" (arranged "$0,$1,$3,$4\"cent\",\"hello\"" "a,b,c,d,e"))
    ;; a field alone keeps its quotes as written; one joined is quoted anew when it needs it
    (is string= "\"x,y\",\"x,yZ\",\"q\"\"Z\"" (arranged "$1,$1\"Z\",$2\"Z\"" "a,\"x,y\",\"q\"\"\""))
    ;; a field the record lacks is empty; $NF the last; $* the record
    (is string= ",c,a,b,c" (arranged "$9,$NF,$*" "a,b,c"))
    ;; a quoted field joined to text is quoted again only when its value needs it
    (is string= "abcZ,\" xZ\"" (arranged "$0\"Z\",$1\"Z\"" "\"abc\",\" x\""))
    ;; $NF follows each record's own length
    (is string= "e" (arranged "$NF" "a,b,c,d,e"))
    (is string= "a" (arranged "$NF" "a"))
    ;; a byte-order mark stays at the start, no part of the first field
    (is string= (format nil "~cb,a" (code-char #xfeff)) (arranged "$1,$0" (format nil "~ca,b" (code-char #xfeff))))
    ;; a field far past the first ones is still found
    (is string= ",c" (arranged "$100000000,$2" "a,b,c"))))

(define-test (csv-columns rearranged)
  (dolist (on-disk '(nil t))
    ;; every line, the header too
    (is string= (format nil "NOTE,NAME,NOTE!~%tea,Ann,tea!~%coffee,Bob,coffee!~%\"said \"\"hi\"\"\",Cat,\"said \"\"hi\"\"!\"~%,Dan,!~%")
        (csv-columns-after (format nil "C-c r a ~a RET" (csv-columns-keys "$2,$0,$2\"!\"")) *csv-columns-sample* :on-disk on-disk)))
  ;; a CRLF file keeps its line ends
  (is string= (format nil "b,a~a~%d,c~a~%" #\Return #\Return)
      (csv-columns-after "C-c r a $ 1 , $ 0 RET" (format nil "a,b~a~%c,d~a~%" #\Return #\Return) :crlf t)))

(define-test (csv-columns whole-record-joined)
  ;; $* joined to text is one column of the record as written, its quotes doubled
  (is string= "\"\"\"a\"\",\"\"b\"\"x\"" (csv-columns::rearranged-record "\"a\",\"b\"" 7 44 (csv-columns::parse-layout "$*\"x\"")))
  (is string= "\"a,b!\"" (csv-columns::rearranged-record "a,b" 3 44 (csv-columns::parse-layout "$*\"!\""))))

(define-test (csv-columns unchanged-records-kept)
  ;; a record that comes out as it was is kept, not rewritten or counted
  (is eq t (csv-columns::rearranged-record "a,b,c" 5 44 (csv-columns::parse-layout "$0,$1,$2")))
  (is eq t (csv-columns::rearranged-record "a,\"b,c\"" 7 44 (csv-columns::parse-layout "$*")))
  (is string= "b,a" (csv-columns::rearranged-record "a,b" 3 44 (csv-columns::parse-layout "$1,$0")))
  (multiple-value-bind (f editor)
      (run-keys-in-turns (list (format nil "M-x c s v - m o d e RET C-c r a ~a RET" (csv-columns-keys "$0,$1")) "")
                         :text (format nil "a,b~%c,d~%") :width 100)
    (true (tuatara.frontend.fake:fake-frame-showing f "Rearranged 0 lines"))
    (is string= (format nil "a,b~%c,d~%") (session-text editor))))

(define-test (csv-columns quotes-worked-out-once)
  ;; whether a joined column needs quotes is worked out once for each record
  (let ((asked 0) (kept (fdefinition 'csv-columns::column-needs-quotes-p)))
    (setf (fdefinition 'csv-columns::column-needs-quotes-p)
          (lambda (&rest arguments) (incf asked) (apply kept arguments)))
    (unwind-protect
         (is string= "ab!" (csv-columns::rearranged-record "a,b" 3 44 (csv-columns::parse-layout "$0$1\"!\"")))
      (setf (fdefinition 'csv-columns::column-needs-quotes-p) kept))
    (is = 1 asked)))
