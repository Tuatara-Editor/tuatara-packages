;;;; csv-columns/csv-columns.lisp --- The columns of a CSV file rearranged as awk prints them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "csv-columns")
;;;; in the init file.
;;;;
;;;; In a CSV or TSV buffer, C-c r a rearranges the columns of every record
;;;; as awk's print does, columns counted from 0 as csvdt counts them:
;;;; $0,$1,$3,$4"cent","hello" leaves out the third column, puts cent after
;;;; the fifth's value and adds a column holding hello. A record is a line,
;;;; as CSV mode reads one. One step to undo, done in the background on every
;;;; core when the file is large, as keep-lines is. Keeping, deleting,
;;;; showing and hiding records by a column is Tuatara's own (C-x / k, d, f
;;;; and h).

(defpackage #:csv-columns
  (:use #:cl #:tuatara)
  (:documentation "The columns of a CSV file rearranged as awk prints them."))

(in-package #:csv-columns)

;;; Rearranging the columns

(defun parse-layout (spec)
  "The columns SPEC asks for: columns separated by commas, each made of $N
\(field N, counted from 0; $NF the last, $* the whole record) and \"text\"
\(\\\" a quote, \\\\ a backslash inside), side by side to join them. A list of
columns, each a list of its parts: a field's index, :LAST, :RECORD, or a
string. An error says where SPEC is not understood."
  (let ((columns '()) (parts '()) (i 0) (n (length spec)))
    (flet ((fail (what) (error "~a at ~d of ~a" what (1+ i) spec)))
      (loop
        (loop while (and (< i n) (member (char spec i) '(#\Space #\Tab))) do (incf i))
        (when (>= i n)
          (push (nreverse parts) columns)
          (return (nreverse columns)))
        (let ((c (char spec i)))
          (cond ((char= c #\,)
                 (push (nreverse parts) columns)
                 (setf parts '())
                 (incf i))
                ((char= c #\$)
                 (incf i)
                 (cond ((and (<= (+ i 2) n) (string-equal (subseq spec i (+ i 2)) "NF"))
                        (push :last parts)
                        (incf i 2))
                       ((and (< i n) (char= (char spec i) #\*))
                        (push :record parts)
                        (incf i))
                       ((and (< i n) (digit-char-p (char spec i)))
                        (let ((end (or (position-if-not #'digit-char-p spec :start i) n)))
                          (push (parse-integer spec :start i :end end) parts)
                          (setf i end)))
                       (t (fail "A $ needs a number, NF or *"))))
                ((char= c #\")
                 (let ((out (make-string-output-stream)))
                   (incf i)
                   (loop
                     (when (>= i n) (fail "A quote is left open"))
                     (let ((d (char spec i)))
                       (cond ((char= d #\") (incf i) (return))
                             ((and (char= d #\\) (< (1+ i) n))
                              (write-char (char spec (1+ i)) out)
                              (incf i 2))
                             (t (write-char d out) (incf i)))))
                   (push (get-output-stream-string out) parts)))
                (t (fail "Not understood"))))))))

(defun field-span (text from length delimiter spans count part)
  "Where the field PART names (an index, :LAST or :RECORD) stands in the
record TEXT holds from FROM below LENGTH, as written: START and END, both
LENGTH for a field the record lacks. SPANS holds where the record's first
fields are and COUNT how many it has (CSV-FIELD-SPANS)."
  (declare (type (simple-array fixnum (*)) spans) (type fixnum from length count))
  (let ((index (case part (:record -1) (:last (1- count)) (t part))))
    (declare (type fixnum index))
    (cond ((minusp index) (values from length))
          ((>= index count) (values length length))
          ((< (1+ (* 2 index)) (length spans)) (values (aref spans (* 2 index)) (aref spans (1+ (* 2 index)))))
          ;; past what SPANS has room for ($NF of a wide record)
          (t (csv-field-bounds text from length delimiter index)))))

(defun put-value (out at text start end unquote double)
  "Put TEXT from START below END in the string OUT from AT, or only count it
when OUT is NIL: with UNQUOTE a doubled quote as one, with DOUBLE each quote
doubled. Where the next character goes."
  (declare (type (or null (simple-array character (*))) out) (type simple-string text)
           (type fixnum at start end) (optimize speed))
  (if (not (or unquote double))
      (progn (when out (replace out text :start1 at :start2 start :end2 end))
             (+ at (- end start)))
      (let ((i start))
        (declare (type fixnum i))
        (loop while (< i end)
              do (let ((c (schar text i)))
                   (when (and unquote (char= c #\") (< (1+ i) end) (char= (schar text (1+ i)) #\"))
                     (incf i))
                   (when (and double (char= c #\"))
                     (when out (setf (schar out at) #\"))
                     (incf at))
                   (when out (setf (schar out at) c))
                   (incf at)
                   (incf i)))
        at)))

(defun column-needs-quotes-p (text from length delimiter spans count parts)
  "True when the column made of PARTS joined must be quoted: its text holds
the DELIMITER (a character), a quote or a line end, or begins or ends with a
space."
  (let ((first-char nil) (last-char nil))
    (flet ((look (string start end)
             (declare (type simple-string string) (type fixnum start end))
             (when (< start end)
               (unless first-char (setf first-char (schar string start)))
               (setf last-char (schar string (1- end)))
               (loop for i of-type fixnum from start below end
                     for c = (schar string i)
                     thereis (or (char= c delimiter) (char= c #\") (char= c #\Newline) (char= c #\Return))))))
      (or (loop for part in parts
                thereis (cond ((stringp part) (look part 0 (length part)))
                              ((eq part :record) (look text from length))
                              (t (multiple-value-bind (start end)
                                     (multiple-value-call #'csv-value-bounds text (field-span text from length delimiter spans count part))
                                   (look text start end)))))
          (eql first-char #\Space)
          (eql last-char #\Space)))))

(defun field-alone-p (column)
  "True when COLUMN, a column of a layout, is one field alone, written as it is."
  (and column (null (rest column)) (not (stringp (first column)))))

(defun put-record (out text from length delimiter spans count layout quotes &key (at 0))
  "Put the record LAYOUT makes of TEXT from FROM below LENGTH in the string
OUT from AT, or only count it when OUT is NIL (REARRANGED-RECORD); QUOTES,
a bit for each column of LAYOUT, 1 for one to be quoted. Where it ends."
  (declare (type simple-bit-vector quotes))
  (let ((at at))
    (declare (type fixnum at))
    (loop for (column . more) on layout for k of-type fixnum from 0
          do (if (field-alone-p column)
                 ;; one field alone: as it is written
                 (multiple-value-bind (start end) (field-span text from length delimiter spans count (first column))
                   (setf at (put-value out at text start end nil nil)))
                 (let ((quote (= 1 (sbit quotes k))))
                   (when quote (when out (setf (schar out at) #\")) (incf at))
                   (dolist (part column)
                     (setf at (cond ((stringp part) (put-value out at part 0 (length part) nil quote))
                                    ;; the whole record as it is written
                                    ((eq part :record) (put-value out at text from length nil quote))
                                    (t (multiple-value-bind (start end quoted)
                                           (multiple-value-call #'csv-value-bounds text (field-span text from length delimiter spans count part))
                                         (put-value out at text start end quoted quote))))))
                   (when quote (when out (setf (schar out at) #\")) (incf at))))
             (when more
               (when out (setf (schar out at) delimiter))
               (incf at)))
    at))

(defparameter *layout-most-placed* 256
  "How many of a record's first fields are placed at once for a layout; fields
further on are found one at a time.")

(defun layout-width (layout)
  "How many of a record's first fields LAYOUT names by number, to place at
once: one more than the largest index it names, at least one, at most
*LAYOUT-MOST-PLACED*."
  (min *layout-most-placed*
       (1+ (reduce #'max (loop for column in layout append (remove-if-not #'integerp column)) :initial-value 0))))

(defun rearranged-record (text length delimiter layout &optional (width (layout-width layout)))
  "The record TEXT holds in its first LENGTH characters, its columns as
LAYOUT (PARSE-LAYOUT) asks, DELIMITER the code of the separator, WIDTH its
LAYOUT-WIDTH: a column of one field alone is that field as written; one
joining several parts, or a text, is written anew, quoted when it needs to
be (a quote inside doubled); $* there is the record as it is written. A
field the record lacks is empty. A byte-order mark the record begins with,
as a file's first line may, is no part of its first field and stays at its
start. T when the record comes out as it was. The record is read once and
the new one is the only thing made, measured first and then filled, as every
line of a large file is."
  (let ((delimiter (code-char delimiter))
        (spans (make-array (* 2 width) :element-type 'fixnum :initial-element 0))
        (quotes (make-array (length layout) :element-type 'bit :initial-element 0))
        (mark (and (plusp length) (char= (char text 0) (code-char #xfeff)))))
    (declare (dynamic-extent spans quotes))
    (let* ((from (if mark 1 0))
           (count (csv-field-spans text from length delimiter spans)))
      (loop for column in layout for k from 0
            unless (field-alone-p column)
              do (when (column-needs-quotes-p text from length delimiter spans count column)
                   (setf (sbit quotes k) 1)))
      (let ((out (make-string (put-record nil text from length delimiter spans count layout quotes :at from))))
        (when mark (setf (char out 0) (code-char #xfeff)))
        (put-record out text from length delimiter spans count layout quotes :at from)
        (or (and (= (length out) length) (string= out text :end2 length)) out)))))

(define-command csv-rearrange-columns ()
  "Rearrange the columns of every record, the columns asked for counted from
0: $0,$1,$3,$4\"cent\",\"hello\" leaves out the third column, joins cent to
the fifth's value and adds a column holding hello. $N is field N from 0, $NF
the last, $* the whole record (alone, its own columns; joined, one column of
its text as written); \"text\" is text as it is, joined to what stands
beside it. A field alone keeps its quotes as written; joined text is quoted
when it holds the separator or a quote. Records that come out as they were
are left as they are. A field a record lacks is
empty. The header line is rearranged too, like every other. The lines the
selection touches, or every line; one step to undo; a large file is done in
the background."
  (let* ((buffer (current-buffer))
         (spec (read-string "Columns from $0 ($0,$2,$1\"text\"): "))
         (delimiter (csv-delimiter buffer)))
    (multiple-value-bind (start end) (selected-lines buffer)
      (unless start (setf start 0 end (buffer-size buffer)))
      (cond ((string= (string-trim " " spec) "") (message "No columns given: nothing rearranged"))
            ((>= start end) (message "No records to rearrange"))
            (t (let* ((layout (parse-layout spec)) (width (layout-width layout)))
                 (rewrite-lines buffer start end "Rearranging columns"
                                (lambda (text length) (rearranged-record text length delimiter layout width))
                                (lambda (gone changed)
                                  (declare (ignore gone))
                                  (format nil "Rearranged ~:d line~:p" changed)))))))))

;;; Keys

(defun install-csv-columns ()
  "Bind the package's keys in CSV and TSV buffers, under C-c r."
  (define-keys (mode-keymap 'csv-mode) '(("C-c r a" csv-rearrange-columns)))
  (label-keys (mode-keymap 'csv-mode) '(("C-c r" "rearrange")))
  (commands-for-mode 'csv-mode "a CSV or TSV buffer, with the csv-columns package"
                     'csv-rearrange-columns))

(install-csv-columns)
