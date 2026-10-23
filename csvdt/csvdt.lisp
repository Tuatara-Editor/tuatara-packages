;;;; csvdt/csvdt.lisp --- csvdt run over a buffer, a selection or banked lines, its result in a buffer of its own.
;;;; Copyright (C) 2025-2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "csvdt") in
;;;; the init file. It drives csvdt (github.com/YardQuit/csvdt), a program of
;;;; its own that converts the timestamps and durations of CSV records.
;;;;
;;;; Almost nothing about csvdt's options is written down here. The help is
;;;; csvdt's own --help, and the options offered for completion come from
;;;; csvdt --list-options, which csvdt prints from its own definitions: a
;;;; csvdt that gains an option is usable here at once. Three are named, in
;;;; settings, because a run over part of a buffer cannot be worked out
;;;; without them: the one saying the first record is a header, and the two
;;;; setting the quote and the delimiter, since where a record begins depends
;;;; on both.
;;;;
;;;; The lines go to csvdt on its standard input, whole records only, from a
;;;; job in the background (M-x stop-job stops it); what it writes goes to
;;;; *csvdt output*, and the source buffer is never changed. Nothing is run
;;;; through a shell.

(defpackage #:csvdt
  (:use #:cl #:tuatara)
  (:documentation "csvdt run over a buffer, a selection or banked lines, its result in a buffer of its own."))

(in-package #:csvdt)

;;; Settings

(define-package-setting *csvdt-program* "csvdt"
  "The csvdt program: a name, looked for on the PATH, or a path. A path with a
slash in it means that file and is never looked for on the PATH; a relative
one, \"./target/release/csvdt\", is taken from the directory of the buffer
the command is run in.")

(define-package-setting *csvdt-default-arguments* "-H -p"
  "The arguments a csvdt command line starts with, selected: typing replaces
them, \\<minibuffer>\\[minibuffer-forward-char] keeps them to add to, \\[minibuffer-done] runs them as they
are. The default is for a file with a header row, the header printed.

They are selected rather than only offered because the two mistakes are not
alike. Leaving the header option off a file that has a header is loud: the
header comes back as parse_err. Giving it to a file that has none is quiet:
the first row is taken for a header and, by where the conversion puts its
column, either goes through unconverted or is lost. Arguments to be noticed
and deleted point at the quiet mistake; arguments to be kept do not.")

(define-package-setting *csvdt-region-carries-header* t
  "True to put the buffer's header line in front of a selection or banked
lines below it, when the arguments ask for a header. Without it csvdt takes
the first selected row for the header, quietly: a header is exempt from the
conversion, so that row goes through unconverted, or, where the conversion
replaces its column, is lost. The header options are
*CSVDT-HEADER-OPTIONS*.")

(define-package-setting *csvdt-header-options* '("-H" "--has-header")
  "The csvdt options saying the first record is a header, which
*CSVDT-REGION-CARRIES-HEADER* has to recognise. Should csvdt rename them,
name the new ones here.")

(define-package-setting *csvdt-single-quote-options* '("--single-quote")
  "The csvdt options making ' the quote character. Where a record begins
depends on which character quotes a field, so a run told to read single
quotes has its records in other places.")

(define-package-setting *csvdt-read-delimiter-options* '("--read-delimiter")
  "The csvdt options setting the delimiter fields are read with. A quote only
opens a field where a field begins, and where a field begins depends on the
delimiter.")

(define-package-setting *csvdt-output-mode* 'csv-mode
  "The major mode of *csvdt output*. A mode that cannot be entered leaves the
buffer in fundamental-mode rather than losing a run csvdt has done.")

(defparameter *csvdt-output-name* "*csvdt output*"
  "The name of the buffer csvdt's output is shown in.")

(defparameter *csvdt-wait* 1/2
  "Seconds a run waits for csvdt before going on in the background.")

(defparameter *csvdt-piece* (* 1024 1024)
  "Octets read from the text and written to csvdt at a time.")

(defconstant +csvdt-option-list-format+ 1
  "The version of csvdt's --list-options format this package reads. csvdt
prints it on the first line; another means the fields may not mean what is
assumed here, and the list is read from --help instead.")

(defvar *csvdt-options-cache* nil
  "The options of the csvdt last asked, as (PROGRAM STAMP VERSION . RECORDS):
PROGRAM its real path, STAMP when it was written. A rebuilt csvdt has
another stamp and is asked again; one unchanged is not run again to find
that out. VERSION is its --version line, asked at the same time.")

(defvar *csvdt-history* '()
  "The csvdt command lines run, the latest first, gone back through with
\\<minibuffer>\\[minibuffer-previous-history] while csvdt arguments are read.")

;;; The program

(defun csvdt-space-p (char)
  "True when CHAR separates arguments in a csvdt command line: a space, a tab,
a line end, a form feed or a vertical tab."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))))

(defun csvdt-program (&optional directory)
  "The full path of the csvdt program (*CSVDT-PROGRAM*), a relative path taken
from DIRECTORY, else from the current buffer's; an error naming what is the
matter when there is none."
  (let ((name *csvdt-program*))
    (if (find #\/ name)
        (let ((path (if (char= (char name 0) #\/)
                        name
                        (path-join (or directory (buffer-directory (current-buffer))) name))))
          (case (path-kind path)
            (:directory (error "~a is a directory, not the csvdt program: set *csvdt-program* to the program itself" path))
            ((nil) (error "Cannot find ~a: set *csvdt-program*" path))
            (t (or (find-program path)
                   (error "~a cannot be run: chmod +x it, or set *csvdt-program* to one that can" path)))))
        (or (find-program name)
            (error "Cannot find ~a on the PATH: set *csvdt-program* to its full path" name)))))

(defun csvdt-output-of (&rest arguments)
  "What csvdt writes when run with ARGUMENTS and no input; NIL when it fails,
so that a csvdt declining them is gone around rather than broken on."
  (multiple-value-bind (output ok) (program-output (cons (csvdt-program) arguments) :seconds 10)
    (and ok output)))

(defun csvdt-version-string ()
  "csvdt's --version line, which names the time zone database built into it."
  (string-trim '(#\Space #\Newline) (or (csvdt-output-of "--version") "")))

(defun csvdt-help-text ()
  "csvdt's --help."
  (or (csvdt-output-of "--help") (error "~a --help failed" *csvdt-program*)))

;;; Its options, from its own list

(defun csvdt-parse-option-list (text)
  "The options in TEXT, what csvdt --list-options prints, each a plist of
:short, :long, :value and :separator; NIL when TEXT is not in a format this
package reads (+CSVDT-OPTION-LIST-FORMAT+)."
  (let ((lines (remove "" (uiop:split-string (or text "") :separator '(#\Newline)) :test #'string=)))
    (when (equal (first lines) (format nil "# csvdt-option-list ~d" +csvdt-option-list-format+))
      (loop for line in (rest lines)
            for fields = (uiop:split-string line :separator '(#\Tab))
            unless (uiop:string-prefix-p "#" line)
              collect (list :short (nth 0 fields) :long (nth 1 fields)
                            :value (nth 2 fields) :separator (nth 3 fields))))))

(defun csvdt-parse-help (help)
  "The options HELP, csvdt's --help, lists, for a csvdt too old to list them
itself: each \"  -x, --xxx\" or \"      --xxx\" at the start of a line, the
first of each, the later ones being its prose. A help page does not say
which options take a value in a way worth reading, so :value is NIL,
not known, and CSVDT-FILE-ARGUMENTS guesses at no filenames without it."
  (let ((records '()))
    (cl-ppcre:do-register-groups (short long)
        ("(?m)^[ \\t]+(?:-([A-Za-z]), )?--([A-Za-z][A-Za-z0-9-]*)" help)
      (let ((long (concatenate 'string "--" long)))
        (unless (find long records :key (lambda (record) (getf record :long)) :test #'equal)
          (push (list :short (if short (concatenate 'string "-" short) "") :long long :value nil :separator nil)
                records))))
    (nreverse records)))

(defun csvdt-option-records ()
  "csvdt's options as records (CSVDT-PARSE-OPTION-LIST), from its own list,
else from its --help; kept while the program is the same file, unchanged
\(*CSVDT-OPTIONS-CACHE*)."
  (let* ((program (namestring (truename (csvdt-program))))
         (stamp (file-write-date program)))
    (if (and (equal (first *csvdt-options-cache*) program)
             (eql (second *csvdt-options-cache*) stamp))
        (nthcdr 3 *csvdt-options-cache*)
        (let ((records (or (csvdt-parse-option-list (csvdt-output-of "--list-options"))
                           (csvdt-parse-help (csvdt-help-text)))))
          (setf *csvdt-options-cache* (list* program stamp (csvdt-version-string) records))
          records))))

(defun csvdt-cached-version ()
  "csvdt's version line, asked with its options rather than at each command."
  (csvdt-option-records)
  (third *csvdt-options-cache*))

(defun csvdt-options ()
  "The options to offer for completion, short and long; an option whose value
must be joined with = offered with it too, since a value written apart would
be read as a filename."
  (let ((options '()))
    (dolist (record (csvdt-option-records))
      (dolist (form (list (getf record :short) (getf record :long)))
        (when (plusp (length form))
          (pushnew form options :test #'string=)
          (when (equal (getf record :separator) "equals")
            (pushnew (concatenate 'string form "=") options :test #'string=)))))
    (nreverse options)))

;;; Reading a command line

;; Read whole and split as a shell would, not as a comma-separated list: a
;; comma is csvdt's own separator inside a value (-a 1,0, -i 0,replace,
;; -o0,+02:00).

(defun csvdt-split-arguments (line)
  "LINE split into arguments as a shell would: a quoted argument may hold a
space, an unquoted one a comma, which csvdt's value lists need. Either quote
is taken; in double quotes the escapes \\t \\n \\r \\\\ \\\" and no others, in
single quotes none. So a lone quote is written in the other kind, and a value
keeping its backslashes in single quotes. Outside quotes a backslash is
itself, unlike a shell: a Windows path typed as a value comes through whole."
  (multiple-value-bind (words ok why) (command-words line :backslash :escapes)
    (unless ok (error "Cannot read ~a: ~a" line why))
    words))

(defun csvdt-argument-at (text point options)
  "The argument of TEXT, a csvdt command line, that ends at POINT, to be
completed from OPTIONS: three values, where it starts, POINT, and OPTIONS."
  (let ((space (position-if #'csvdt-space-p text :end point :from-end t)))
    (values (if space (1+ space) 0) point options)))

(defun csvdt-complete-argument ()
  "Complete the argument before the minibuffer's cursor from csvdt's options,
in a list beside it: \\<minibuffer>\\[minibuffer-complete] while csvdt arguments are read."
  (let ((options (handler-case (csvdt-options)
                   (error (e) (message "~a" e) (return-from csvdt-complete-argument nil)))))
    (complete-in-minibuffer (lambda (text point) (csvdt-argument-at text point options))
                            :constituent (lambda (char) (not (csvdt-space-p char))) :keep-order t)))

(defun csvdt-read-arguments ()
  "Read a csvdt command line and split it (CSVDT-SPLIT-ARGUMENTS). It starts
as *CSVDT-DEFAULT-ARGUMENTS*, selected: typing replaces it, \\<minibuffer>\\[minibuffer-forward-char] keeps
it to add to, \\[minibuffer-done] runs it as it is. \\[minibuffer-complete] completes the option
before the cursor, and \\[minibuffer-previous-history] and \\[minibuffer-next-history] go back and forth through the
lines run before. The prompt names the csvdt that will run."
  (let ((line (read-string (format nil "csvdt arguments (~a): " (csvdt-cached-version))
                           :initial *csvdt-default-arguments* :select-initial t
                           :complete #'csvdt-complete-argument :history *csvdt-history*)))
    (unless (equal line (first *csvdt-history*))
      (push line *csvdt-history*))
    (csvdt-split-arguments line)))

;;; What the arguments say

(defun csvdt-shorts-taking-a-separate-value ()
  "The short options whose value may be the next argument. One whose value
must be joined with = takes none apart: written apart, its value stands
where the filename goes."
  (loop for record in (csvdt-option-records)
        for short = (getf record :short)
        when (and (plusp (length short))
                  (member (getf record :value) '("required" "optional") :test #'equal)
                  (not (equal (getf record :separator) "equals")))
          collect short))

(defun csvdt-short-flags (argument)
  "The short flags clustered in ARGUMENT, -Hp being two; NIL when it is no
cluster. A short whose value may be joined ends the cluster, the rest being
its value, so -i0,replace is one option and its value; one whose value must
be joined with = does not, nothing after it in a cluster being its value. An
argument holding = is an option and its value, never a cluster."
  (when (and (> (length argument) 1)
             (char= (char argument 0) #\-)
             (char/= (char argument 1) #\-)
             (not (find #\= argument)))
    (let ((valued (csvdt-shorts-taking-a-separate-value)))
      (loop for index from 1 below (length argument)
            for short = (format nil "-~c" (char argument index))
            until (member short valued :test #'string=)
            collect short))))

(defun csvdt-expects-a-following-value (argument)
  "True when ARGUMENT is an option whose value is the argument after it: not
when its value is joined, when it takes none, or when it must be joined with =."
  (cond ((uiop:string-prefix-p "--" argument)
         (and (not (find #\= argument))
              (some (lambda (record)
                      (and (equal (getf record :long) argument)
                           (member (getf record :value) '("required" "optional") :test #'equal)
                           (not (equal (getf record :separator) "equals"))))
                    (csvdt-option-records))))
        ((and (> (length argument) 1) (char= (char argument 0) #\-))
         ;; a cluster ends at its first short taking a value, which takes the
         ;; next argument only when nothing follows it: -Hu 0 does, -Hu0 not
         (let ((valued (csvdt-shorts-taking-a-separate-value)))
           (loop for index from 1 below (length argument)
                 when (member (format nil "-~c" (char argument index)) valued :test #'string=)
                   return (= index (1- (length argument))))))))

(defun csvdt-file-arguments (arguments directory)
  "The ARGUMENTS csvdt would read as a file, those naming a file in
DIRECTORY. These commands give csvdt the lines on its standard input, and a
file named replaces them rather than adding to them, with a result looking
exactly like a run over the lines. One naming nothing on disk is left to
csvdt, whose own error says it better. When the options' records cannot say
which options take a value apart (those read from --help), nothing is
reported: a value taken for a filename would refuse a good run."
  (when (some (lambda (record) (getf record :value)) (csvdt-option-records))
    (let ((files '()) (positional nil))
      (loop while arguments
            do (let ((argument (pop arguments)))
                 (cond ((and (not positional) (string= argument "--")) (setf positional t))
                       ((and (not positional) (csvdt-expects-a-following-value argument)) (pop arguments))
                       ((and (not positional) (> (length argument) 1) (char= (char argument 0) #\-)))
                       ;; csvdt does not expand ~, so neither is it expanded here
                       ((and (plusp (length argument)) (char/= (char argument 0) #\~)
                             (eq :regular (path-kind (if (char= (char argument 0) #\/)
                                                         argument
                                                         (path-join directory argument)))))
                        (push argument files)))))
      (nreverse files))))

(defun csvdt-options-before-positionals (arguments)
  "ARGUMENTS up to --, after which everything is a filename, whatever it looks like."
  (subseq arguments 0 (position "--" arguments :test #'string=)))

(defun csvdt-header-requested-p (arguments)
  "True when ARGUMENTS ask csvdt to take the first record for a header
\(*CSVDT-HEADER-OPTIONS*), alone, long or in a cluster."
  (some (lambda (argument)
          (or (member argument *csvdt-header-options* :test #'string=)
              (intersection (csvdt-short-flags argument) *csvdt-header-options* :test #'string=)))
        (csvdt-options-before-positionals arguments)))

(defun csvdt-delimiter-in (options)
  "The code of the delimiter OPTIONS name (*CSVDT-READ-DELIMITER-OPTIONS*),
given as the next argument or joined with =; NIL for the default."
  (loop for (argument next) on options
        for joined = (find-if (lambda (name) (uiop:string-prefix-p (concatenate 'string name "=") argument))
                              *csvdt-read-delimiter-options*)
        for value = (cond ((member argument *csvdt-read-delimiter-options* :test #'string=) next)
                          (joined (subseq argument (1+ (length joined)))))
        when (plusp (length value))
          return (char-code (char value 0))))

(defun csvdt-dialect (arguments)
  "Two values, the octets of the quote and the delimiter a run with ARGUMENTS
reads with."
  (let ((options (csvdt-options-before-positionals arguments)))
    (values (if (some (lambda (argument) (member argument *csvdt-single-quote-options* :test #'string=))
                      options)
                39 34)
            (let ((delimiter (csvdt-delimiter-in options)))
              ;; one octet a field is split on; csvdt itself says what it makes of anything else
              (if (and delimiter (< delimiter 128)) delimiter 44)))))

;;; Whole lines, whole records

(defun csvdt-line-start (rope position)
  "Where the line of ROPE holding POSITION starts."
  (if (zerop (rope-bytes rope)) 0 (rope-line-start rope (min position (rope-bytes rope)))))

(defun csvdt-next-line-start (rope position)
  "Where the line after POSITION's line of ROPE starts, or the text's end."
  (let ((size (rope-bytes rope)))
    (if (>= position size)
        size
        (min size (1+ (rope-line-end rope position))))))

(defun csvdt-line-start-p (rope position)
  "True when POSITION of ROPE is at the start of a line."
  (or (zerop position) (= 10 (rope-octet rope (1- position)))))

(defun csvdt-line-end-p (rope position)
  "True when POSITION of ROPE is at the end of a line or of the text."
  (or (>= position (rope-bytes rope))
      (member (rope-octet rope position) '(10 13))))

(defun csvdt-whole-lines (rope start end)
  "START..END of ROPE widened to whole lines, as (START . END). Half a line is
half a record, which csvdt would read as a short value rather than as damage.
An END at the start of a line stops there, the line before having been taken
to its newline."
  (let ((from (csvdt-line-start rope start))
        (to (if (and (> end start) (csvdt-line-start-p rope end)) end (csvdt-next-line-start rope end))))
    (cons from (max from to))))

(defun csvdt-inside-quoted-field-p (rope position quote-octet delimiter)
  "True when POSITION of ROPE falls inside a quoted field rather than between
records, QUOTE-OCTET and DELIMITER the octets read with. Read from the start,
since nothing nearer can say which side of a quote a place is on; from quote
to quote, so it costs as many steps as there are quotes before POSITION."
  (let ((inside nil) (at 0) (size (rope-bytes rope)))
    (loop for found = (and (< at position) (rope-find-octet rope quote-octet :start at :end position))
          while found
          do (cond (inside
                    ;; a quote doubled inside a run is one quote, and the run goes on
                    (if (and (< (1+ found) size) (= quote-octet (rope-octet rope (1+ found))))
                        (setf at (+ found 2))
                        (setf inside nil at (1+ found))))
                   (t
                    ;; a quote opens a run only where a field begins; elsewhere it is data
                    (when (or (zerop found) (member (rope-octet rope (1- found)) (list delimiter 10 13)))
                      (setf inside t))
                    (setf at (1+ found)))))
    inside))

(defun csvdt-whole-records (rope start end quote-octet delimiter)
  "START..END of ROPE, whole lines, widened to whole records, as (START . END).
A quoted field may hold line ends, and its record then spans lines: a range
starting inside one would hand csvdt the tail of a record as a record of its
own, which converts cleanly into a field nobody wrote. QUOTE-OCTET and
DELIMITER are the octets the run reads with. Only ever widens; a file whose
fields hold no line ends is left as it is."
  (let ((size (rope-bytes rope)))
    (loop while (and (> start 0) (csvdt-inside-quoted-field-p rope start quote-octet delimiter))
          do (setf start (csvdt-line-start rope (1- start))))
    (loop while (and (< end size) (csvdt-inside-quoted-field-p rope end quote-octet delimiter))
          do (setf end (csvdt-next-line-start rope end)))
    (cons start (max start end))))

(defun csvdt-drop-covered (ranges)
  "RANGES, each (START . END), with what an earlier one covers taken out, in
the order given: two banked lines in one record spanning several widen to
that record twice, and it is sent once. An empty range nothing covers is
kept, the whole of an empty buffer being one."
  (let ((kept '()))
    (dolist (range ranges (nreverse kept))
      (let ((start (car range)) (end (cdr range)) (trimmed nil))
        (dolist (seen kept)
          (when (and (< start (cdr seen)) (> end (car seen)))
            (setf trimmed t)
            (cond ((and (>= start (car seen)) (<= end (cdr seen))) (setf start end))
                  ((>= start (car seen)) (setf start (cdr seen)))
                  ((<= end (cdr seen)) (setf end (car seen))))))
        (when (or (not trimmed) (< start end))
          (push (cons start end) kept))))))

(defun csvdt-settle (rope ranges quote-octet delimiter)
  "RANGES of ROPE as a run sends them: whole lines, then whole records, none
sent twice. Three values: the ranges, true when the lines widened them, true
when the records did."
  (let* ((lines (mapcar (lambda (range) (csvdt-whole-lines rope (car range) (cdr range))) ranges))
         (records (csvdt-drop-covered
                   (mapcar (lambda (range) (csvdt-whole-records rope (car range) (cdr range) quote-octet delimiter)) lines))))
    (values records (not (equal lines ranges)) (not (equal records lines)))))

(defun csvdt-header-range (rope start arguments quote-octet delimiter)
  "The buffer's header record as (0 . END), to go in front of lines from
START; NIL unless the arguments ask for a header
\(*CSVDT-REGION-CARRIES-HEADER*) and START is below it. The whole record, not
its first line: a header whose field holds a line end spans two."
  (when (and *csvdt-region-carries-header* (csvdt-header-requested-p arguments))
    (let ((header (csvdt-whole-records rope 0 (csvdt-next-line-start rope 0) quote-octet delimiter)))
      (and (< 0 (cdr header)) (>= start (cdr header)) header))))

;;; What is picked out

(defun csvdt-selection-ranges (buffer)
  "The selection of BUFFER as a list of (START . END), or NIL: a line
selection by its whole lines, a rectangle by the rows it spans, its columns
being csvdt's own to pick."
  (multiple-value-bind (start end) (and (eq buffer (current-buffer)) (selection-range :widen t))
    (and start (< start end) (list (cons start end)))))

(defun csvdt-mid-line-p (rope ranges)
  "True when one of RANGES of ROPE starts or ends inside a line, so a run
covers whole rows the selection only touched. The end of a line, before its
newline, is not inside it."
  (some (lambda (range)
          (or (not (csvdt-line-start-p rope (car range)))
              (not (or (csvdt-line-start-p rope (cdr range)) (csvdt-line-end-p rope (cdr range))))))
        ranges))

(defun csvdt-picked (buffer)
  "Two values: BUFFER's banked lines and its selection, each a list of (START
. END) in order."
  (values (banked-spans buffer) (csvdt-selection-ranges buffer)))

;;; Running it

(defstruct (csvdt-job (:include job) (:constructor make-csvdt-job) (:copier nil))
  "A run of csvdt over text of BUFFER, a job: PROGRAM run in DIRECTORY with
ARGUMENTS, given the RANGES of ROPE, the text as it was when the run began,
CRLF saying its line ends lose their carriage returns. Once it ends, EXIT is
csvdt's exit status, OUTPUT the octets it wrote and SAID what it said.
SNAPPED, SPANNED and STRETCHES are for its report."
  program directory arguments rope ranges crlf exit output said snapped spanned stretches)

(setf (documentation 'csvdt-job-p 'function) "True when OBJECT is a CSVDT-JOB.")

(define-job-words csvdt-job
  :doing "Running csvdt"
  :description (format nil "Running csvdt ~{~a~^ ~}" (csvdt-job-arguments job))
  :verb "run csvdt"
  :left-words "csvdt was stopped, its output left out")

(defun csvdt-send (job fd)
  "Write JOB's ranges to FD, csvdt's standard input, a piece at a time: one
file, each range ended by a newline, carriage returns before newlines taken
out where the buffer ends its lines with both. Stops when JOB is asked to, or
when csvdt stops reading."
  (let ((rope (csvdt-job-rope job))
        (piece (make-array *csvdt-piece* :element-type '(unsigned-byte 8)))
        (out (make-array *csvdt-piece* :element-type '(unsigned-byte 8)))
        (final nil))
    (flet ((put (octets end)
             (when (plusp end)
               (write-octets fd octets :end end :path "csvdt")
               (setf final (aref octets (1- end))))))
      (handler-case
          (dolist (range (csvdt-job-ranges job))
            (loop with at = (car range)
                  while (< at (cdr range))
                  do (when (car (csvdt-job-cancel job)) (return-from csvdt-send))
                     (let* ((n (min *csvdt-piece* (- (cdr range) at)))
                            (end (rope-copy-into rope at (+ at n) piece)))
                       (if (csvdt-job-crlf job)
                           (let ((kept 0))
                             (dotimes (i end)
                               (let ((octet (aref piece i)))
                                 (unless (and (= octet 13)
                                              (= 10 (if (< (1+ i) end)
                                                        (aref piece (1+ i))
                                                        (if (< (+ at n) (rope-bytes rope))
                                                            (rope-octet rope (+ at n))
                                                            0))))
                                   (setf (aref out kept) octet)
                                   (incf kept))))
                             (put out kept))
                           (put piece end))
                       (incf at n)))
            ;; a last line with no newline would run into the next range
            (when (and final (/= final 10))
              (put (make-array 1 :element-type '(unsigned-byte 8) :initial-element 10) 1)))
        ;; csvdt ended without reading it all: what it said says why
        (file-system-error () nil)))))

(defun csvdt-work (job)
  "Run JOB's csvdt over its ranges, written to it a piece at a time
\(CSVDT-SEND), what it writes and says read as it goes; stopping the job
ends it."
  (multiple-value-bind (written said exit)
      (filter-through-program (cons (csvdt-job-program job) (csvdt-job-arguments job))
                              (lambda (fd) (csvdt-send job fd))
                              :job job :directory (csvdt-job-directory job) :limit nil)
    (if (car (csvdt-job-cancel job))
        (setf (csvdt-job-status job) :stopped)
        (setf (csvdt-job-output job) (or written (make-array 0 :element-type '(unsigned-byte 8)))
              (csvdt-job-said job) (string-trim '(#\Space #\Newline) (or said ""))
              (csvdt-job-exit job) exit
              (csvdt-job-status job) :done))))

(defun csvdt-line-total (octets)
  "How many lines OCTETS hold, a last one with no newline counted."
  (+ (count 10 octets)
     (if (and (plusp (length octets)) (/= (aref octets (1- (length octets))) 10)) 1 0)))

(defun csvdt-fill-output (octets)
  "Make OCTETS, as csvdt wrote them, the whole of *csvdt output*, in
*CSVDT-OUTPUT-MODE*, point at its start; return the buffer. It stays a
buffer to edit and run csvdt on again, and undo brings back what it held
before. Lines banked in that are let go, the text they stood for being
gone."
  (let ((out (find-or-make-buffer *csvdt-output-name*)))
    (setf (buffer-banks out) nil
          (buffer-mark-active-p out) nil)
    (delete-spans out (list (cons 0 (buffer-size out))) :replacements (list octets))
    ;; entered after the text is in, so that csv-mode finds its separator in it
    (handler-case (set-major-mode out *csvdt-output-mode*)
      (error () (set-major-mode out 'fundamental-mode)))
    (setf (buffer-point out) 0)
    out))

(defun csvdt-strip-name (said)
  "SAID, what csvdt said, without the csvdt: it starts each of its lines with."
  (if (uiop:string-prefix-p "csvdt: " said) (subseq said (length "csvdt: ")) said))

(defmethod job-take-in ((job csvdt-job))
  "Show what JOB's csvdt wrote in *csvdt output* and say how the run went: how
many lines, over what, and what csvdt said besides; or, when it failed, its
exit status and its reason, with what it wrote before failing shown too,
unless the run was over *csvdt output* itself, which keeps its text."
  (ecase (csvdt-job-status job)
    (:done
     (let ((output (csvdt-job-output job)) (said (csvdt-job-said job)) (source (csvdt-job-buffer job)))
       (if (eql (csvdt-job-exit job) 0)
           (progn
             (display-buffer (csvdt-fill-output output) :at :start)
             (message "~a"
                      (format nil "csvdt~{ ~a~}: ~d line~:p~[~;~:;~:* from ~d stretches~]~:[~;, selection widened to whole lines~]~:[~;, widened to whole records: a quoted field spans lines~]~@[~%~a~]"
                              (csvdt-job-arguments job) (csvdt-line-total output) (csvdt-job-stretches job)
                              (csvdt-job-snapped job) (csvdt-job-spanned job) (and (plusp (length said)) said))))
           (progn
             (unless (or (zerop (length output))
                         (and source (equal (buffer-name source) *csvdt-output-name*)))
               (display-buffer (csvdt-fill-output output) :at :start))
             (message "~a" (format nil "csvdt exited ~a: ~a" (csvdt-job-exit job)
                                   (if (zerop (length said)) "(no message)" (csvdt-strip-name said))))))))
    ((:stopped :failed) (message "~a" (job-ending-words job)))))

(defun csvdt-run (buffer ranges arguments &key snapped)
  "Run csvdt with ARGUMENTS over RANGES of BUFFER, each (START . END) in order,
as one file on its standard input, and show what it writes in *csvdt
output*. The ranges are widened to whole records first (CSVDT-SETTLE), and
the header put in front of them when the arguments ask for one
\(CSVDT-HEADER-RANGE). SNAPPED says the selection began or ended inside a
line. The run goes on in the background when it takes more than a moment."
  (let* ((directory (buffer-directory buffer))
         (files (csvdt-file-arguments arguments directory)))
    (when files
      (error "csvdt would read ~{~a~^, ~} and leave the selection out; these commands give it the lines on its standard input" files))
    (when (running-job buffer 'csvdt-job)
      (error "~a" (substitute-command-keys "csvdt is still running over this buffer: \\[stop-job] stops it")))
    (let ((program (csvdt-program directory))
          (rope (buffer-text buffer)))
      (multiple-value-bind (quote-octet delimiter) (csvdt-dialect arguments)
        (multiple-value-bind (settled widened spanned) (csvdt-settle rope ranges quote-octet delimiter)
          (let* ((header (csvdt-header-range rope (car (first settled)) arguments quote-octet delimiter))
                 (job (make-csvdt-job :buffer buffer :program program :directory directory :arguments arguments
                                      :rope rope :ranges (if header (cons header settled) settled)
                                      :crlf (buffer-crlf-p buffer) :snapped (or snapped widened) :spanned spanned
                                      :stretches (length settled))))
            (start-job-thread job #'csvdt-work :name "tuatara csvdt" :details (format nil "~{~a~^ ~}" arguments))
            (take-in-soon job *csvdt-wait* "Running csvdt")))))))

;;; The commands

(define-command csvdt-run-buffer ()
  "Run csvdt over the whole buffer with arguments read in the minibuffer
\(\\[csvdt-run-dwim] says how they are read); show what it writes in *csvdt output*."
  (let ((b (current-buffer)))
    (csvdt-run b (list (cons 0 (buffer-size b))) (csvdt-read-arguments))))

(define-command csvdt-run-region ()
  "Run csvdt over the lines of the selection with arguments read in the
minibuffer: to try something on a few rows before the whole file. A
selection starting or ending inside a line takes the whole line, and a
rectangle the whole rows it spans; the header goes in front when the
arguments ask for one."
  (let* ((b (current-buffer)) (selection (csvdt-selection-ranges b)))
    (unless selection (error "No selection"))
    (csvdt-run b selection (csvdt-read-arguments)
               :snapped (csvdt-mid-line-p (buffer-text b) selection))))

(define-command csvdt-run-banked ()
  "Run csvdt over the banked lines (\\[bank-selected-lines]) with arguments
read in the minibuffer, all of them as one file in the order of the buffer:
a duration is measured across what was banked, not across what lies between.
The header goes in front when the arguments ask for one. The lines stay
banked."
  (let* ((b (current-buffer)) (banked (banked-spans b)))
    (unless banked (error "~a" (substitute-command-keys "No banked lines: \\[bank-selected-lines] banks them")))
    (csvdt-run b banked (csvdt-read-arguments))))

(define-command csvdt-run-dwim ()
  "Run csvdt over the banked lines and the selection together, or with
neither over the whole buffer; show what it writes in *csvdt output*. Both,
not one or the other: the selection is most often the part not banked yet.
Where they meet or overlap a line is sent once.
The arguments are read in the minibuffer, *CSVDT-DEFAULT-ARGUMENTS*
selected: typing replaces them, \\<minibuffer>\\[minibuffer-forward-char] keeps them to add to,
\\[minibuffer-done] runs them; \\[minibuffer-complete] completes an option from those csvdt
reports, and \\[minibuffer-previous-history] brings back a line run before. They are split as a shell
splits them, a comma staying inside its argument. A filename among them is
refused: the lines go to csvdt on its standard input, and a file would take
their place. \\[csvdt-describe-run] says what a run would cover, without
running."
  (let ((b (current-buffer)))
    (multiple-value-bind (banked selection) (csvdt-picked b)
      (let ((ranges (merge-ranges (append banked selection))))
        (csvdt-run b (or ranges (list (cons 0 (buffer-size b)))) (csvdt-read-arguments)
                   :snapped (and selection (csvdt-mid-line-p (buffer-text b) selection)))))))

(defun csvdt-describe-ranges (buffer ranges)
  "RANGES of BUFFER in words: 3 lines in 2 stretches."
  (let ((lines (span-line-count buffer ranges)))
    (format nil "~d line~:p in ~d stretch~:*~[es~;~:;es~]" lines (length ranges))))

(define-command csvdt-describe-run ()
  "Say what \\[csvdt-run-dwim] would cover, and from what, without running
anything: for when a run covered more or less than expected. Counted as a
run counts, in whole records, so two banked lines inside one record that
spans several are the one stretch the run makes of them. Which character
quotes a field decides where a record ends, so the dialect is read from
*CSVDT-DEFAULT-ARGUMENTS*; a run naming another may cover a little more."
  (let* ((b (current-buffer)) (rope (buffer-text b)))
    (multiple-value-bind (quote-octet delimiter)
        (csvdt-dialect (or (ignore-errors (csvdt-split-arguments *csvdt-default-arguments*)) '()))
      (flet ((settle (ranges) (and ranges (values (csvdt-settle rope ranges quote-octet delimiter)))))
        (multiple-value-bind (banked selection) (csvdt-picked b)
          (let ((ranges (settle (merge-ranges (append banked selection)))))
            (message "~a"
                     (format nil "csvdt would run over ~a.  Banked: ~a.  Selection: ~a."
                             (if ranges
                                 (csvdt-describe-ranges b ranges)
                                 (format nil "the whole buffer, ~d line~:p"
                                         (span-line-count b (list (cons 0 (buffer-size b))))))
                             (if banked (csvdt-describe-ranges b (settle banked)) "none")
                             (if selection (csvdt-describe-ranges b (settle selection)) "none")))))))))

(define-command csvdt-help ()
  "Show csvdt's own --help, read from the program each time, so that it is
always that of the csvdt that runs."
  (show-help-pieces (ansi-pieces (csvdt-help-text)) :name "*csvdt help*"))

(define-command csvdt-version ()
  "Say which csvdt runs, and which time zone database is built into it."
  (message "~a" (csvdt-version-string)))

;;; Keys

(defun install-csvdt ()
  "Bind the csvdt commands in CSV and TSV buffers under \\`C-c C-v': r
\\[csvdt-run-dwim], b the buffer, SPC the selection, k the banked lines, ?
\\[csvdt-describe-run], h the help, v the version."
  (define-keys (mode-keymap 'csv-mode)
               '(("C-c C-v r" csvdt-run-dwim) ("C-c C-v b" csvdt-run-buffer) ("C-c C-v SPC" csvdt-run-region)
                 ("C-c C-v k" csvdt-run-banked) ("C-c C-v ?" csvdt-describe-run) ("C-c C-v h" csvdt-help)
                 ("C-c C-v v" csvdt-version)))
  (label-keys (mode-keymap 'csv-mode) '(("C-c C-v" "csvdt"))))

(install-csvdt)
