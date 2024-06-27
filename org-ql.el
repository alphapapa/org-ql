;;; org-ql.el --- Org Query Language, search command, and agenda-like view  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql
;; Version: 0.9-pre
;; Package-Requires: ((emacs "27.1") (compat "29.1") (dash "2.18.1") (f "0.17.2") (map "2.1") (org "9.0") (org-super-agenda "1.2") (ov "1.0.6") (peg "1.0.1") (s "1.12.0") (transient "0.1") (ts "0.2-pre"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `org-ql' is a Lispy query language for Org files.  It allows you to
;; find Org entries matching certain criteria and return a list of
;; them or perform actions on them.  Commands are also provided which
;; display a buffer with matching results, similar to an Org Agenda
;; buffer.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'org-duration)
(require 'org-element)
(require 'org-habit)
(require 'seq)
(require 'subr-x)

(require 'compat)
(require 'dash)
(require 'map)
(require 'ts)

;;;; Constants

;; Note the use of the `rx' `blank' keyword, which matches "horizontal" whitespace.

(defconst org-ql-tsr-regexp-inactive
  (concat org-ts-regexp-inactive "\\(--?-?"
	  org-ts-regexp-inactive "\\)?")
  ;; MAYBE: Propose this for org.el.
  "Regular expression matching an inactive timestamp or timestamp range.")

(defconst org-ql-clock-regexp
  (rx bol (0+ blank) "CLOCK:" (group (1+ not-newline)))
  "Regular expression matching Org \"CLOCK:\" lines.
Like `org-clock-line-re', but matches the timestamp range in a
match group.")

(defconst org-ql-planning-regexp
  (rx bol (0+ blank) (or "CLOSED" "DEADLINE" "SCHEDULED") ":" (1+ blank) (group (1+ not-newline)))
  "Regular expression matching Org \"planning\" lines.
That is, \"CLOSED:\", \"DEADLINE:\", or \"SCHEDULED:\".")

(defconst org-ql-tag-line-re
  "^\\*+ \\(?:.*[ \t]\\)?\\(:\\([[:alnum:]_@#%:]+\\):\\)[ \t]*$"
  ;; Copied from `org-tag-line-re' from org.el.
  "Regexp matching tags in a headline.
Tags are stored in match group 1.  Match group 2 stores the tags
without the enclosing colons.")

(defvaralias 'org-ql-link-regexp
  ;; FIXME: `org-link-bracket-re' is void until `org-link-make-regexps' is called.
  (if (bound-and-true-p org-link-bracket-re)
      'org-link-bracket-re
    'org-bracket-link-regexp)
  "Regexp used to match Org bracket links.
Necessary because of changes in Org 9.something.")

(defconst org-ql-link-description-group
  (if (bound-and-true-p org-link-bracket-re)
      2
    3)
  ;; I wish Org would not introduce backward-incompatible changes like this in
  ;; minor releases.  It requires awkward workarounds to be maintained for years.
  "Regexp match group used to extract description from Org bracket links.
Necessary because of backward-incompatible changes in Org
9.something: when `org-link-bracket-re' was added,
`org-bracket-link-regexp' was marked as an obsolete alias for it,
but the match groups were changed, so they are not compatible.")

;;;; Compatibility
(defalias 'org-ql--org-timestamp-format
  (if (version<= "9.6" org-version)
      'org-format-timestamp
    'org-timestamp-format))

;;;; Variables

(defvar org-ql--today nil)

(defvar org-ql-use-preamble t
  ;; MAYBE: Naming things is hard.  There must be a better term than "preamble."
  "Use query preambles to speed up searches.
May be disabled for debugging, benchmarks, etc.")

(defvar org-ql-cache (make-hash-table :weakness 'key)
  ;; IIUC, setting weakness to `key' means that, when a buffer is closed,
  ;; its entries will be removed from this table at the next GC.
  "Query cache, keyed by buffer.
Each value is a list of the buffer's modified tick and another
hash table, keyed by arguments passed to
`org-ql--select-cached'.")

(defvar org-ql-tags-cache (make-hash-table :weakness 'key)
  "Per-buffer tags cache.
Keyed by buffer.  Each value is a cons of the buffer's modified
tick, and another hash table keyed on buffer position, whose
values are a list of two lists, inherited tags and local tags, as
strings.")

(defvar org-ql-node-value-cache (make-hash-table :weakness 'key)
  "Per-buffer node cache.
Keyed by buffer.  Each value is a cons of the buffer's modified
tick, and another hash table keyed on buffer position, whose
values are alists in which the key is a function and the value is
the value returned by it at that node.")

(eval-and-compile
  (defvar org-ql-predicates
    ;; FIXME: Is this remapping still necessary?  It was mapping `org-back-to-heading'
    ;; to itself until now, so maybe I broke it and it doesn't matter anymore.
    (list (cons 'org-back-to-heading (list :name 'org-back-to-heading :fn (symbol-function 'outline-back-to-heading))))
    "Plist of predicates, their corresponding functions, and their docstrings.
This list should not contain any duplicates."))

;;;;; Timestamp regexps

;; We need more specificity than the built-in Org timestamp regexps
;; provide, and sometimes they change from version to version, so we
;; define our own.  And by defining them with `rx', they are much
;; easier to understand than the string-based ones in org.el (of
;; course, `rx' probably wasn't available when most of those were
;; written).

;; MAYBE: Use newer `rx' custom expressions to define these.
;; MAYBE: Add match groups corresponding to the ones in the "official" Org regexps.
;; TODO: Use these new regexps in more places.

(defvar org-ql-regexp-part-ts-date
  (rx (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)
      ;; Day of week
      (optional " " (1+ (or alpha punct))))
  "Matches the inner, date part of an Org timestamp, both active and inactive.
Used to build other timestamp regexps.")

(defvar org-ql-regexp-part-ts-repeaters
  ;; Repeaters (not sure if the colon is necessary, but it's in the org.el one)
  (rx (repeat 1 2 (seq " " (repeat 1 2 (any "-+:.")) (1+ digit) (any "hdwmy")
                       (optional "/" (1+ digit) (any "hdwmy")))))
  "Matches the repeater part of an Org timestamp.
Includes leading space character.")

(defvar org-ql-regexp-part-ts-time
  (rx " " (repeat 1 2 digit) ":" (repeat 2 digit)
      (optional "-" (repeat 1 2 digit) ":" (repeat 2 digit)))
  "Matches the inner, time part of an Org timestamp (i.e. HH:MM).
Includes leading space character.  Used to build other timestamp
regexps.")

;; NOTE: The inactive timestamp regexps don't allow repeaters.  I don't know if this is
;; officially correct, but it seems to make sense, and would be easy to change if necessary.

(defvar org-ql-regexp-ts-both
  (rx-to-string
   `(or (seq "<" (regexp ,org-ql-regexp-part-ts-date)
             (optional (regexp ,org-ql-regexp-part-ts-time))
             (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">")
        (seq "[" (regexp ,org-ql-regexp-part-ts-date)
             (optional (regexp ,org-ql-regexp-part-ts-time)))))
  "Matches both active and inactive Org timestamps, with or without time.")

(defvar org-ql-regexp-ts-both-with-time
  (rx-to-string `(or (seq "<" (regexp ,org-ql-regexp-part-ts-date)
                          (regexp ,org-ql-regexp-part-ts-time)
                          (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">")
                     (seq "[" (regexp ,org-ql-regexp-part-ts-date)
                          (regexp ,org-ql-regexp-part-ts-time) "]")))
  "Matches both active and inactive Org timestamps, with time.")

(defvar org-ql-regexp-ts-both-without-time
  (rx-to-string `(or (seq "<" (regexp ,org-ql-regexp-part-ts-date)
                          (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">")
                     (seq "[" (regexp ,org-ql-regexp-part-ts-date) "]")))
  "Matches both active and inactive Org timestamps, without time.")

(defvar org-ql-regexp-ts-active
  (rx-to-string `(seq "<" (regexp ,org-ql-regexp-part-ts-date)
                      (optional (regexp ,org-ql-regexp-part-ts-time))
                      (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">"))
  "Matches active Org timestamps, with or without time.")

(defvar org-ql-regexp-ts-active-with-time
  (rx-to-string `(seq "<" (regexp ,org-ql-regexp-part-ts-date)
                      (regexp ,org-ql-regexp-part-ts-time)
                      (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">"))
  "Matches active Org timestamps, with time.")

(defvar org-ql-regexp-ts-active-without-time
  (rx-to-string `(seq "<" (regexp ,org-ql-regexp-part-ts-date)
                      (optional (regexp ,org-ql-regexp-part-ts-repeaters)) ">"))
  "Matches active Org timestamps, without time.")

(defvar org-ql-regexp-ts-inactive
  (rx-to-string `(seq "[" (regexp ,org-ql-regexp-part-ts-date)
                      (optional (regexp ,org-ql-regexp-part-ts-time))"]"))
  "Matches inactive Org timestamps, with or without time.")

(defvar org-ql-regexp-ts-inactive-with-time
  (rx-to-string `(seq "[" (regexp ,org-ql-regexp-part-ts-date)
                      (regexp ,org-ql-regexp-part-ts-time)"]"))
  "Matches inactive Org timestamps, with time.")

(defvar org-ql-regexp-ts-inactive-without-time
  (rx-to-string `(seq "[" (regexp ,org-ql-regexp-part-ts-date) "]"))
  "Matches inactive Org timestamps, without time.")

(defvar org-ql-regexp-planning
  (rx-to-string `(seq bow (or (seq "CLOSED" ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-inactive)))
                              (seq (or "DEADLINE" "SCHEDULED") ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-active))))))
  "Matches CLOSED, DEADLINE or SCHEDULED keyword with timestamp.
Matches with or without time.")

(defvar org-ql-regexp-planning-with-time
  (rx-to-string `(seq bow (or (seq "CLOSED" ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-inactive-with-time)))
                              (seq (or "DEADLINE" "SCHEDULED") ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-active-with-time))))))
  "Matches CLOSED, DEADLINE or SCHEDULED keyword with timestamp, with time.")

(defvar org-ql-regexp-planning-without-time
  (rx-to-string `(seq bow (or (seq "CLOSED" ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-inactive-without-time)))
                              (seq (or "DEADLINE" "SCHEDULED") ":" (0+ " ")
                                   (group-n 1 (regexp ,org-ql-regexp-ts-active-without-time))))))
  "Matches CLOSED, DEADLINE or SCHEDULED keyword with timestamp, without time.")

(defvar org-ql-regexp-deadline
  (rx-to-string `(seq bow "DEADLINE" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active))))
  "Matches DEADLINE keyword with a time-and-hour stamp, with or without time.")

(defvar org-ql-regexp-deadline-with-time
  (rx-to-string `(seq bow "DEADLINE" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active-with-time))))
  "Matches DEADLINE keyword with a time-and-hour stamp, with time.")

(defvar org-ql-regexp-deadline-without-time
  (rx-to-string `(seq bow "DEADLINE" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active-without-time))))
  "Matches DEADLINE keyword with a time-and-hour stamp, without time.")

(defvar org-ql-regexp-scheduled
  (rx-to-string `(seq bow "SCHEDULED" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active))))
  "Matches SCHEDULED keyword with a time-and-hour stamp, with or without time.")

(defvar org-ql-regexp-scheduled-with-time
  (rx-to-string `(seq bow "SCHEDULED" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active-with-time))))
  "Matches SCHEDULED keyword with a time-and-hour stamp, with time.")

(defvar org-ql-regexp-scheduled-without-time
  (rx-to-string `(seq bow "SCHEDULED" ":" (0+ " ")
                      (group (regexp ,org-ql-regexp-ts-active-without-time))))
  "Matches SCHEDULED keyword with a time-and-hour stamp, without time.")

;;;; Customization

(defgroup org-ql nil
  "Customization for `org-ql'."
  :group 'org
  :link '(custom-manual "(org-ql)Usage")
  :link '(url-link "https://github.com/alphapapa/org-ql"))

(defcustom org-ql-signal-peg-failure nil
  "Signal an error when parsing a plain-string query fails.
This should only be enabled while debugging."
  :type 'boolean)

(defcustom org-ql-ask-unsafe-queries t
  "Ask before running a query that could run arbitrary code.
Org QL queries in sexp form can contain arbitrary expressions.
When opening an \"org-ql-search:\" link or updating a dynamic
block that contains a query in sexp form, and this option is
non-nil, the user will be prompted for confirmation before
opening the link.

This variable may be set file-locally to disable this warning in
files that the user assumes are safe (e.g. of known provenance).
Users who are entirely unconcerned about this issue may disable
the option globally (at their own risk, however minimal it
probably is).

See Info node `(org-ql)Queries'."
  :type 'boolean
  :risky t)

(defcustom org-ql-default-predicate 'rifle
  "Predicate used for plain-string tokens without a specified predicate."
  :type '(choice (const heading)
                 (const heading-regexp)
                 (const regexp)
                 (const rifle)
                 (const smart)
                 (const outline-path)
                 (const outline-path-segment)))

;;;; Functions

;;;;; Query execution

(define-hash-table-test 'org-ql-hash-test #'equal (lambda (args)
                                                    (sxhash-equal (prin1-to-string args))))

;;;###autoload
(cl-defun org-ql-select (buffers-or-files query &key action narrow sort)
  "Return items matching QUERY in BUFFERS-OR-FILES.

BUFFERS-OR-FILES is a file or buffer, a list of files and/or
buffers, or a function which returns such a list.

QUERY is an `org-ql' query sexp (quoted, since this is a
function).

ACTION is a function which is called on each matching entry with
point at the beginning of its heading.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to calling
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-view--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

If NARROW is non-nil, buffers are not widened (the default is to
widen and search the entire buffer).

SORT is either nil, in which case items are not sorted; or one or
a list of defined `org-ql' sorting methods (`date', `deadline',
`scheduled', `closed', `todo', `priority', `reverse', or `random'); or a
user-defined comparator function that accepts two items as
arguments and returns nil or non-nil.  Sorting methods are
applied in the order given (i.e. later methods override earlier
ones), and `reverse' may be used more than once.

For example, `(date priority)' would present items with the
highest priority first, and within each priority the oldest items
would appear first.  In contrast, `(date reverse priority)' would
also present items with the highest priority first, but within
each priority the newest items would appear first."
  (declare (indent defun))
  (-let* ((buffers (->> (cl-typecase buffers-or-files
                          (null (list (current-buffer)))
                          (function (funcall buffers-or-files))
                          (list buffers-or-files)
                          (otherwise (list buffers-or-files)))
                        (--map (cl-etypecase it
                                 ;; NOTE: This etypecase is essential to opening links safely,
                                 ;; as it rejects, e.g. lambdas in the buffers-files argument.
                                 (buffer it)
                                 (string (or (find-buffer-visiting it)
                                             (when (file-readable-p it)
                                               ;; It feels unintuitive that `find-file-noselect' returns
                                               ;; a buffer if the filename doesn't exist.
                                               (find-file-noselect it))
                                             (display-warning 'org-ql-select (format "Can't open file: %s" it) :error)))))
                        ;; Ignore special/hidden buffers.
                        (--remove (string-prefix-p " " (buffer-name it)))))
          (query (org-ql--normalize-query query))
          ((&plist :query :preamble :preamble-case-fold) (org-ql--query-preamble query))
          (predicate (org-ql--query-predicate query))
          (action (pcase action
                    ;; NOTE: These two lambdas are backquoted to prevent "unused lexical
                    ;; variable" warnings from byte-compilation, because they don't use
                    ;; all of the variables from their enclosing scope.
                    ('element-with-markers (byte-compile
                                            `(lambda (&rest _ignore)
                                               (org-ql--add-markers
                                                (org-element-headline-parser (line-end-position))))))
                    ((or 'nil 'element) (byte-compile
                                         `(lambda (&rest _ignore)
                                            (org-element-headline-parser (line-end-position)))))
                    ((pred functionp) action)
                    ((and (pred listp) (guard (or (special-form-p (car action))
                                                  (macrop (car action))
                                                  (functionp (car action)))))
                     (byte-compile
                      `(lambda (&rest _ignore)
                         ,action)))
                    (_ (user-error "Invalid action form: %s" action))))
          (org-ql--today (ts-now))
          (items (let (orig-fns)
                   (unwind-protect
                       (progn
                         (--each org-ql-predicates
                           ;; Set predicate functions.
                           (-let (((&plist :name :fn) (cdr it)))
                             ;; Save original function.
                             (push (list :name name :fn (symbol-function name)) orig-fns)
                             ;; Temporarily set new function definition.
                             (fset name fn)))
                         ;; Run query on buffers.
                         (->> buffers
                              (--map (with-current-buffer it
                                       (unless (derived-mode-p 'org-mode)
                                         (display-warning 'org-ql-select (format  "Not an Org buffer: %s" (buffer-name)) :error))
                                       (org-ql--select-cached :query query :preamble preamble :preamble-case-fold preamble-case-fold
                                                              :predicate predicate :action action :narrow narrow)))
                              (-flatten-n 1)))
                     (--each orig-fns
                       ;; Restore original function mappings.
                       (-let (((&plist :name :fn) it))
                         (fset name fn)))))))
    ;; Sort items
    (pcase sort
      (`nil items)
      ((guard (cl-subsetp (-list sort) '(date deadline scheduled closed todo priority random reverse)))
       ;; Default sorting functions
       (org-ql--sort-by items (-list sort)))
      ;; Sort by user-given comparator.
      ((pred functionp) (-sort sort items))
      (_ (user-error "SORT must be either nil, one or a list of the defined sorting methods (see documentation), or a comparison function of two arguments")))))

;;;###autoload
(cl-defun org-ql-query (&key (select 'element-with-markers) from where narrow order-by)
  "Like `org-ql-select', but arguments are named more like a SQL query.

SELECT corresponds to the `org-ql-select' argument ACTION.  It is
the function called on matching headings, the results of which
are returned by this function.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-view--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

FROM corresponds to the `org-ql-select' argument BUFFERS-OR-FILES.
It may be one or a list of file paths and/or buffers.

WHERE corresponds to the `org-ql-select' argument QUERY.  It
should be an `org-ql' query sexp.

ORDER-BY corresponds to the `org-ql-select' argument SORT, which
see.

NARROW corresponds to the `org-ql-select' argument NARROW."
  (declare (indent 0))
  (org-ql-select from where
    :action select
    :narrow narrow
    :sort order-by))

(defun org-ql--select-cached (&rest args)
  "Return results for ARGS and current buffer using cache."
  ;; MAYBE: Timeout cached queries.  Probably not necessarily since they will be removed when a
  ;; buffer is closed, or when a query is run after modifying a buffer.
  (-let* (((&plist :query :preamble :action :narrow :preamble-case-fold) args)
          (query-cache-key
           ;; The key must include the preamble, because some queries are replaced by
           ;; the preamble, leaving a nil query, which would make the key ambiguous.
           (list :query query :preamble preamble :action action :preamble-case-fold preamble-case-fold
                 (if narrow
                     ;; Use bounds of narrowed portion of buffer.
                     (cons (point-min) (point-max))
                   nil))))
    (if-let* ((buffer-cache (gethash (current-buffer) org-ql-cache))
              (query-cache (cadr buffer-cache))
              (modified-tick (car buffer-cache))
              (buffer-unmodified-p (eq (buffer-chars-modified-tick) modified-tick))
              (cached-result (gethash query-cache-key query-cache)))
        (pcase cached-result
          ('org-ql-nil nil)
          (_ cached-result))
      (let ((new-result (apply #'org-ql--select args)))
        (cond ((or (not query-cache)
                   (not buffer-unmodified-p))
               (puthash (current-buffer)
                        (list (buffer-chars-modified-tick)
                              (let ((table (make-hash-table :test 'org-ql-hash-test)))
                                (puthash query-cache-key (or new-result 'org-ql-nil) table)
                                table))
                        org-ql-cache))
              (t (puthash query-cache-key (or new-result 'org-ql-nil) query-cache)))
        new-result))))

(cl-defun org-ql--select (&key preamble preamble-case-fold predicate action narrow
                               &allow-other-keys)
  "Return results for given arguments.
Return results of mapping function ACTION across entries in
current buffer matching function PREDICATE.  If NARROW is
non-nil, buffer will not be widened.

PREAMBLE may be a regexp to search for before calling PREDICATE.
When doing so, `case-fold-search' is bound to
PREAMBLE-CASE-FOLD."
  ;; Since the mappings are stored in the variable `org-ql-predicates', macros like `flet'
  ;; can't be used, so we do it manually (this is same as the equivalent `flet' expansion).
  ;; Mappings are stored in the variable because it allows predicates to be defined with a
  ;; macro, which allows documentation to be easily generated for them.
  (save-excursion
    (save-restriction
      (unless narrow
        (widen))
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (outline-next-heading))
      (if (not (org-at-heading-p))
          (progn
            ;; No headings in buffer: return nil.
            (unless (string-prefix-p " " (buffer-name))
              ;; Not a special, hidden buffer: show message, because if a user accidentally
              ;; searches a buffer without headings, he might be confused.
              (message "org-ql: No headings in buffer: %s" (current-buffer)))
            nil)
        ;; Find matching entries.
        ;; TODO: Bind `case-fold-search' around the preamble loop.
        (cond (preamble (cl-loop while (let ((case-fold-search preamble-case-fold))
                                         (re-search-forward preamble nil t))
                                 do (outline-back-to-heading 'invisible-ok)
                                 when (funcall predicate)
                                 collect (funcall action)
                                 do (outline-next-heading)))
              (t (cl-loop when (funcall predicate)
                          collect (funcall action)
                          while (outline-next-heading))))))))

;;;;; Helpers

(defun org-ql--ensure-buffer (file-or-buffer)
  "Ensure a buffer is named or visiting FILE-OR-BUFFER.
If no such buffer exists with the name, and it is the name of a
readable file, `find-file-noselect' it into a buffer."
  ;; See comment in `org-ql-find'.
  ;; FIXME: Use this in `helm-org-ql' the same way it's used in
  ;; `org-ql-find'.
  (unless (or (get-buffer file-or-buffer)
              (find-buffer-visiting file-or-buffer))
    (if (file-readable-p file-or-buffer)
        (with-current-buffer (find-file-noselect file-or-buffer)
          (cl-assert (eq 'org-mode major-mode) nil (format "Not an Org buffer: %S" file-or-buffer)))
      (display-warning 'org-ql (format "Not a readable file: %S" file-or-buffer) :error))))

(defun org-ql--tags-at (position)
  ;; FIXME: This function actually assumes that point is already at POSITION.
  "Return tags for POSITION in current buffer.
Returns cons (INHERITED-TAGS . LOCAL-TAGS)."
  ;; I'd like to use `-if-let*', but it doesn't leave non-nil variables
  ;; bound in the else clause, so destructured variables that are non-nil,
  ;; like found caches, are not available in the else clause.
  (if-let* ((buffer-cache (gethash (current-buffer) org-ql-tags-cache))
            (modified-tick (car buffer-cache))
            (tags-cache (cdr buffer-cache))
            (buffer-unmodified-p (eq (buffer-chars-modified-tick) modified-tick))
            (cached-result (gethash position tags-cache)))
      ;; Found in cache: return them.
      ;; FIXME: Isn't `cached-result' a list of (INHERITED . LOCAL)?  It
      ;; will never be just `org-ql-nil', but the CAR and CDR may be, so
      ;; they need to each be checked and replaced with nil if necessary.
      (pcase cached-result
        ('org-ql-nil nil)
        (_ cached-result))
    ;; Not found in cache: get tags and cache them.
    (let* ((local-tags (or (when (looking-at org-ql-tag-line-re)
                             (split-string (match-string-no-properties 2) ":" t))
                           'org-ql-nil))
           (inherited-tags (or (when org-use-tag-inheritance
                                 (save-excursion
                                   (if (org-up-heading-safe)
                                       ;; Return parent heading's tags.
                                       (-let* (((inherited local) (org-ql--tags-at (point)))
                                               (tags (when (or inherited local)
                                                       (cond ((and (listp inherited)
                                                                   (listp local))
                                                              (->> (append inherited local)
                                                                   -non-nil -uniq))
                                                             ((listp inherited) inherited)
                                                             ((listp local) local)))))
                                         (cl-typecase org-use-tag-inheritance
                                           (list (setf tags (-intersection tags org-use-tag-inheritance)))
                                           (string (setf tags (--select (string-match org-use-tag-inheritance it)
                                                                        tags))))
                                         (pcase org-tags-exclude-from-inheritance
                                           ('nil tags)
                                           (_ (-difference tags org-tags-exclude-from-inheritance))))
                                     ;; Top-level heading: use file tags.
                                     org-file-tags)))
                               'org-ql-nil))
           (all-tags (list inherited-tags local-tags)))
      ;; Check caches again, because they may have been set now.
      ;; TODO: Is there a clever way we could avoid doing this, or is it inherently necessary?
      (setf buffer-cache (gethash (current-buffer) org-ql-tags-cache)
            modified-tick (car buffer-cache)
            tags-cache (cdr buffer-cache)
            buffer-unmodified-p (eq (buffer-chars-modified-tick) modified-tick))
      (unless (and buffer-cache buffer-unmodified-p)
        ;; Buffer-local tags cache empty or invalid: make new one.
        (setf tags-cache (make-hash-table))
        (puthash (current-buffer)
                 (cons (buffer-chars-modified-tick) tags-cache)
                 org-ql-tags-cache))
      (puthash position all-tags tags-cache))))

(defun org-ql--outline-path ()
  "Return outline path for heading at point."
  (save-excursion
    (let ((heading (save-match-data
                     (let (case-fold-search)
                       (if (looking-at org-complex-heading-regexp)
                           (or (match-string-no-properties 4) "")
                         "")))))
      (if (org-up-heading-safe)
          ;; MAYBE: It seems wrong to call the cache function from
          ;; inside this function, like a violation of separation of
          ;; concern.  Can this be rewritten to not work that way?
          (append (org-ql--value-at (point) #'org-ql--outline-path)
                  (list heading))
        (list heading)))))

;; TODO: Use --value-at for tags cache.

(defun org-ql--value-at (position fn)
  ;; TODO: Either rename to `value-at-point' and remove `position' arg, or move point.
  "Return FN's value at POSITION in current buffer.
Values compared with `equal'."
  ;; I'd like to use `-if-let*', but it doesn't leave non-nil variables
  ;; bound in the else clause, so destructured variables that are non-nil,
  ;; like found caches, are not available in the else clause.
  (pcase (if-let* ((buffer-cache (gethash (current-buffer) org-ql-node-value-cache))
                   (modified-tick (car buffer-cache))
                   (position-cache (cdr buffer-cache))
                   (buffer-unmodified-p (eq (buffer-chars-modified-tick) modified-tick))
                   (value-cache (gethash position position-cache))
                   (cached-value (alist-get fn value-cache nil nil #'equal)))
             ;; Found in cache: return it.
             cached-value
           ;; Not found in cache: call FN, cache and return its value.
           (let ((new-value (or (funcall fn) 'org-ql-nil)))
             ;; Check caches again, because it may have been set now, e.g. by
             ;; recursively going up an outline tree.
             ;; TODO: Is there a clever way we could avoid doing this, or is it inherently necessary?
             (setf buffer-cache (gethash (current-buffer) org-ql-node-value-cache)
                   modified-tick (car buffer-cache)
                   position-cache (cdr buffer-cache)
                   value-cache (when position-cache
                                 (gethash position position-cache))
                   buffer-unmodified-p (eq (buffer-chars-modified-tick) modified-tick))
             (unless (and buffer-cache buffer-unmodified-p)
               ;; Buffer-local node cache empty or invalid: make new one.
               (setf position-cache (make-hash-table)
                     value-cache (gethash position position-cache))
               (puthash (current-buffer)
                        (cons (buffer-chars-modified-tick) position-cache)
                        org-ql-node-value-cache))
             (setf (alist-get fn value-cache nil nil #'equal) new-value)
             (puthash position value-cache position-cache)
             new-value))
    ;; Return nil or the non-nil value.
    ('org-ql-nil nil)
    (else else)))

(defun org-ql--add-markers (element)
  "Return ELEMENT with Org marker text properties added.
ELEMENT should be an Org element like that returned by
`org-element-headline-parser'.  This function should be called
from within ELEMENT's buffer."
  ;; NOTE: `org-agenda-new-marker' works, until it doesn't, because...I don't know.  It sometimes
  ;; raises errors or returns markers that don't point into a buffer.  `copy-marker' always works,
  ;; of course, but maybe it will leave "dangling" markers, which could affect performance over
  ;; time?  I don't know, but for now, it seems that we have to use `copy-marker'.
  (let* ((marker (copy-marker (org-element-property :begin element)))
         (properties (--> (cadr element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (cadr element) properties)
    element))

(defun org-ql--ask-unsafe-query (query)
  "Signal an error if user rejects running QUERY.
If `org-ql-view-ask-unsafe-links' is nil, does nothing and
returns nil."
  (when org-ql-ask-unsafe-queries
    (let ((query-string (propertize (cl-etypecase query
                                      (list (prin1-to-string query))
                                      (string query))
                                    'face 'font-lock-warning-face)))
      (unless (yes-or-no-p (concat "Query is in sexp form and could contain arbitrary code: "
                                   query-string " Execute it? "))
        (user-error "Query aborted by user")))))

(defun org-ql--plist-get* (plist property)
  "Return the value of PROPERTY in PLIST, or `not-found'.
Returns `not-found' if the property is missing."
  (if-let ((pair (plist-member plist property)))
      (cadr pair)
    'not-found))

;;;;; Query processing

;; Processing, compiling, etc. for queries.

;; This error is used for when compiling a query signals an error,
;; making it easier for the UI to avoid spurious warnings, e.g. for
;; partially typed queries in the Helm commands.
(define-error 'org-ql-invalid-query "Invalid Org QL query" 'user-error)

(defun org-ql--coalesce-ands (query)
  "Return QUERY having coalesced any AND'ed clauses' predicates.
Multiple calls to the same predicate within an `and' expression
are coalesced into a single call to the predicate.

Note that this is a relatively simple function which does not
comprehensively coalesce every call that could be.  For example,
if QUERY contained four calls to the `src' predicate with two
unique language arguments, only the calls for one language would
be coalesced."
  ;; TODO: Use a per-predicate alist-getting function that accounts
  ;; for arguments which must be unique...maybe...someday...

  ;; NOTE: This implentation can sometimes reorder sub-expressions,
  ;; like:
  ;;
  ;;   (and (src :regexps ("foo") :lang "elisp") (src :regexps ("bar")))
  ;;
  ;; becomes:
  ;;
  ;;   (and (src :regexps ("bar")) (src :regexps ("foo") :lang "elisp"))
  ;;
  ;; because the first one could be coalescable, but the second one
  ;; can't be coalesced with it since they don't specify the same
  ;; language.  That could be fixed, but it's probably not worth it.
  (cl-labels ((rec (sexp)
                (pcase sexp
                  (`(,(and boolean (or 'or 'not)) . ,sexps)
                   `(,boolean ,@(mapcar #'rec sexps)))
                  (`(and . ,sexps)
                   (anded sexps))
                  (_ sexp)))
              (anded (sexps)
                (let (anded-predicates new-sexp)
                  (dolist (sexp sexps)
                    (pcase sexp
                      (`(,(or 'or 'not) . ,_)
                       (push (rec sexp) new-sexp))
                      (`(,predicate . ,args)
                       (pcase-exhaustive (plist-get (alist-get predicate org-ql-predicates) :coalesce)
                         (`nil (push sexp new-sexp))
                         (`t (setf (alist-get predicate anded-predicates)
                                   (append (alist-get predicate anded-predicates) args)))
                         ((and fn (pred functionp))
                          (if-let (new-args (funcall fn (alist-get predicate anded-predicates) args))
                              (setf (alist-get predicate anded-predicates) new-args)
                            (push sexp new-sexp)))))))
                  (delq nil `(and ,@(nreverse new-sexp) ,@(nreverse anded-predicates))))))
    (rec query)))

(defun org-ql--sanity-check-form (form)
  "Signal error if any forms in FORM do not have preconditions met.
Or, when possible, fix the problem."
  (cl-flet ((check (symbol)
              (pcase symbol
                ('done (unless org-done-keywords
                         ;; NOTE: This check needs to be done from within the Org buffer being checked.
                         (error "Variable `org-done-keywords' is nil.  Are you running this from an Org buffer?"))))))
    (cl-loop for elem in form
	     if (consp elem)
	     do (progn
		  (check (car elem))
		  (org-ql--sanity-check-form (cdr elem)))
	     else do (check elem))))

(cl-defun org-ql--link-regexp (&key description-or-target description target)
  "Return a regexp matching Org links according to arguments.
Each argument is treated as a regexp (so non-regexp strings
should be quoted before being passed to this function).  If
DESCRIPTION-OR-TARGET, match it in either description or target.
If DESCRIPTION, match it in the description.  If TARGET, match it
in the target.  If both DESCRIPTION and TARGET, match both,
respectively."
  ;; This `rx' part is borrowed from `org-make-link-regexps'.  It matches the interior of an
  ;; Org link target (i.e. the parts between the brackets, including any escaped brackets).
  (let ((link-target-part '(0+ (or (not (any "[]\\"))
			           (and "\\" (0+ "\\\\") (any "[]"))
			           (and (1+ "\\") (not (any "[]")))))))
    (cl-labels
        ((no-desc (match)
           (rx-to-string `(seq (or bol (1+ blank))
                               "[[" ,link-target-part (regexp ,match) ,link-target-part
                               "]]")))
         (match-both (description target)
           (rx-to-string `(seq (or bol (1+ blank))
                               "[[" ,link-target-part (regexp ,target) ,link-target-part
                               "][" (*? anything) (regexp ,description) (*? anything)
                               "]]")))
         ;; Note that these actually allow empty descriptions
         ;; or targets, depending on what they are matching.
         (match-desc (match)
           (rx-to-string `(seq (or bol (1+ blank))
                               "[[" ,link-target-part
                               "][" (*? anything) (regexp ,match) (*? anything)
                               "]]")))
         (match-target (match)
           (rx-to-string `(seq (or bol (1+ blank))
                               "[[" ,link-target-part (regexp ,match) ,link-target-part
                               "][" (*? anything)
                               "]]"))))
      (cond (description-or-target
             (rx-to-string `(or (regexp ,(no-desc description-or-target))
                                (regexp ,(match-desc description-or-target))
                                (regexp ,(match-target description-or-target)))))
            ((and description target)
             (match-both description target))
            (description (match-desc description))
            (target (rx-to-string `(or (regexp ,(no-desc target))
                                       (regexp ,(match-target target)))))))))

(defun org-ql--format-src-block-regexp (&optional lang)
  "Return regexp equivalent to `org-babel-src-block-regexp' with LANG filled in."
  ;; I couldn't find a way to match block contents without the regexp
  ;; also matching past the end of the block and into later blocks.  Even
  ;; using `minimal-match' in several different combinations didn't work.
  ;; So matching contents will have to be done with the predicate.
  (rx-to-string `(seq bol (group (zero-or-more (any "	 ")))
                      "#+begin_src"
                      (one-or-more (any "	 "))
                      ,(or lang `(1+ (not (any "	\n\f "))))
                      (zero-or-more (any "	 "))
                      (group (or (seq (zero-or-more (not (any "\n\":")))
                                      "\""
                                      (zero-or-more (not (any "\n\"*")))
                                      "\""
                                      (zero-or-more (not (any "\n\":"))))
                                 (zero-or-more (not (any "\n\":")))))
                      (group (zero-or-more (not (any "\n")))) "\n"
                      (63 (group (*\? (not (any " "))) "\n"))
                      (zero-or-more (any "	 "))
                      "#+end_src")
                t))

(defun org-ql--byte-compile-warning (string _pos _fill level)
  "Signal an `org-ql-invalid-query' error.
Arguments STRING, POS, FILL, and LEVEL are according to
`byte-compile-log-warning-function'."
  ;; Used as the `byte-compile-log-warning-function' in `org-ql--query-preamble'.
  (signal 'org-ql-invalid-query (list string level)))

(defun org-ql--query-predicate (query)
  "Return predicate function for QUERY."
  ;; Use custom log function to prevent warnings for e.g. partially typed queries.
  (let ((byte-compile-log-warning-function #'org-ql--byte-compile-warning))
    (byte-compile
     `(lambda ()
        ;; NOTE: If possible, all of this argument processing should be done in each predicate's
        ;; normalizers.  However, it's probably better to do the regexps here, because we don't
        ;; want that showing up in the normalized query form that the user sees.
        ;; NOTE: `clocked' and `closed' don't have WITH-TIME args,
        ;; because they should always have a time.
        ;; NOTE: The pcases check for both t/nil symbols and strings, because the
        ;; string queries always return keyword arguments' values as strings.
        (cl-macrolet ((clocked (&key from to on)
                        `(org-ql--predicate-clocked :from ,from :to ,to))
                      (closed (&key from to on (with-time 'not-found))
                        `(org-ql--predicate-closed :from ,from :to ,to))
                      (deadline (&key from to on (with-time 'not-found))
                        `(org-ql--predicate-deadline
                          :from ,from :to ,to :with-time ',with-time
                          :regexp ,(pcase-exhaustive with-time
                                     ((or 't "t") org-ql-regexp-deadline-with-time)
                                     ((or 'nil "nil") org-ql-regexp-deadline-without-time)
                                     ('not-found org-ql-regexp-deadline))))
                      (planning (&key from to on (with-time 'not-found))
                        `(org-ql--predicate-planning
                          :from ,from :to ,to :with-time ',with-time
                          :regexp ,(pcase-exhaustive with-time
                                     ((or 't "t") org-ql-regexp-planning-with-time)
                                     ((or 'nil "nil") org-ql-regexp-planning-without-time)
                                     ('not-found org-ql-regexp-planning))))
                      (scheduled (&key from to on (with-time 'not-found))
                        `(org-ql--predicate-scheduled
                          :from ,from :to ,to :with-time ',with-time
                          :regexp ,(pcase-exhaustive with-time
                                     ((or 't "t") org-ql-regexp-scheduled-with-time)
                                     ((or 'nil "nil") org-ql-regexp-scheduled-without-time)
                                     ('not-found org-ql-regexp-scheduled))))
                      (ts (&key from to on (type 'both) (with-time 'not-found))
                        ;; NOTE: The TYPE argument is elided from the arguments actually passed to the predicate, being converted to the REGEXP argument.
                        ;; MAYBE: Move the :regexp handling out of this macrolet and into the normalizer.
                        `(org-ql--predicate-ts
                          :from ,from :to ,to :with-time ',with-time
                          :regexp ,(pcase type
                                     ((or 'nil 'both) (pcase-exhaustive with-time
                                                        ((or 't "t") org-ql-regexp-ts-both-with-time)
                                                        ((or 'nil "nil") org-ql-regexp-ts-both-without-time)
                                                        ('not-found org-ql-regexp-ts-both)))
                                     ('active (pcase-exhaustive with-time
                                                ((or 't "t") org-ql-regexp-ts-active-with-time)
                                                ((or 'nil "nil") org-ql-regexp-ts-active-without-time)
                                                ('not-found org-ql-regexp-ts-active)))
                                     ('inactive (pcase-exhaustive with-time
                                                  ((or 't "t") org-ql-regexp-ts-inactive-with-time)
                                                  ((or 'nil "nil") org-ql-regexp-ts-inactive-without-time)
                                                  ('not-found org-ql-regexp-ts-inactive)))))))
          ,query)))))

;;;;; String query parsing

;; This section implements parsing of "plain," non-Lisp queries using the `peg'
;; library.  NOTE: This needs to appear after the predicates are defined.

(require 'peg)

(defun org-ql--def-query-string-to-sexp-fn (predicates)
  "Define function `org-ql--query-string-to-sexp' according to PREDICATES.
Builds the PEG expression using PREDICATES (which should be the
value of `org-ql-predicates')."
  (let* ((names (--map (symbol-name (plist-get (cdr it) :name))
                       predicates))
         (aliases (->> predicates
                       (--map (plist-get (cdr it) :aliases))
                       -non-nil
                       -flatten
                       (-map #'symbol-name)))
         (predicate-names (->> (append names aliases)
                               -uniq
                               ;; Sort the keywords longest-first to work around what seems to be an
                               ;; obscure bug in `peg': when one keyword is a substring of another,
                               ;; and the shorter one is listed first, the shorter one fails to match.
                               (-sort (-on #'> #'length))))
         (pexs `((query (and (* [blank]) (+ (and term (* [blank])))))
                 (term (or (and negation (list positive-term)
                                ;; This is a bit confusing, but it seems to work.  There's probably a better way.
                                `(pred -- (list 'not (car pred))))
                           positive-term empty-quote))
                 (positive-term (or (and predicate-with-args `(pred args -- (cons (intern pred) args)))
                                    (and predicate-without-args `(pred -- (list (intern pred))))
                                    (and plain-string `(s -- (list org-ql-default-predicate s)))))
                 (plain-string (or quoted-arg unquoted-arg))
                 (predicate-with-args (substring predicate) ":" args)
                 (predicate-without-args (substring predicate) ":")
                 (predicate (or ,@predicate-names))
                 (args (list (+ (and (or keyword-arg quoted-arg unquoted-arg) (opt separator)))))
                 (keyword-arg (and keyword "=" `(kw -- (intern (concat ":" kw)))))
                 (keyword (substring (+ (not (or separator "=" "\"" (syntax-class whitespace))) (any))))
                 (quoted-arg "\"" (substring (+ (not (or separator "\"")) (any))) "\"")
                 (unquoted-arg (substring (+ (not (or separator "\"" (syntax-class whitespace))) (any))))
                 (empty-quote
                  ;; This avoids aborting parsing or signaling an
                  ;; error if the user types in two successive
                  ;; quotation marks while typing a query (e.g. when
                  ;; using electric-pair-mode).
                  "\"\"")
                 (negation "!")
                 (separator "," )))
         (closure (lambda (input &optional boolean)
                    "Return query parsed from plain query string INPUT.
  Multiple predicate-names are combined with BOOLEAN (default: `and')."
                    ;; HACK: Silence unused lexical variable warnings.
                    (ignore predicates predicate-names names aliases)
                    (unless (s-blank-str? input)
                      (let* ((boolean (or boolean 'and))
                             (parsed-sexp
                              (with-temp-buffer
                                (insert input)
                                (goto-char (point-min))
                                ;; Copied from `peg-parse'.  There is no function in `peg' that
                                ;; returns a matcher function--every entry point is a macro,
                                ;; which means that, since we define our PEG rules at runtime when
                                ;; predicate-names are defined, we either have to use `eval', or we
                                ;; have to borrow some code.  It ends up that we only have to
                                ;; borrow this `with-peg-rules' call, which isn't too bad.
                                (eval `(with-peg-rules ,pexs
                                         (peg-run (peg ,(caar pexs))
                                                  (lambda (failures)
                                                    (when org-ql-signal-peg-failure
                                                      (peg-signal-failure failures)))))))))
                        (pcase parsed-sexp
                          (`(,one-predicate) one-predicate)
                          (`(,_ . ,_) (cons boolean (reverse parsed-sexp)))
                          (_ nil)))))))
    (fset 'org-ql--query-string-to-sexp closure)))

;;;;; Predicate definition

;; HACK: These functions *will* be defined at runtime, so we silence
;; compiler warnings about them:
(declare-function org-ql--normalize-query "ext:org-ql" (query) t)
(declare-function org-ql--query-preamble "ext:org-ql" (query) t)

(defvar org-ql-defpred-defer nil
  "Defer expensive function redefinitions when defining predicates.
When non-nil, defining a predicate with `org-ql-defpred' does not
cause the functions `org-ql--normalize-query',
`org-ql--query-preamble', and `org-ql--query-string-to-sexp' to
be redefined.  These functions must be redefined in order to
account for new predicates, but when defining many
predicates (like at load time), that may be deferred for
performance (after which those functions should be updated
manually; see the definition of `org-ql-defpred').")

;; Yes, these two functions are a little hairy: `pcase' is challenging
;; enough, but splicing forms into one is something else.  But it's
;; worth it to do this ugly stuff here, in one place, so the
;; `org-ql-defpred' macro becomes easy to use.

(defun org-ql--define-normalize-query-fn (predicates)
  "Define function `org-ql--normalize-query' for PREDICATES.
PREDICATES should be the value of `org-ql-predicates'."
  (let ((normalizer-patterns (->> predicates
                                  (--map (plist-get (cdr it) :normalizers))
                                  (-flatten-n 1))))
    (fset 'org-ql--normalize-query
          (byte-compile
           `(lambda (query)
              "Return normalized form of QUERY expression.
This function is defined by calling
`org-ql--define-normalize-query-fn', which uses normalizer forms
defined in `org-ql-predicates' by calling `org-ql-defpred'."
              (cl-labels ((rec (element)
                            (pcase element
                              (`(or . ,clauses) `(or ,@(mapcar #'rec clauses)))
                              (`(and . ,clauses) `(and ,@(mapcar #'rec clauses)))
                              (`(not . ,clauses) `(not ,@(mapcar #'rec clauses)))
                              (`(when ,condition . ,clauses) `(when ,(rec condition)
                                                                ,@(mapcar #'rec clauses)))
                              (`(unless ,condition . ,clauses) `(unless ,(rec condition)
                                                                  ,@(mapcar #'rec clauses)))
                              ;; TODO: Combine (regexp) when appropriate (i.e. inside an OR, not an AND).
                              ((pred stringp) `(,org-ql-default-predicate ,element))

                              ,@normalizer-patterns

                              ;; Any other form: passed through unchanged.
                              (_ element))))
                ;; Repeat normalization until result doesn't change (limiting to 10 in case of an infinite-loop bug).
                (cl-loop with limit = 10 and count = 0
                         for new-query = (rec query)
                         until (equal new-query query)
                         do (progn
                              (setf query new-query)
                              (when (eq (cl-incf count) limit)
                                (error "Query normalization limit exceeded: QUERY:%S" query)))
                         finally return (org-ql--coalesce-ands new-query))))))))

(defun org-ql--define-query-preamble-fn (predicates)
  "Define function `org-ql--query-preamble' for PREDICATES.
PREDICATES should be the value of `org-ql-predicates'."
  ;; NOTE: I don't how the `list' symbol ends up in the list, but anyway...
  (let* ((preamble-patterns
          (-flatten-n 1 (-non-nil
                         ;; NOTE: Using -let instead of pcase-let here because I can't make map 2.1 install in the test sandbox.
                         (--map (-let* (((&plist :preambles) (cdr it)))
                                  (--map (pcase-let* ((`(,pattern ,exp) it))
                                           `(,pattern
                                             (-let* (((&plist :regexp :case-fold :query) ,exp))
                                               (setf org-ql-preamble regexp
                                                     preamble-case-fold case-fold)
                                               ;; NOTE: Even when `predicate' is nil, it must be returned in the pcase form.
                                               query)))
                                         preambles))
                                predicates)))))
    (fset 'org-ql--query-preamble
          `(lambda (query)
             "Return preamble data plist for QUERY.
The plist has the following keys:

  :preamble Regexp to search the Org buffer for to find potential
            matches.  If the regexp doesn't guarantee a match to
            QUERY, the :query key should be an appropriate query
            expression to test against the heading (i.e. usually
            QUERY again).

  :preamble-case-fold What to bind variable `case-fold-search' to
                      around the regexp search.

  :query The query expression to use instead of QUERY (or QUERY
         again, when appropriate).

This function is defined by calling
`org-ql--define-query-preamble-fn', which uses preamble forms
defined in `org-ql-predicates' by calling `org-ql-defpred'."
             (pcase org-ql-use-preamble
               ('nil (list :query query :preamble nil))
               (_ (let ((preamble-case-fold t)
                        org-ql-preamble)
                    (cl-labels ((rec (element)
                                  (or (when org-ql-preamble
                                        ;; Only one preamble is allowed
                                        element)
                                      (pcase element
                                        (`(or ,element)
                                         ;; A predicate with a single name: unwrap the OR.  (Pcase doesn't like
                                         ;; "one-armed ORs", giving a "Please avoid it" compilation error.)
                                         element)

                                        ,@preamble-patterns

                                        (`(and . ,rest)
                                         (let ((clauses (mapcar #'rec rest)))
                                           `(and ,@(-non-nil clauses))))
                                        (_ element)))))
                      (setq query (pcase (mapcar #'rec (list query))
                                    ((or `(nil)
                                         `((nil))
                                         `((and))
                                         `((or)))
                                     t)
                                    (`(t) t)
                                    (query (-flatten-n 1 query))))
                      (list :query query :preamble org-ql-preamble :preamble-case-fold preamble-case-fold)))))))
    ;; For some reason, byte-compiling the backquoted lambda form directly causes a warning
    ;; that `query' refers to an unbound variable, even though that's not the case, and the
    ;; function still works.  But to avoid the warning, we byte-compile it afterward.
    (byte-compile 'org-ql--query-preamble)))

(cl-defmacro org-ql-defpred (name args docstring &key body preambles normalizers coalesce)
  "Define an Org QL selector predicate `org-ql--predicate-NAME'.
NAME may be a symbol or a list of symbols: if a list, the first
is used as NAME and the rest are aliases.  A function is only
created for NAME, not for aliases, so a normalizer should be used
to replace aliases with NAME in queries (keep reading).

ARGS is a `cl-defun'-style argument list.  DOCSTRING is the
function's docstring.

BODY is the body of the predicate.  It will be evaluated with
point on the beginning of an Org heading and should return
non-nil if the heading's entry is a match.

PREAMBLES and NORMALIZERS are lists of `pcase' forms matched
against Org QL query sexps.  They are spliced into `pcase' forms
in the definitions of the functions `org-ql--query-preamble' and
`org-ql--normalize-query', which see.  Those functions are
redefined when this macro is expanded, unless variable
`org-ql-defpred-defer' is non-nil, in which case those functions
should be redefined manually after defining predicates by calling
`org-ql--define-query-preamble-fn' and `org-ql--define-normalize-query-fn'.

NORMALIZERS are used to normalize query expressions to standard
forms.  For example, when the predicate has aliases, the aliases
should be replaced with predicate names using a normalizer.
Also, predicate arguments may be put into a more optimal form so
that the predicate has less work to do at query time.  NOTE:
Normalizers are applied to a query repeatedly until the query is
fully normalized, so normalizers should be carefully written to
avoid infinite loops.

PREAMBLES refer to regular expressions which may be used to
search through a buffer directly to a potential match rather than
testing the predicate body on each heading.  (Naming things is
hard.)  In each `pcase' form in PREAMBLES, the `pcase'
expression (not the pattern) should be a plist with the following
keys, each value of which should be an expression which may refer
to variables bound in the pattern:

  :regexp     Regular expression which searches directly to a
              potential match.

  :case-fold  Bound to `case-fold-search' around the regexp search.

  :query      Expression which should replace the query expression,
              or \\+`query' if it should not be changed (e.g. if the
              regexp is insufficient to determine whether a
              heading matches, in which case the predicate's body
              needs to be tested on the heading).  If the regexp
              guarantees a match, this may be simply t, leaving the
              query expression with no work to do, which improves
              performance.

When COALESCE is t, multiple calls to this predicate within a
boolean AND query clause may be combined into a single call to
this predicate (so it is expected that this predicate treats
multiple arguments as being boolean AND'ed together; if it does
not, then it should not be set to coalesce).

This argument may also be a function called to do coalescing of
two predicate expressions.  It is called with two arguments: the
list of already-coalesced arguments to an expression, and the
list of arguments to the call being coalesced (note that a
query's arguments are normalized before the query is coalesced).
If it returns nil, the expression is not coalesced; otherwise, it
should return a new list of arguments coalescing the given
arguments, with new arguments being first.  (Using a
predicate-specific function to coalesce arguments is useful when,
e.g. a predicate takes keyword arguments, so arguments to
multiple calls can't be simply appended.)

For convenience, within the `pcase' patterns, the symbol
\\+`predicate-names' is a special form which is replaced with a
pattern matching any of the predicate's name and aliases.  For
example, if NAME were:

  (heading h)

Then if NORMALIZERS were:

  ((\\=`(,predicate-names . ,args)
    \\=`(heading ,@args)))

It would be expanded to:

  ((\\=`(,(or \\='heading \\='h) . ,args)
    \\=`(heading ,@args)))"
  ;; FIXME: Update defpred tutorial to include :coalesce.

  ;; NOTE: The debug form works, completely!  For example, use `edebug-defun'
  ;; on the `heading' predicate, then evaluate this form:
  ;; (let* ((query '(heading "HEADING"))
  ;;        (normalized (org-ql--normalize-query query))
  ;;        (preamble (org-ql--query-preamble normalized)))
  ;;   (list :query query
  ;;         :normalized normalized
  ;;         :preamble preamble))
  (declare (debug ([&or symbolp listp] listp stringp
                   &rest [&or [":coalesce" form]
                              [":body" def-body]
                              [":normalizers" (&rest (sexp def-body))]
                              [":preambles" (&rest (sexp def-body))]]))
           (indent defun))
  (let* ((aliases (when (listp name)
                    (cdr name)))
         (name (cl-etypecase name
                 (list (car name))
                 (atom name)))
         (fn-name (intern (concat "org-ql--predicate-" (symbol-name name))))
         (predicate-name (intern (symbol-name name)))
         (predicate-names (delq nil (cons predicate-name aliases)))
         (normalizers (cl-sublis (list (cons 'predicate-names (cons 'or (--map (list 'quote it) predicate-names))))
                                 normalizers))
         (preambles (cl-sublis (list (cons 'predicate-names (cons 'or (--map (list 'quote it) predicate-names))))
                               preambles)))
    ;; Ensure that a normalizer is defined if aliases are defined.
    ;; (This doesn't ensure that the normalizer does what is intended,
    ;; but it's at least some safeguard.)
    ;; TODO: Add per-predicate test in suite to ensure that aliases
    ;; are normalized to the full name.
    (when (and aliases (not normalizers))
      (user-error "org-ql-defpred: Aliases defined for predicate `%s' without a normalizer (which must be used to replace aliases with the predicate's full name)" name))
    `(progn
       (cl-defun ,fn-name ,args ,docstring ,body)
       ;; SOMEDAY: Use `map-elt' here, after map 2.1 can be automatically installed in CI sandbox...
       (setf (alist-get ',predicate-name org-ql-predicates)
             `(:name ,',name :aliases ,',aliases :fn ,',fn-name :docstring ,(\, docstring) :args ,',args
                     :normalizers ,',normalizers :preambles ,',preambles
                     :coalesce ,,coalesce))
       (unless org-ql-defpred-defer
         ;; Reversing preserves the order in which predicates were defined.
         (org-ql--define-normalize-query-fn (reverse org-ql-predicates))
         (org-ql--define-query-preamble-fn (reverse org-ql-predicates))
         (org-ql--def-query-string-to-sexp-fn (reverse org-ql-predicates))))))

(defmacro org-ql--normalize-from-to-on (&rest body)
  "For internal use.
Expands into a form that processes arguments to timestamp-related
predicates and evaluates BODY, which is expected to evaluate to a
timestamp-related query predicate form.  It expects the variable
`rest' to be bound to a list of the predicate's arguments.  In
BODY, these variables are bound to normalized values, when
applicable: `from', `to', `on', `type'.  If `rest' includes a
`:with-time' argument, it is automatically added to BODY's
result form."
  ;; Several attempts to use `cl-macrolet' and `cl-symbol-macrolet' failed, so I resorted
  ;; to this top-level macro.  It will do for now.  This is a bit messy, but it's better
  ;; to do it in one macro in one place than in every predicate's definition.
  (declare (indent defun))
  ;; NOTE: Had to use `-let' instead of `pcase-let' here due to inexplicable errors
  ;; that only happen on GitHub CI and never happen locally.  Possibly something to
  ;; do with the version of map.el being used (although it happens locally even in
  ;; a clean sandbox, which should produce the same result as on CI).  Maybe the
  ;; real fix would be to make makem.sh support dependency versions...
  `(-let (((&keys :from :to :on :type) rest)
          (result))
     (ignore type) ;; Only (ts) uses it.
     (pcase rest
       (`(,(and num (pred numberp)) . ,rest*)
        (setf on num
              rest rest*)))
     (when on
       (setq from on
             to on))
     (when from
       (setq from (pcase from
                    ("-"
                     ;; Ignore, because it means the user is typing a negative number.
                     nil)
                    ((or 'today "today") (->> (ts-now)
                                              (ts-apply :hour 0 :minute 0 :second 0)))
                    ((pred numberp) (->> (ts-now)
                                         (ts-adjust 'day from)
                                         (ts-apply :hour 0 :minute 0 :second 0)))
                    ((and (pred stringp)
                          (guard (ignore-errors (cl-parse-integer from))))
                     ;; The `pcase' `let' pattern doesn't bind values in the
                     ;; body forms, so we have to parse the integer again.
                     (->> (ts-now)
                          (ts-adjust 'day (cl-parse-integer from))
                          (ts-apply :hour 0 :minute 0 :second 0)))
                    ((pred stringp) (ts-parse-fill 'begin from))
                    ((pred ts-p) from))))
     (when to
       (setq to (pcase to
                  ("-"
                   ;; Ignore, because it means the user is typing a negative number.
                   nil)
                  ((or 'today "today") (->> (ts-now)
                                            (ts-apply :hour 23 :minute 59 :second 59)))
                  ((pred numberp) (->> (ts-now)
                                       (ts-adjust 'day to)
                                       (ts-apply :hour 23 :minute 59 :second 59)))
                  ((and (pred stringp)
                        (guard (ignore-errors (cl-parse-integer to))))
                   ;; The `pcase' `let' pattern doesn't bind values in the
                   ;; body forms, so we have to parse the integer again.
                   (->> (ts-now)
                        (ts-adjust 'day (cl-parse-integer to))
                        (ts-apply :hour 23 :minute 59 :second 59)))
                  ((pred stringp) (ts-parse-fill 'end to))
                  ((pred ts-p) to))))
     (setf result (progn ,@body))
     ;; Add :with-time to the result when necessary, but only when it's not already present.
     ;; (This is messy, but we do this to make predicate definition and normalization easier.)
     (when (and (plist-member rest :with-time)
                (not (memq :with-time result)))
       (setf result (append result (list :with-time (plist-get rest :with-time)))))
     ;; Remove certain keyword arguments whose value is nil.  This is
     ;; a little bit ugly, but it allows us to normalize queries more
     ;; easily, without leaving useless arguments in the result.
     (dolist (property '(:from :to :on :type))
       (when (plist-member (cdr result) property)
         (unless (plist-get (cdr result) property)
           (plist-put (cdr result) property 'delete-this)
           (setf (cdr result) (delq property (cdr result)))
           (setf (cdr result) (delq 'delete-this (cdr result))))))
     result))

;;;;;; Predicates

;; Improve load time by deferring the per-predicate preamble- and normalizer-function
;; redefinitions until all of the predicates have been defined.
(setf org-ql-defpred-defer t)

(org-ql-defpred blocked ()
  "Return non-nil if entry is blocked.
Calls `org-entry-blocked-p', which see."
  :body (org-entry-blocked-p))

(org-ql-defpred (category c) (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES."
  :normalizers ((`(,predicate-names . ,rest)
                 `(category ,@rest)))
  :body (when-let ((category (org-get-category (point))))
          (cl-typecase categories
            (null t)
            (otherwise (member category categories)))))

(org-ql-defpred done ()
  "Return non-nil if entry's TODO keyword is in `org-done-keywords'."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  :body (or (apply #'org-ql--predicate-todo org-done-keywords)))

(defalias 'org-ql--duration-to-minutes
  ;; TODO: Remove when compatibility with Org 9.0 is dropped.
  (cond ((fboundp 'org-duration-to-minutes) #'org-duration-to-minutes)
        ((fboundp 'org-duration-string-to-minutes) #'org-duration-string-to-minutes)
        (t (error "org-ql: Unable to define alias `org-ql-search--link-heading-search-string'.  Please report this as a bug")))
  "Return DURATION string as a number of minutes.
For compatibility, since Org 9.1 deprecated
`org-duration-string-to-minutes', replacing it with
`org-duration-to-minutes', which seems to return floats instead
of integers.")

(org-ql-defpred effort (&optional effort-or-comparator effort)
  "Return non-nil if current heading's effort property matches arguments.
The following forms are accepted:

  (effort DURATION): Matches if effort is DURATION.
  (effort DURATION DURATION): Matches if effort is between DURATIONs, inclusive.
  (effort COMPARATOR DURATION): Matches if effort compares to DURATION with
                                COMPARATOR.

COMPARATOR may be `<', `<=', `>', or `>='.  DURATION should be an
Org effort string, like \"5\" or \"0:05\"."
  :normalizers ((`(,predicate-names
                   . ,(and args (guard (cl-loop for arg in args
                                                thereis (or (stringp arg)
                                                            (memq arg '(< <= > >= =)))))))
                 ;; Arguments could be given as strings (e.g. from a non-Lisp query).
                 `(effort ,@(--map (pcase-exhaustive it
                                     ((or "<" "<=" ">" ">=" "=")
                                      (intern it))
                                     ((pred stringp) (org-ql--duration-to-minutes it))
                                     ((pred numberp) it)
                                     ((or '< '<= '> '>= '=)
                                      ;; FIXME: This same treatment probably needs to be done in the (level) predicate's normalizer (which I would have caught had I already written tests for it).
                                      `',it))
                                   args))))
  ;; NOTE: We always test the body, because we must ensure that the
  ;; ":effort:" string found is actually an entry property.
  :preambles ((`(,predicate-names ,_comparator-or-num ,_num)
               ;; Since we can't compare this using regexps, we just check for what looks like a valid effort.
               ;; FIXME: Are decimal efforts allowed, or only colon-separated HH:MM?
               (list :regexp (rx-to-string `(seq bol (0+ blank) ":effort:" (1+ blank) (1+ (or digit "." ":"))) t)
                     :case-fold t :query query))
              (`(,predicate-names ,num)
               ;; Testing equality to a certain effort.
               (let* ((with-colon (org-duration-from-minutes num))
                      (without-colon (number-to-string num))
                      (without-colon-and-decimal
                       (if (string-suffix-p ".0" without-colon)
                           (replace-regexp-in-string (rx ".0" eos) "" without-colon t t)
                         "")))
                 (list :regexp (rx-to-string `(seq bol (0+ blank) ":effort:" (1+ blank)
                                                   (or ,with-colon ,without-colon ,without-colon-and-decimal) (or blank eol)) t)
                       :case-fold t :query query))))
  :body (let ((entry-effort (org-entry-get (point) "effort"))
              entry-effort-minutes)
          (when entry-effort
            (setf entry-effort-minutes (org-ql--duration-to-minutes entry-effort))
            (pcase effort-or-comparator
              ('nil
               ;; Any effort.  (NOTE: This must come before the symbolp clause, because `symbolp' is non-nil for nil!)
               t)
              ((pred numberp) (pcase effort
                                ('nil ;; Equality
                                 (= effort-or-comparator entry-effort-minutes))
                                ((pred numberp) ;; Between two levels
                                 (<= effort-or-comparator entry-effort-minutes effort))))
              ((pred symbolp) ;; Compare with function
               (funcall effort-or-comparator entry-effort-minutes effort))))))

(org-ql-defpred habit ()
  "Return non-nil if entry is a habit."
  :preambles ((`(,predicate-names)
               (list :regexp (rx bol (0+ space) ":STYLE:" (1+ space) "habit" (0+ space) eol))))
  :body (org-is-habit-p))

(org-ql-defpred (heading h) (&rest _strings)
  "Return non-nil if current entry's heading matches all STRINGS.
Matching is done case-insensitively."
  :coalesce t
  :normalizers ((`(,predicate-names . ,args)
                 ;; NOTE: Each string argument must be converted to a regexp
                 ;; for testing by the body, so we just normalize to the
                 ;; `heading-regexp' predicate, leaving this predicate as
                 ;; one that merely regexp-quotes its arguments.
                 `(heading-regexp ,@(mapcar #'regexp-quote args)))))

(org-ql-defpred (heading-regexp h*) (&rest regexps)
  "Return non-nil if current entry's heading matches all REGEXPS (regexp strings).
Matching is done case-insensitively."
  :coalesce t
  :normalizers ((`(,predicate-names . ,args)
                 ;; "h" alias.
                 `(heading-regexp ,@args)))
  ;; MAYBE: Adjust regexp to avoid matching in tag list.
  :preambles ((`(,predicate-names)
               ;; This clause protects against the case in which the
               ;; arguments are nil, which would cause an error in
               ;; `rx-to-string' in other clauses.  This can happen
               ;; with `org-ql-completing-read', e.g. when the input
               ;; is "h:" while the user is typing.
               (list :regexp (rx bol (1+ "*") (1+ blank) (0+ nonl))
                     :case-fold t :query query))
              (`(,predicate-names ,regexp)
               ;; Only one regexp: match with preamble, then let predicate confirm (because
               ;; the match could be in e.g. the tags rather than the heading text).
               (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ blank) (0+ nonl)
                                                 (regexp ,regexp))
                                           'no-group)
                     :case-fold t :query query))
              (`(,predicate-names . ,regexps)
               ;; Multiple regexps: use preamble to match against first
               ;; regexp, then let the predicate match the rest.
               (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ blank) (0+ nonl)
                                                 (regexp ,(car regexps)))
                                           'no-group)
                     :case-fold t :query query)))
  ;; TODO: In Org 9.2+, `org-get-heading' takes 2 more arguments.
  :body (let ((heading (org-get-heading 'no-tags 'no-todo))
              (case-fold-search t))
          (--all? (string-match it heading) regexps)))

(org-ql-defpred level (level-or-comparator &optional level)
  "Return non-nil if current heading's outline level matches arguments.
The following forms are accepted:

  (level NUMBER): Matches if heading level is NUMBER.
  (level NUMBER NUMBER): Matches if heading level is equal to or between
                         NUMBERs.
  (level COMPARATOR NUMBER): Matches if heading level compares to NUMBER with
                             COMPARATOR.

COMPARATOR may be `<', `<=', `>', or `>='."
  :normalizers ((`(,predicate-names . ,args)
                 ;; Arguments could be given as strings (e.g. from a non-Lisp query).
                 `(level ,@(--map (pcase it
                                    ((or "<" "<=" ">" ">=" "=")
                                     (intern it))
                                    ((pred stringp) (string-to-number it))
                                    (_ it))
                                  args))))
  :preambles ((`(,predicate-names)
               ;; This clause protects against the case in which the
               ;; arguments are nil, which would cause an error in
               ;; `rx-to-string' in other clauses.  This can happen
               ;; with `org-ql-completing-read', e.g. when the input
               ;; is "h:" while the user is typing.
               (list :regexp (rx bol (1+ "*") " ")
                     :case-fold t))
              (`(,predicate-names ,comparator-or-num ,num)
               (let ((repeat (pcase comparator-or-num
                               ('< `(repeat 1 ,(1- num) "*"))
                               ('<= `(repeat 1 ,num "*"))
                               ('> `(>= ,(1+ num) "*"))
                               ('>= `(>= ,num "*"))
                               ((pred integerp) `(repeat ,comparator-or-num ,num "*")))))
                 (list :regexp (rx-to-string `(seq bol ,repeat " ") t)
                       :case-fold t)))
              (`(,predicate-names ,num)
               (list :regexp (rx-to-string `(seq bol (repeat ,num "*") " ") t)
                     :case-fold t)))
  ;; NOTE: It might be necessary to take into account `org-odd-levels'; see docstring for
  ;; `org-outline-level'.
  :body (when-let ((outline-level (org-outline-level)))
          (pcase level-or-comparator
            ((pred numberp) (pcase level
                              ('nil ;; Equality
                               (= outline-level level-or-comparator))
                              ((pred numberp) ;; Between two levels
                               (>= level-or-comparator outline-level level))))
            ((pred symbolp) ;; Compare with function
             (funcall level-or-comparator outline-level level)))))

(org-ql-defpred link (&rest args)
  ;; User-facing argument form: (&optional description-or-target &key description target regexp-p).
  "Return non-nil if current heading contains a link matching arguments.
DESCRIPTION-OR-TARGET is matched against the link's description
and target.  Alternatively, one or both of DESCRIPTION and TARGET
may be matched separately.  Without arguments, return non-nil if
any link is found."
  ;; NOTE: It would be preferable to avoid this manual argument parsing every time the predicate
  ;; is called, but pre-processing it to a normal form gets complicated with the preamble and
  ;; pre-processing, because we don't want to display a query like "(link :description-or-target
  ;; "FOO")" in the view header, which would be ugly.  So, since preambles are expected to be
  ;; enabled nearly all of the time, in which case this function won't be called anyway, it's
  ;; probably not worth rewriting code all over the place to fix this.
  :preambles ((`(,predicate-names)
               ;; Match a link with a target and optionally a description.
               (list :regexp (org-ql--link-regexp :target ".*")))
              (`(,predicate-names ,(and description-or-target
                                        (guard (not (keywordp description-or-target))))
                                  . ,plist)
               (list :regexp (org-ql--link-regexp :description-or-target
                                                  (if (plist-get plist :regexp-p)
                                                      description-or-target
                                                    (regexp-quote description-or-target))))
               nil)
              (`(,predicate-names . ,plist)
               (list :regexp (org-ql--link-regexp
                              :description
                              (when (plist-get plist :description)
                                (if (plist-get plist :regexp-p)
                                    (plist-get plist :description)
                                  (regexp-quote (plist-get plist :description))))
                              :target (when (plist-get plist :target)
                                        (if (plist-get plist :regexp-p)
                                            (plist-get plist :target)
                                          (regexp-quote (plist-get plist :target))))))
               nil))
  :body (let* (plist description-or-target description target regexp-p)
          (if (not (keywordp (car args)))
              (setf description-or-target (car args)
                    plist (cdr args))
            (setf plist args))
          (setf description (plist-get plist :description)
                target (plist-get plist :target)
                regexp-p (plist-get plist :regexp-p))
          (unless regexp-p
            ;; NOTE: It would also be preferable to avoid regexp-quoting every time this predicate
            ;; is called.  Ideally that would be handled in the query pre-processing step.  However,
            ;; handling that properly, in combination with preparing the query preamble and whether
            ;; REGEXP-P is enabled, is also complicated, so let's not.
            (when description-or-target
              (setf description-or-target (regexp-quote description-or-target)))
            (when description
              (setf description (regexp-quote description)))
            (when target
              (setf target (regexp-quote target))))
          (when (save-excursion
                  (re-search-forward org-ql-link-regexp (org-entry-end-position) t))
            (pcase description-or-target
              ('nil (and (or (null target)
                             (string-match-p target (match-string 1)))
                         (or (null description)
                             (string-match-p description (match-string org-ql-link-description-group)))))
              (_ (if (and description target)
                     (and (string-match-p target (match-string 1))
                          (string-match-p description (match-string org-ql-link-description-group)))
                   (or (string-match-p description-or-target (match-string 1))
                       (string-match-p description-or-target
                                       (match-string org-ql-link-description-group)))))))))

(org-ql-defpred (rifle smart) (&rest strings)
  "Return non-nil if each of strings is found in the entry or its outline path.
Works like `org-rifle'.  This is probably the most useful,
intuitive, general-purpose predicate."
  ;; NOTE: This predicate advertises that it takes strings, but they
  ;; are normalized to regexps.  Because of that, we must use a
  ;; coalescing function.
  :coalesce (lambda (coalesced-args current-args)
              (plist-put coalesced-args :regexps
                         (list 'quote (append (cadr (plist-get coalesced-args :regexps))
                                              (cadr (plist-get current-args :regexps))))))
  :normalizers ((`(,predicate-names . ,(and rest (guard (cl-every #'stringp rest))))
                 ;; If this doesn't match, it's already normalized.
                 `(rifle :regexps ',(mapcar #'regexp-quote rest))))
  :preambles ((`(,predicate-names :regexps ',regexps)
               (list :regexp (rx-to-string `(seq bow (or ,@(mapcar (lambda (s) `(regexp ,s)) regexps))))
                     :case-fold t :query query)))
  :body (cl-loop for regexp in (plist-get strings :regexps)
                 always (or (org-ql--predicate-regexp regexp)
                            (org-ql--predicate-outline-path regexp))))

;; MAYBE: Preambles for outline-path predicates.  Not sure if possible without complicated logic.
;; FIXME: These preds say they accept regexps but the strings get regexp-quoted.  They should probably just take strings.

(org-ql-defpred (outline-path olp) (&rest regexps)
  "Return non-nil if current node's outline path matches all of REGEXPS.
Each string is compared as a regexp to each element of the node's
outline path with `string-match'.  For example, if an entry's
outline path were \"Food/Fruits/Grapes\", it would match any of
the following queries:

  (olp \"Food\")
  (olp \"Fruits\")
  (olp \"Food\" \"Fruits\")
  (olp \"Fruits\" \"Grapes\")
  (olp \"Food\" \"Grapes\")"
  :coalesce t
  :normalizers ((`(,predicate-names . ,strings)
                 ;; Regexp quote headings.
                 `(org-ql--predicate-outline-path ,@(mapcar #'regexp-quote strings))))
  :body (let ((entry-olp (org-ql--value-at (point) #'org-ql--outline-path)))
          (cl-loop for h in regexps
                   always (cl-member h entry-olp :test #'string-match))))

(org-ql-defpred (outline-path-segment olps) (&rest regexps)
  "Return non-nil if current node's outline path matches segment REGEXPS.
Matches REGEXPS as a contiguous segment of the outline path.
Each regexp is compared to each element of the node's outline
path with `string-match'.  For example, if an entry's outline
path were \"Food/Fruits/Grapes\", it would match any of the
following queries:

  (olp \"Food\")
  (olp \"Fruit\")
  (olp \"Food\" \"Fruit\")
  (olp \"Fruit\" \"Grape\")

But it would not match the following, because they do not match a
contiguous segment of the outline path:

  (olp \"Food\" \"Grape\")"
  ;; MAYBE: Allow anchored matching.
  :normalizers ((`(,(or 'outline-path-segment 'olps) . ,strings)
                 ;; Regexp quote headings.
                 `(outline-path-segment ,@(mapcar #'regexp-quote strings))))
  :body (org-ql--infix-p regexps (org-ql--value-at (point) #'org-ql--outline-path)))

(org-ql-defpred path (&rest regexps)
  "Return non-nil if current heading's buffer's file path matches any of REGEXPS.
Without arguments, return non-nil if buffer is file-backed."
  ;; FIXME: This should AND the regexps together, not OR them.
  :body (when (buffer-file-name)
          (cl-typecase regexps
            (null t)
            (list (cl-loop for regexp in regexps
                           thereis (string-match regexp (buffer-file-name)))))))

(org-ql-defpred priority (&rest args)
  "Return non-nil if current heading has a certain priority.
ARGS may be either a list of one or more priority letters as
strings, or a comparator function symbol followed by a priority
letter string.  For example:

  (priority \"A\")
  (priority \"A\" \"B\")
  (priority '>= \"B\")

Note that items without a priority cookie never match this
predicate (while Org itself considers items without a cookie to
have the default priority, which, by default, is equal to
priority B)."
  ;; NOTE: This treats priorities differently than Org proper treats them, in that
  ;; items without a priority cookie never match this predicate, even though Org
  ;; itself would consider un-cookied items to have a default numeric priority
  ;; value.  We do this because it doesn't seem very useful or intuitive for a
  ;; query like (priority "B") to match an item that has no priority cookie.
  ;; TODO: Convert priority arg(s) to numeric values in pre-processing.
  :normalizers
  ((`(,predicate-names ,(and (or '= '< '> '<= '>=) comparator) ,letter)
    ;; Quote comparator.
    `(priority ',comparator ,letter)))

  :preambles
  (;; NOTE: This only accepts A, B, or C.  I haven't seen
   ;; other priorities in the wild, so this will do for now.
   (`(,predicate-names)
    ;; Any priority cookie.
    (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ blank) (0+ nonl) "[#" (in "ABC") "]") t)))
   (`(,predicate-names ,(and (or ''= ''< ''> ''<= ''>=) comparator) ,letter)
    ;; Comparator and priority letter.
    ;; NOTE: The double-quoted comparators.  See below.
    (let* ((priority-letters '("A" "B" "C"))
           (index (-elem-index letter priority-letters))
           ;; NOTE: Higher priority == lower number.
           ;; NOTE: Because we need to support both preamble-based queries and
           ;; regular predicate ones, we work around an idiosyncrasy of query
           ;; pre-processing by accepting both quoted and double-quoted comparator
           ;; function symbols.  Not the most elegant solution, but it works.
           (priorities (s-join "" (pcase comparator
                                    ((or '= ''=) (list letter))
                                    ((or '> ''>) (cl-subseq priority-letters 0 index))
                                    ((or '>= ''>=) (cl-subseq priority-letters 0 (1+ index)))
                                    ((or '< ''<) (cl-subseq priority-letters (1+ index)))
                                    ((or '<= ''<=) (cl-subseq priority-letters index))))))
      (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ blank) (optional (1+ upper) (1+ blank))
                                        "[#" (in ,priorities) "]") t))))
   (`(,predicate-names . ,letters)
    ;; One or more priorities.
    ;; MAYBE: Disable case-folding.
    (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ blank)
                                      (optional (1+ upper) (1+ blank))
                                      "[#" (or ,@letters) "]") t))))
  :body
  (when-let* ((item-priority (save-excursion
                               (save-match-data
                                 ;; TODO: Is the save-match-data above necessary?
                                 (when (and (looking-at org-heading-regexp)
                                            (save-match-data
                                              (string-match org-priority-regexp (match-string 0))))
                                   (org-get-priority (match-string 0)))))))
    ;; Item has a priority: compare it.
    (pcase args
      ('nil
       ;; No arguments: return non-nil.
       t)
      (`(,(and (or '= '< '> '<= '>=) comparator) ,priority-arg)
       ;; Comparator and priority arguments given: compare item priority using them.
       (funcall comparator item-priority
                (* 1000 (- org-lowest-priority (string-to-char priority-arg)))))
      (_
       ;; List of priorities given as arguments: compare each of them to item priority using =.
       (cl-loop for priority-arg in args
                thereis (= item-priority (* 1000 (- org-lowest-priority (string-to-char priority-arg)))))))))

(org-ql-defpred property (property &optional value &key inherit)
  "Return non-nil if current entry has PROPERTY, and optionally VALUE.
If INHERIT is nil, only match entries with PROPERTY set on the
entry; if t, also match entries with inheritance.  If INHERIT is
not specified, use the Boolean value of
`org-use-property-inheritance', which see (i.e. it is only
interpreted as nil or non-nil)."
  :normalizers ((`(,predicate-names)
                 ;; HACK: This clause protects against the case in
                 ;; which the arguments are nil, which would cause an
                 ;; error in `rx-to-string' in other clauses.  This
                 ;; can happen with `org-ql-completing-read',
                 ;; e.g. when the input is "property:" while the user
                 ;; is typing.
                 ;; FIXME: Instead of this being moot, make this
                 ;; predicate test for whether an entry has local
                 ;; properties when no arguments are given.
                 (list 'property ""))
                (`(,predicate-names ,property ,value . ,plist)
                 ;; Convert keyword property arguments to strings.  Non-sexp
                 ;; queries result in keyword property arguments (because to do
                 ;; otherwise would require ugly special-casing in the parsing).
                 (when (keywordp property)
                   (setf property (substring (symbol-name property) 1)))
                 (list 'property property value
                       :inherit (cond ((plist-member plist :inherit) (plist-get plist :inherit))
                                      ((listp org-use-property-inheritance) ''selective)
                                      (t org-use-property-inheritance)))))
  ;; MAYBE: Should case folding be disabled for properties?  What about values?
  ;; MAYBE: Support (property) without args.

  ;; NOTE: When inheritance is enabled, the preamble can't be used,
  ;; which will make the search slower.
  :preambles ((`(,predicate-names ,property ,value . ,(map :inherit))
               ;; We do NOT return nil, because the predicate still needs to be tested,
               ;; because the regexp could match a string not inside a property drawer.
               (list :regexp (unless inherit
                               (rx-to-string `(seq bol (0+ space) ":" ,property ":"
                                                   (1+ space) ,value (0+ space) eol)))
                     :query query))
              (`(,predicate-names ,property . ,(map :inherit))
               ;; We do NOT return nil, because the predicate still needs to be tested,
               ;; because the regexp could match a string not inside a property drawer.
               ;; NOTE: The preamble only matches if there appears to be a value.
               ;; A line like ":ID: " without any other text does not match.
               (list :regexp (unless inherit
                               (rx-to-string `(seq bol (0+ space) ":" ,property ":" (1+ space)
                                                   (minimal-match (1+ not-newline)) eol)))
                     :query query)))
  :body
  (pcase property
    ('nil (user-error "Property matcher requires a PROPERTY argument"))
    (_ (pcase value
         ('nil
          ;; Check that PROPERTY exists
          (org-ql--value-at
           (point) (lambda ()
                     (org-entry-get (point) property))))
         (_
          ;; Check that PROPERTY has VALUE.

          ;; TODO: Since --value-at doesn't account for inheritance,
          ;; we should generalize --tags-at to also work for property
          ;; inheritance and use it here, which should be much faster.
          (string-equal value (org-ql--value-at
                               (point) (lambda ()
                                         (org-entry-get (point) property inherit)))))))))

;; TODO: Add property-local, property-inherit, etc. to match tags predicates.
;; TODO: Add tests for property inheritance.

(org-ql-defpred (regexp r) (&rest regexps)
  "Return non-nil if current entry matches all of REGEXPS (regexp strings)."
  :coalesce t
  :normalizers ((`(,predicate-names . ,args)
                 `(regexp ,@args)))
  ;; MAYBE: Separate case-sensitive (Regexp) predicate.
  :preambles ((`(,predicate-names ,regexp)
               (list :case-fold t :regexp regexp :query t))
              (`(,predicate-names . ,regexps)
               ;; Search for first regexp, then confirm with predicate.
               (list :case-fold t :regexp (car regexps) :query query)))
  :body
  (let ((end (or (save-excursion
                   (outline-next-heading))
                 (point-max))))
    (save-excursion
      (goto-char (line-beginning-position))
      (cl-loop for regexp in regexps
               always (save-excursion
                        (re-search-forward regexp end t))))))

(org-ql-defpred src (&key regexps lang)
  "Return non-nil if current entry has an Org source block matching all REGEXPS.
If keyword argument LANG is non-nil, the block must be in that
language.  Matching is done case-insensitively."
  :coalesce (lambda (coalesced-args current-args)
              (when (or (not coalesced-args)
                        (equal (plist-get current-args :lang)
                               (plist-get coalesced-args :lang)))
                (setf coalesced-args
                      (plist-put coalesced-args :lang (plist-get current-args :lang)))
                (setf coalesced-args
                      (plist-put coalesced-args
                                 :regexps (list 'quote
                                                (append (car (delq 'quote (plist-get coalesced-args :regexps)))
                                                        ;; A bit awkward, but necessary.
                                                        (car (delq 'quote (plist-get current-args :regexps)))))))))
  :normalizers ((`(,predicate-names)
                 ;; This clause protects against the case in which the
                 ;; arguments are nil, which would cause an error in
                 ;; `rx-to-string' in other clauses.  This can happen
                 ;; with `org-ql-completing-read', e.g. when the input
                 ;; is "src:" while the user is typing.
                 (list 'src))
                ;; NOTE: The :regexps argument is a quoted list,
                ;; because we call the byte-compiler at runtime, and
                ;; without quoting, it would interpret it as a
                ;; function call.  This requires some awkwardness in
                ;; other places to deal with the quoting.
                (`(,predicate-names . ,args)
                 ;; Rewrite to use keyword args.
                 (cond ((cl-every #'stringp args)
                        ;; No keywords, only regexps.
                        `(src :regexps ',args))
                       ((and (stringp (car args)) (cl-some #'keywordp args))
                        ;; Regexp as first arg with keyword later.
                        (let* ((keyword-pos (cl-position :lang args))
                               (regexps (cl-subseq args 0 keyword-pos))
                               ;; We assume that if :lang is given, the string argument follows.
                               (lang (nth (1+ (cl-position :lang args)) args)))
                          `(src :lang ,lang
                                :regexps ',regexps)))
                       ((keywordp (car args))
                        ;; All plist args.
                        `(src ,@(delq nil
                                      (append (when (plist-get args :lang)
                                                (list :lang (plist-get args :lang)))
                                              (pcase (plist-get args :regexps)
                                                (`(quote . ,_)
                                                 ;; Already quoted: return as-is to stop further normalization.
                                                 (list :regexps (plist-get args :regexps)))
                                                (_ (list :regexps `(quote ,(plist-get args :regexps))))))))))))
  ;; NOTE: We match case-insensitively since the
  ;; "#+BEGIN_SRC/#+END_SRC" lines could be either upper- or
  ;; lowercase, as well as the language name.
  :preambles ((`(,predicate-names)
               ;; This clause protects against the case in which the
               ;; arguments are nil, which would cause an error in
               ;; `rx-to-string' in other clauses.  This can happen
               ;; with `org-ql-completing-read', e.g. when the input
               ;; is "src:" while the user is typing.
               (list :regexp (org-ql--format-src-block-regexp)
                     :case-fold t
                     ;; Always check contents with predicate.
                     :query query))
              (`(,predicate-names . ,args)
               (list :regexp (org-ql--format-src-block-regexp (plist-get args :lang))
                     :case-fold t
                     ;; Always check contents with predicate.
                     :query query)))
  :body
  (save-excursion
    (save-match-data
      (cl-loop while (re-search-forward org-babel-src-block-regexp (org-entry-end-position) t)
               thereis (when (or (not lang) (equal lang (match-string 2)))
                         (or (not regexps)
                             (save-excursion
                               (let ((contents-beg (progn
                                                     (goto-char (match-beginning 0))
                                                     (forward-line 1)
                                                     (point)))
                                     (contents-end (progn
                                                     (goto-char (match-end 0))
                                                     (pos-bol))))
                                 (cl-loop for re in regexps
                                          do (goto-char contents-beg)
                                          always (re-search-forward re contents-end t))))))))))

(org-ql-defpred (tags) (&rest tags)
  "Return non-nil if current heading has one or more of TAGS (a list of strings).
Tests both inherited and local tags."
  ;; MAYBE: -all versions for inherited and local.
  :body (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
          (-let* (((inherited local) (org-ql--tags-at (point))))
            (cl-typecase tags
              (null (or (tags-p inherited)
                        (tags-p local)))
              (otherwise (or (when (tags-p inherited)
                               (seq-intersection tags inherited))
                             (when (tags-p local)
                               (seq-intersection tags local))))))))

(org-ql-defpred (tags-all tags&) (&rest tags)
  "Return non-nil if current heading has all of TAGS (a list of strings).
Tests both inherited and local tags."
  :coalesce t
  ;; MAYBE: -all versions for inherited and local.
  :normalizers ((`(,predicate-names . ,tags)
                 `(and ,@(--map `(tags ,it) tags))))
  :body (apply #'org-ql--predicate-tags tags))

(org-ql-defpred (tags-inherited inherited-tags tags-i itags) (&rest tags)
  "Return non-nil if current heading's inherited tags include any of TAGS.
If TAGS is nil, return non-nil if heading has any inherited tags."
  :normalizers ((`(,predicate-names . ,tags)
                 `(tags-inherited ,@tags)))
  :body (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
          (-let* (((inherited _) (org-ql--tags-at (point))))
            (cl-typecase tags
              (null (tags-p inherited))
              (otherwise (when (tags-p inherited)
                           (seq-intersection tags inherited)))))))

(org-ql-defpred (tags-local local-tags tags-l ltags) (&rest tags)
  "Return non-nil if current heading's local tags include any of TAGS.
If TAGS is nil, return non-nil if heading has any local tags."
  :normalizers ((`(,predicate-names . ,tags)
                 `(tags-local ,@tags)))
  :preambles ((`(,predicate-names . ,(and tags (guard tags)))
               ;; When searching for local, non-inherited tags, we can
               ;; search directly to headings containing one of the tags.
               (list :regexp (rx-to-string `(seq bol (1+ "*") (1+ space) (1+ not-newline)
                                                 ":" (or ,@tags) ":")
                                           t)
                     :query t)))
  :body (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
          (-let* (((_ local) (org-ql--tags-at (point))))
            (cl-typecase tags
              (null (tags-p local))
              (otherwise (when (tags-p local)
                           (seq-intersection tags local)))))))

(org-ql-defpred (tags-regexp tags*) (&rest regexps)
  "Return non-nil if current heading has tags matching one or more of REGEXPS.
Tests both inherited and local tags."
  :normalizers ((`(,predicate-names . ,regexps)
                 `(tags-regexp ,@regexps)))
  :body (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
          (-let* (((inherited local) (org-ql--tags-at (point))))
            (cl-typecase regexps
              (null (or (tags-p inherited)
                        (tags-p local)))
              (otherwise (or (when (tags-p inherited)
                               (cl-loop for tag in inherited
                                        thereis (cl-loop for regexp in regexps
                                                         thereis (string-match regexp tag))))
                             (when (tags-p local)
                               (cl-loop for tag in local
                                        thereis (cl-loop for regexp in regexps
                                                         thereis (string-match regexp tag))))))))))

(org-ql-defpred todo (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS."
  ;; TODO: Can we make a preamble for plain (todo) queries?
  :preambles ((`(,predicate-names . ,(and todo-keywords (guard todo-keywords)))
               (list :case-fold nil :regexp (rx-to-string `(seq bol (1+ "*") (1+ space) (or ,@todo-keywords) (or " " eol)) t))))
  :body (when-let ((state (org-get-todo-state)))
          (cl-typecase keywords
            (null (not (member state org-done-keywords)))
            (list (member state keywords))
            (symbol (member state (symbol-value keywords)))
            (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

;;;;;; Ancestor/descendant

;; These predicates search ancestor and descendant headings for sub-queries.

;; Note that the implementations of the upward-searching, ancestor/parent predicates differ
;; from that of the downward-searching, descendants/children predicates in that the former
;; take a predicate function as their argument and test it on each heading (the predicate
;; being created by the `--pre-process-query' function, which see), while the latter take an
;; `org-ql' query form as their argument and execute another `org-ql-select' query inside of
;; the currently running query.  This "split" implementation seems like the most generally
;; efficient one, because searching descendants searches potentially many more headings than
;; searching ancestors, so executing a full query in that case can be faster due to use of
;; the "preambles" provided by running a full query.  However, see note below.

;; NOTE: The ancestors and parent predicates' docstrings are developer-facing
;; rather than user-facing, since their arguments are predicates provided
;; automatically by `--pre-process-query'.

(org-ql-defpred ancestors (predicate)
  "Return non-nil if any of current entry's ancestors satisfy PREDICATE."
  :normalizers ((`(,predicate-names
                   ;; Avoid infinitely compiling already-compiled functions.
                   ,(and query (guard (not (byte-code-function-p query)))))
                 `(ancestors ,(org-ql--query-predicate (org-ql--normalize-query query))))
                (`(,predicate-names) '(ancestors (lambda () t))))
  :body
  (org-with-wide-buffer
   (cl-loop while (org-up-heading-safe)
            thereis (org-ql--value-at (point) predicate))))

(org-ql-defpred parent (predicate)
  "Return non-nil if the current entry's parent satisfies PREDICATE."
  :normalizers ((`(,predicate-names
                   ;; Avoid infinitely compiling already-compiled functions.
                   ,(and query (guard (not (byte-code-function-p query)))))
                 `(parent ,(org-ql--query-predicate (org-ql--normalize-query query))))
                (`(,predicate-names) '(parent (lambda () t))))
  :body
  (org-with-wide-buffer
   (when (org-up-heading-safe)
     (org-ql--value-at (point) predicate))))

;; MAYBE: The `children' and `descendants' predicates could probably be rewritten like
;; the `ancestors' predicate, which avoids calling `org-ql-select' recursively and its
;; associated overhead.  However, that would preclude the use of preambles, so depending
;; on the Org file being searched and the sub-query, performance could be better or
;; worse.  It should be benchmarked extensively before so changing the implementation.

(org-ql-defpred children (query)
  "Return non-nil if current entry has children matching QUERY."
  ;; Quote children queries so the user doesn't have to.
  :normalizers ((`(,predicate-names
                   ;; Avoid infinitely compiling already-compiled functions.
                   ,(and query (guard (not (byte-code-function-p query)))))
                 `(children ,(org-ql--query-predicate (rec query))))
                (`(,predicate-names) '(children (lambda () t))))
  :body
  (org-with-wide-buffer
   ;; Widening is needed if inside an "ancestors" query
   (org-narrow-to-subtree)
   (when (org-goto-first-child)
     ;; Lisp makes this easy and elegant: all we do is modify the query,
     ;; nesting it inside an (and), and it doesn't descend into grandchildren.
     (let* ((level (org-current-level))
            (query (cl-typecase query
                     (byte-code-function `(and (level ,level)
                                               (funcall ,query)))
                     (t `(and (level ,level)
                              ,query)))))
       (catch 'found
         (org-ql-select (current-buffer)
           query
           :narrow t
           :action (lambda ()
                     (throw 'found t))))))))

(org-ql-defpred descendants (query)
  "Return non-nil if current entry has descendants matching QUERY."
  ;; TODO: This could probably be rewritten like the `ancestors' predicate,
  ;; which avoids calling `org-ql-select' recursively and its associated overhead.
  :normalizers ((`(,predicate-names
                   ;; Avoid infinitely requoting query.
                   ,(and query (guard (and (listp query)
                                           (not (eq 'quote (car query)))))))
                 `(descendants ',query))
                (`(,predicate-names) '(descendants (lambda () t))))
  :body
  (org-with-wide-buffer
   (org-narrow-to-subtree)
   (when (org-goto-first-child)
     (narrow-to-region (point) (point-max))
     (catch 'found
       (org-ql-select (current-buffer)
         query
         :narrow t
         :action (lambda ()
                   (throw 'found t)))))))

;;;;;; Timestamps

;; NOTE: The underscores before some arguments in these definitions
;; prevent "unused lexical variable" warnings, because we pre-process
;; them before the functions are called.

;; TODO: Remove the _underscored vars from these arg lists.  I think they're not
;; necessary, or shouldn't be, since --pre-process-query should handle them.

;; NOTE: Arguments to these predicates are pre-processed in
;; `org-ql--normalize-query' and `org-ql--query-predicate'.  Some
;; arguments are not to be given by the user in a query,
;; e.g. `regexp'.  FROM and TO are actually expected to be `ts'
;; structs.  However, the docstrings are written for users, which
;; makes documentation easier to update.

;; TODO: Update the macro to define a user-facing docstring so I don't
;; have to manually update the documentation.

;; This string is common to these predicates and is used in
;; documentation; keeping it here should make it easier to update:
"If FROM, return non-nil if entry's timestamp is on or after FROM.

If TO, return non-nil if entry's timestamp is on or before TO.

If ON, return non-nil if entry's timestamp is on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."

(org-ql-defpred clocked (&key from to _on)
  "Return non-nil if current entry was clocked in given period.
Without arguments, return non-nil if entry was ever clocked.
Note: Clock entries are expected to be clocked out.  Currently
clocked entries (i.e. with unclosed timestamp ranges) are
ignored."
  ;; TODO: Verify that currently clocked entries are still ignored.
  :normalizers ((`(,predicate-names ,(and num-days (pred numberp)))
                 ;; (clocked) and (closed) implicitly look into the past.
                 (let* ((from-day (* -1 num-days))
                        (rest (list :from from-day)))
                   (org-ql--normalize-from-to-on
                     `(clocked :from ,from))))
                (`(,predicate-names . ,rest)
                 (org-ql--normalize-from-to-on
                   `(clocked :from ,from :to ,to))))
  :preambles ((`(,predicate-names ,(pred numberp))
               (list :regexp org-ql-clock-regexp :query t))
              (`(,predicate-names)
               (list :regexp org-ql-clock-regexp :query t)))
  :body
  (org-ql--predicate-ts :from from :to to :regexp org-ql-clock-regexp :match-group 1))

(org-ql-defpred closed (&key from to _on)
  ;; MAYBE: Use the new org-ql-regexps?
  "Return non-nil if current entry was closed in given period.
Without arguments, return non-nil if entry is closed."
  :normalizers ((`(,predicate-names ,(and num-days (pred numberp)))
                 ;; (clocked) and (closed) implicitly look into the past.
                 (let* ((from-day (* -1 num-days))
                        (rest (list :from from-day)))
                   (org-ql--normalize-from-to-on
                     `(closed :from ,from))))
                (`(,predicate-names . ,rest)
                 (org-ql--normalize-from-to-on
                   `(closed :from ,from :to ,to))))
  :preambles ((`(,predicate-names . ,_)
               ;;  Predicate still needs testing.
               (list :regexp org-closed-time-regexp :query query)))
  :body
  (org-ql--predicate-ts :from from :to to :regexp org-closed-time-regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql-defpred deadline (&key from to _on regexp _with-time)
  "Return non-nil if current entry has deadline in given period.
If argument is `auto', return non-nil if entry has deadline
within `org-deadline-warning-days'.  Without arguments, return
non-nil if entry has a deadline."
  :normalizers ((`(,predicate-names auto . ,rest)
                 ;; Use `org-deadline-warning-days' as the :to arg.
                 (let ((ts (->> (ts-now)
                                (ts-adjust 'day org-deadline-warning-days)
                                (ts-apply :hour 23 :minute 59 :second 59))))
                   `(deadline-warning :to ,ts ,@rest)))
                (`(,predicate-names . ,(and rest (guard (numberp (car rest)))))
                 (org-ql--normalize-from-to-on
                   `(deadline :to ,to)))
                (`(,predicate-names . ,rest)
                 (org-ql--normalize-from-to-on
                   `(deadline :from ,from :to ,to))))
  ;; NOTE: Does this normalizer cause the preamble to not be used?
  ;; (Adding one to the deadline-warning definition to be sure.)
  :preambles ((`(,predicate-names . ,rest)
               (list :query query
                     :regexp (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                               ((or 't "t") org-ql-regexp-deadline-with-time)
                               ((or 'nil "nil") org-ql-regexp-deadline-without-time)
                               ('not-found org-ql-regexp-deadline)))))
  :body
  (org-ql--predicate-ts :from from :to to :regexp regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql-defpred deadline-warning (&key from to)
  ;; TODO: Should this also accept a WITH-TIME argument?
  ;; MAYBE: Use the new org-ql-regexps?
  "Internal predicate.
Used to handle `org-deadline-warning-days' and deadlines with warning periods."
  :preambles ((`(,predicate-names . ,_)
               (list :regexp org-deadline-time-regexp :query query)))
  :body
  (save-excursion
    (forward-line 1)
    (when (re-search-forward org-deadline-time-regexp (line-end-position) t)
      (-let* ((context (org-element-context))
              ;; Since we need to handle warning periods, we parse the Org timestamp
              ;; as an org-element rather than as a string.  Unfortunately, sometimes
              ;; `org-element-context' returns a timestamp nested inside a `planning'
              ;; element, other times just the timestamp, so we have to handle both.
              (deadline-ts-element (pcase context
                                     (`(planning ,tss) (plist-get tss :deadline))
                                     (`(timestamp . ,_) context)))
              ((_timestamp (&keys :warning-value :warning-unit)) deadline-ts-element)
              (ts (ts-parse-org-element deadline-ts-element)))
        (pcase warning-unit
          ('nil ;; Deadline has no warning unit: compare with ts passed in.
           (cond ((and from to) (ts-in from to ts))
                 (from (ts<= from ts))
                 (to (ts<= ts to))))
          ;; Deadline has warning unit: compare with current time (`org-ql--today').
          ((and unit (or 'year 'month 'day))
           (ts<= (->> ts (ts-adjust unit (- warning-value))) org-ql--today))
          ('week (ts<= (->> ts (ts-adjust 'day (* -7 warning-value))) org-ql--today)))))))

(org-ql-defpred planning (&key from to _on regexp _with-time)
  "Return non-nil if current entry has planning timestamp in given period.
Without arguments, return non-nil if entry has any planning timestamp."
  :normalizers ((`(,predicate-names . ,(and rest (guard (numberp (car rest)))))
                 (org-ql--normalize-from-to-on
                   `(planning :to ,to)))
                (`(,predicate-names . ,rest)
                 (org-ql--normalize-from-to-on
                   `(planning :from ,from :to ,to))))
  :preambles ((`(,predicate-names . ,rest)
               (list :query query
                     :regexp (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                               ((or 't "t") org-ql-regexp-planning-with-time)
                               ((or 'nil "nil") org-ql-regexp-planning-without-time)
                               ('not-found org-ql-regexp-planning)))))
  ;; MAYBE: Should the regexp be done in the normalizer instead?  (If
  ;; so, also in other ts-related predicates.)
  :body
  (org-ql--predicate-ts :from from :to to :regexp regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql-defpred scheduled (&key from to _on regexp _with-time)
  "Return non-nil if current entry is scheduled in given period.
Without arguments, return non-nil if entry is scheduled."
  :normalizers ((`(,predicate-names . ,rest)
                 (org-ql--normalize-from-to-on
                   `(scheduled :from ,from :to ,to))))
  :preambles ((`(,predicate-names . ,rest)
               (list :query query
                     :regexp (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                               ((or 't "t") org-ql-regexp-scheduled-with-time)
                               ((or 'nil "nil") org-ql-regexp-scheduled-without-time)
                               ('not-found org-ql-regexp-scheduled)))))
  :body
  (org-ql--predicate-ts :from from :to to :regexp regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql-defpred (ts ts-active ts-a ts-inactive ts-i)
  (&key from to _on regexp _with-time
        (match-group 0) (limit (org-entry-end-position)))
  "Return non-nil if current entry has a timestamp in given period.
Without arguments, return non-nil if entry has a timestamp.

TYPE may be `active' to match active timestamps, `inactive' to
match inactive ones, or `both' / nil to match both types.

LIMIT bounds the search for the timestamp REGEXP.  It defaults to
the end of the entry, i.e. the position returned by
`org-entry-end-position', but for certain searches it should be
bound to a different positiion (e.g. for planning lines, the end
of the line after the heading).  MATCH-GROUP should be the number
of REGEXP's group that matches the Org timestamp (i.e. excluding
any planning prefix); it defaults to 0 (i.e. the whole regexp)."
  ;; MAYBE: Define active/inactive ones separately?
  :normalizers
  ((`(,(or 'ts-active 'ts-a) . ,rest) `(ts :type active ,@rest))
   (`(,(or 'ts-inactive 'ts-i) . ,rest) `(ts :type inactive ,@rest))
   (`(,predicate-names . ,(and rest (guard (numberp (car rest)))))
    (org-ql--normalize-from-to-on
      `(ts :type ,type :to ,to)))
   (`(,predicate-names . ,rest)
    (org-ql--normalize-from-to-on
      `(ts :type ,type :from ,from :to ,to))))

  :preambles
  ((`(,predicate-names . ,rest)
    (list :regexp (pcase (plist-get rest :type)
                    ((or 'nil 'both) (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                                       ((or 't "t") org-ql-regexp-ts-both-with-time)
                                       ((or 'nil "nil") org-ql-regexp-ts-both-without-time)
                                       ('not-found org-ql-regexp-ts-both)))
                    ('active (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                               ((or 't "t") org-ql-regexp-ts-active-with-time)
                               ((or 'nil "nil") org-ql-regexp-ts-active-without-time)
                               ('not-found org-ql-regexp-ts-active)))
                    ('inactive (pcase-exhaustive (org-ql--plist-get* rest :with-time)
                                 ((or 't "t") org-ql-regexp-ts-inactive-with-time)
                                 ((or 'nil "nil") org-ql-regexp-ts-inactive-without-time)
                                 ('not-found org-ql-regexp-ts-inactive))))
          ;; Predicate needs testing only when args are present.
          :query (-let (((&keys :from :to :on) rest))
                   ;; TODO: This used to be (when (or from to on) query), but
                   ;; that doesn't seem right, so I changed it to this if, and the
                   ;; tests pass either way.  Might deserve a little scrutiny.
                   (if (or from to on)
                       query
                     t)))))

  :body
  (cl-macrolet ((next-timestamp ()
                  `(when (re-search-forward regexp limit t)
                     (ts-parse-org (match-string match-group))))
                (test-timestamps (pred-form)
                  `(cl-loop for next-ts = (next-timestamp)
                            while next-ts
                            thereis ,pred-form)))
    (save-excursion
      (cond ((not (or from to)) (re-search-forward regexp limit t))
            ((and from to) (test-timestamps (ts-in from to next-ts)))
            (from (test-timestamps (ts<= from next-ts)))
            (to (test-timestamps (ts<= next-ts to)))))))

;; NOTE: Predicates defined: stop deferring and define normalizer and
;; preamble functions now.  Reversing preserves the order in which
;; they were defined.  Generally it shouldn't matter, but it might...
(setf org-ql-defpred-defer nil)
(org-ql--define-normalize-query-fn (reverse org-ql-predicates))
(org-ql--define-query-preamble-fn (reverse org-ql-predicates))
(org-ql--def-query-string-to-sexp-fn (reverse org-ql-predicates))

;;;;; Sorting

;; TODO: These appear to work properly, but it would be good to have tests for them.
;; MAYBE: Add timestamp sorter.  Could be slow in some cases, without clever caching of timestamps per-entry.

(defun org-ql--sort-by (items predicates)
  "Return ITEMS sorted by PREDICATES.
PREDICATES is a list of one or more sorting methods, including:
`deadline', `scheduled', `closed' and `priority'."
  ;; MAYBE: Use macrolet instead of flet.
  (cl-flet* ((sorter (symbol)
               (pcase symbol
                 ((or 'deadline 'scheduled 'closed)
                  (apply-partially #'org-ql--date-type< (intern (concat ":" (symbol-name symbol)))))
                 ;; TODO: Rename `date' to `planning'.  `date' should be something else.
                 ('date #'org-ql--date<)
                 ('priority #'org-ql--priority<)
                 ('random (lambda (&rest _ignore)
                            (= 0 (random 2))))
                 ;; NOTE: reverse and todo are handled below.
                 ;; TODO: Add more.
                 (_ (user-error "Invalid sorting predicate: %s" symbol))))
             (sort-by-todo-keyword (items)
               (let* ((grouped-items (--group-by (when-let (keyword (org-element-property :todo-keyword it))
                                                   (substring-no-properties keyword))
                                                 items))
                      (sorted-groups (cl-sort grouped-items #'<
                                              :key (lambda (keyword)
                                                     (or (cl-position (car keyword) org-todo-keywords-1 :test #'string=)
                                                         ;; Put at end of list if not found
                                                         (1+ (length org-todo-keywords-1)))))))
                 (-flatten-n 1 (-map #'cdr sorted-groups)))))
    (dolist (pred predicates)
      (setq items (pcase pred
                    ;; NOTE: Using `reverse' instead of `nreverse' because my gut
                    ;; tells me that, while `nreverse' would be preferable and faster,
                    ;; it would probably cause weird bugs, like items' order being
                    ;; reversed every time a cached query is refreshed in a view.
                    ('reverse (reverse items))
                    ('todo (sort-by-todo-keyword items))
                    (_ (-sort (sorter pred) items)))))
    items))

;; TODO: Rewrite date sorters using `ts'.

(defun org-ql--date-type< (type a b)
  "Return non-nil if A's date of TYPE is earlier than B's.
A and B are Org headline elements.  TYPE should be a symbol like
`:deadline' or `:scheduled'"
  (org-ql--org-timestamp-element< (org-element-property type a)
                                  (org-element-property type b)))

(defun org-ql--date< (a b)
  "Return non-nil if A's deadline or scheduled property is earlier than B's.
Deadline is considered before scheduled."
  (cl-macrolet ((ts (item)
                  `(or (org-element-property :deadline ,item)
                       (org-element-property :scheduled ,item))))
    (org-ql--org-timestamp-element< (ts a) (ts b))))

(defun org-ql--org-timestamp-element< (a b)
  "Return non-nil if A's date element is earlier than B's.
A and B are Org timestamp elements."
  (cl-macrolet ((ts (ts)
                  `(when ,ts
                     (org-ql--org-timestamp-format ,ts "%s"))))
    (let* ((a-ts (ts a))
           (b-ts (ts b)))
      (cond ((and a-ts b-ts)
             (string< a-ts b-ts))
            (a-ts t)
            (b-ts nil)))))

(defun org-ql--priority< (a b)
  "Return non-nil if A's priority is higher than B's.
A and B are Org headline elements."
  (cl-macrolet ((priority (item)
                  `(org-element-property :priority ,item)))
    ;; NOTE: Priorities are numbers in Org elements.  This might differ from the priority selector logic.
    (let ((a-priority (priority a))
          (b-priority (priority b)))
      (cond ((and a-priority b-priority)
             (< a-priority b-priority))
            (a-priority t)
            (b-priority nil)))))

(defun org-ql--infix-p (infix list)
  "Return non-nil if INFIX is an infix of LIST.
Each element of INFIX is compared using `string-match', so each
element should be a regexp string."
  (cl-loop with infix-length = (length infix)
           while (and list
                      (>= (length list) infix-length))
           thereis (cl-loop for i in infix
                            for l in list
                            always (string-match i l))
           do (pop list)))

;; And now we go the other direction...

(defun org-ql--query-sexp-to-string (query)
  "Return a string query for sexp QUERY.
If QUERY can't be converted to a string, return nil."
  ;; This started out pretty simple...but at least it's not just one long function, right?
  (cl-labels ((complex-p (query)
                (or (contains-p 'or query)
		    (contains-p 'ancestors query)
		    (contains-p 'children query)
		    (contains-p 'descendants query)
		    (contains-p 'parent query)))
              (contains-p (symbol list)
                (cl-loop for element in list
                         thereis (or (eq symbol element)
                                     (and (listp element)
                                          (contains-p symbol element)))))
              (format-args (args)
                (let (non-paired paired next-keyword)
                  (cl-loop for arg in args
                           do (cond (next-keyword (push (cons next-keyword arg) paired)
                                                  (setf next-keyword nil))
                                    ((keywordp arg) (setf next-keyword (substring (symbol-name arg) 1)))
                                    (t (push arg non-paired))))
                  (string-join (append (mapcar #'format-atom non-paired)
                                       (nreverse (--map (format "%s=%s" (car it) (cdr it))
                                                        paired)))
                               ",")))
              (format-atom (atom)
                (cl-typecase atom
                  (string (if (string-match (rx space) atom)
                              (format "%S" atom)
                            (format "%s" atom)))
                  (t (format "%s" atom))))
              (format-form (form)
                (pcase form
                  (`(not . (,rest)) (concat "!" (format-form rest)))
                  (`(priority . ,_) (format-priority form))
                  ;; FIXME: Convert (src) queries to non-sexp form...someday...
                  (`(src . ,_) (user-error "Converting (src ...) queries to non-sexp form is not implemented"))
                  (_ (pcase-let* ((`(,pred . ,args) form)
                                  (args-string (pcase args
                                                 ('() "")
                                                 ((guard (= 1 (length args))) (format "%s" (car args)))
                                                 (_ (format-args args)))))
                       (format "%s:%s" pred args-string)))))
              (format-and (form)
                (pcase-let* ((`(and . ,rest) form))
                  (string-join (mapcar #'format-form rest) " ")))
              (format-priority (form)
                (pcase-let* ((`(priority . ,rest) form)
                             (args (pcase rest
                                     (`(,(and comparator (or '< '<= '> '>= '=)) ,letter)
                                      (priority-letters comparator letter))
                                     (_ rest))))
                  (concat "priority:" (string-join args ","))))
              (priority-letters (comparator letter)
                (let* ((char (string-to-char (upcase (symbol-name letter))))
                       (numeric-priorities '(?A ?B ?C))
                       ;; NOTE: The comparator inversion is intentional.
                       (others (pcase comparator
                                 ('< (--select (> it char) numeric-priorities))
                                 ('<= (--select (>= it char) numeric-priorities))
                                 ('> (--select (< it char) numeric-priorities))
                                 ('>= (--select (<= it char) numeric-priorities))
                                 ('= (--select (= it char) numeric-priorities)))))
                  (mapcar #'char-to-string others))))
    ;; FIXME: Error out for ts structs passed to `ts' predicate (very unlikely to be linked to).
    (unless (complex-p query)
      (pcase query
        (`(and . ,_) (format-and query))
        (_ (format-form query))))))

;;;; Footer

(provide 'org-ql)

;;; org-ql.el ends here
