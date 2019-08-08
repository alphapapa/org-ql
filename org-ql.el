;;; org-ql.el --- Query language for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql
;; Version: 0.2-pre
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.0") (s "1.12.0"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; `org-ql' is a lispy query language for Org files.  It allows you to
;; find Org entries matching certain criteria and return a list of
;; them or perform actions on them.  Commands are also provided which
;; display a buffer with matching results, similar to an Org Agenda
;; buffer.

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

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'org-habit)
(require 'seq)
(require 'subr-x)

(require 'dash)

;;;; Compatibility

(if (version< org-version "9.2")
    (progn
      (defalias 'org-timestamp-to-time #'org-timestamp--to-internal-time)
      (defun org-ql--get-tags (&optional pos local)
        (org-get-tags-at pos local)))
  (defun org-ql--get-tags (&optional pos local)
    (org-get-tags pos local)))

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

(defvar org-ql-predicates
  (list (list :name 'org-back-to-heading :fn (symbol-function 'org-back-to-heading)))
  "Plist of predicates, their corresponding functions, and their docstrings.
This list should not contain any duplicates.")

;;;; Macros

(cl-defmacro org-ql--defpred (name args docstring &rest body)
  "Define an `org-ql' selector predicate named `org-ql--predicate-NAME'.
ARGS is a `cl-defun'-style argument list.  DOCSTRING is the
function's docstring.  BODY is the body of the predicate.

Predicates will be called with point on the beginning of an Org
heading and should return non-nil if the heading's entry is a
match."
  (declare (debug (symbolp listp stringp def-body))
           (indent defun))
  (let ((fn-name (intern (concat "org-ql--predicate-" (symbol-name name))))
        (pred-name (intern (symbol-name name))))
    `(progn
       (push (list :name ',pred-name :fn ',fn-name :docstring ,docstring :args ',args) org-ql-predicates)
       (cl-defun ,fn-name ,args ,docstring ,@body))))

(cl-defmacro org-ql (buffers-or-files query &key sort narrow action)
  "Expands into a call to `org-ql-select' with the same arguments.
For convenience, arguments should be unquoted."
  (declare (indent defun))
  `(org-ql-select ,buffers-or-files
     ',query
     :action ',action
     :narrow ,narrow
     :sort ',sort))

;;;; Functions

(define-hash-table-test 'org-ql-hash-test #'equal (lambda (args)
                                                    (sxhash-equal (prin1-to-string args))))

(cl-defun org-ql-select (buffers-or-files query &key action narrow sort)
  "Return items matching QUERY in BUFFERS-OR-FILES.

BUFFERS-OR-FILES is a one or a list of files and/or buffers.

QUERY is an `org-ql' query sexp (quoted, since this is a
function).

ACTION is a function which is called on each matching entry with
point at the beginning of its heading.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to calling
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-agenda--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

If NARROW is non-nil, buffers are not widened (the default is to
widen and search the entire buffer).

SORT is either nil, in which case items are not sorted; or one or
a list of defined `org-ql' sorting methods (`date', `deadline',
`scheduled', `todo', or `priority'); or a user-defined comparator
function that accepts two items as arguments and returns nil or
non-nil."
  (declare (indent defun))
  (-let* ((buffers (->> (cl-typecase buffers-or-files
                          (null (list (current-buffer)))
                          (list buffers-or-files)
                          (otherwise (list buffers-or-files)))
                        (--map (cl-etypecase it
                                 (buffer it)
                                 (string (or (find-buffer-visiting it)
                                             (when (file-readable-p it)
                                               ;; It feels unintuitive that `find-file-noselect' returns
                                               ;; a buffer if the filename doesn't exist.
                                               (find-file-noselect it))
                                             (user-error "Can't open file: %s" it)))))))
          (query (org-ql--pre-process-query query))
          ((query preamble-re) (org-ql--query-preamble query))
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
          ;; TODO: Figure out how to use or reimplement the org-scanner-tags feature.
          ;; (org-use-tag-inheritance t)
          ;; (org-trust-scanner-tags t)
          (org-ql--today (org-today))
          (items (->> buffers
                      (--map (with-current-buffer it
                               (unless (derived-mode-p 'org-mode)
                                 (user-error "Not an Org buffer: %s" (buffer-name)))
                               (org-ql--select-cached :query query :preamble-re preamble-re
                                                      :predicate predicate :action action :narrow narrow)))
                      (-flatten-n 1))))
    ;; Sort items
    (pcase sort
      (`nil items)
      ((guard (and sort
                   (setq sort (-list sort))
                   (cl-loop for elem in sort
                            always (memq elem '(date deadline scheduled todo priority)))))
       ;; Default sorting functions
       (org-ql--sort-by items sort))
      ;; Sort by user-given comparator.
      ((pred functionp) (sort items sort))
      (_ (user-error "SORT must be either nil, or one or a list of the defined sorting methods (see documentation)")))))

(cl-defun org-ql-query (&key (select 'element-with-markers) from where)
  "Like `org-ql-select', but arguments are named more like a SQL query.

SELECT corresponds to the `org-ql-select' argument ACTION.  It is
the function called on matching headings, the results of which
are returned by this function.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-agenda--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

FROM corresponds to the `org-ql-select' argument BUFFERS-OR-FILES.
It may be one or a list of file paths and/or buffers.

WHERE corresponds to the `org-ql-select' argument QUERY.  It
should be an `org-ql' query sexp."
  (declare (indent defun))
  (org-ql-select from where
    :action select))

(defun org-ql--pre-process-query (query)
  "Return QUERY having been pre-processed.
Replaces bare strings with (regexp) selectors."
  ;; This is unsophisticated, but it works.
  (cl-labels ((rec (element)
                   (pcase element
                     (`(or . ,clauses) `(or ,@(mapcar #'rec clauses)))
                     (`(and . ,clauses) `(and ,@(mapcar #'rec clauses)))
                     (`(when ,condition . ,clauses) `(when ,(rec condition)
                                                       ,@(mapcar #'rec clauses)))
                     (`(unless ,condition . ,clauses) `(unless ,(rec condition)
                                                         ,@(mapcar #'rec clauses)))
                     ;; TODO: Combine (regexp) when appropriate (i.e. inside an OR, not an AND).
                     ((pred stringp) `(regexp ,element))
                     (_ element))))
    (rec query)))

(defun org-ql--query-predicate (query)
  "Return predicate function for QUERY."
  (byte-compile `(lambda ()
                   ;; This is either really elegant or really ugly.  Well, also
                   ;; possibly somewhere in-between.  At the least, we should do
                   ;; this in a more flexible, abstracted way, but this will do
                   ;; for now.  Most importantly, it works!
                   (let (from to on)
                     ;; TODO: DRY these macrolets.
                     (cl-macrolet ((clocked (&key from to on)
                                            (when on
                                              (setq from on
                                                    to on))
                                            (when from
                                              (setq from (org-ql--parse-time-string from)))
                                            (when to
                                              (setq to (org-ql--parse-time-string to 'end)))
                                            ;; NOTE: The macro must expand to the actual `org-ql--predicate-clocked'
                                            ;; function, not another `clocked'.
                                            `(org-ql--predicate-clocked :from ,from :to ,to))
                                   (ts (&key from to on)
                                       (when on
                                         (setq from on
                                               to on))
                                       (when from
                                         (setq from (org-ql--parse-time-string from)))
                                       (when to
                                         (setq to (org-ql--parse-time-string to 'end)))
                                       ;; NOTE: The macro must expand to the actual `org-ql--predicate-ts'
                                       ;; function, not another `ts'.
                                       `(org-ql--predicate-ts :from ,from :to ,to))
                                   (ts-active (&key from to on)
                                              (when on
                                                (setq from on
                                                      to on))
                                              (when from
                                                (setq from (org-ql--parse-time-string from)))
                                              (when to
                                                (setq to (org-ql--parse-time-string to 'end)))
                                              ;; NOTE: The macro must expand to the actual `org-ql--predicate-ts'
                                              ;; function, not another `ts'.
                                              `(org-ql--predicate-ts-active :from ,from :to ,to))
                                   (ts-inactive (&key from to on)
                                                (when on
                                                  (setq from on
                                                        to on))
                                                (when from
                                                  (setq from (org-ql--parse-time-string from)))
                                                (when to
                                                  (setq to (org-ql--parse-time-string to 'end)))
                                                ;; NOTE: The macro must expand to the actual `org-ql--predicate-ts'
                                                ;; function, not another `ts'.
                                                `(org-ql--predicate-ts-inactive :from ,from :to ,to)))
                       (cl-symbol-macrolet ((today org-ql--today) ; Necessary because of byte-compiling the lambda
                                            (= #'=)
                                            (< #'<)
                                            (> #'>)
                                            (<= #'<=)
                                            (>= #'>=))
                         ,query))))))

(defun org-ql--query-preamble (query)
  "Return (QUERY PREAMBLE) for QUERY.
When QUERY has a clause with a corresponding preamble, and it's
appropriate to use one (i.e. the clause is not in an `or'),
replace the clause with a preamble."
  (pcase org-ql-use-preamble
    ('nil (list query nil))
    (_ (let (org-ql-preamble)
         (cl-labels ((rec (element)
                          (or (when org-ql-preamble
                                ;; Only one preamble is allowed
                                element)
                              (pcase element
                                (`(or _) element)
                                (`(regexp . ,regexps)
                                 (setq org-ql-preamble (rx-to-string `(or ,@regexps) t))
                                 ;; Return nil, because we don't need to test the predicate.
                                 nil)
                                (`(todo . ,(and todo-keywords (guard todo-keywords)))
                                 ;; FIXME: With case-folding, a query like (todo "WAITING") can find a non-todo
                                 ;; heading named "Waiting".  For correctness, we could test the predicate
                                 ;; anyway, but that would negate some of the speed, and in most cases it
                                 ;; probably won't matter, so I'm leaving it this way for now.
                                 (setq org-ql-preamble
                                       (rx-to-string `(seq bol (1+ "*") (1+ space) (or ,@todo-keywords) (or " " eol))
                                                     t))
                                 ;; Return nil, don't test the predicate.
                                 nil)
                                (`(level ,comparator-or-num ,num)
                                 (let ((repeat (pcase comparator-or-num
                                                 ('< `(repeat 1 ,(1- num) "*"))
                                                 ('<= `(repeat 1 ,num "*"))
                                                 ('> `(>= ,(1+ num) "*"))
                                                 ('>= `(>= ,num "*"))
                                                 ((pred integerp) `(repeat ,comparator-or-num ,num "*")))))
                                   (setq org-ql-preamble (rx-to-string `(seq bol ,repeat " ") t))
                                   ;; Return nil, because we don't need to test the predicate.
                                   nil))
                                (`(level ,num)
                                 (setq org-ql-preamble (rx-to-string `(seq bol (repeat ,num "*") " ") t))
                                 nil)
                                ((and `(tags . ,tags) (guard (not org-use-tag-inheritance)))
                                 ;; When tag inheritance is disabled, we only consider direct tags,
                                 ;; so we can search directly to headings containing one of the tags.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ space) (1+ not-newline)
                                                                           ":" (or ,@tags) ":")
                                                                     t))
                                 ;; Return nil, because we don't need to test the predicate.
                                 nil)
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
                         (query (-flatten-n 1 query))))
           (list query org-ql-preamble))))))

(defun org-ql--select-cached (&rest args)
  "Return results for ARGS and current buffer using cache."
  ;; MAYBE: Timeout cached queries.  Probably not necessarily since they will be removed when a
  ;; buffer is closed, or when a query is run after modifying a buffer.
  (-let (((&plist :query query :action action :narrow narrow) args))
    (if-let* ((buffer-cache (gethash (current-buffer) org-ql-cache))
              (query-cache (cadr buffer-cache))
              (modified-tick (car buffer-cache))
              (buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
              (cache-key (list query action narrow))
              (cached-result (gethash cache-key query-cache)))
        (pcase cached-result
          ('org-ql-nil nil)
          (_ cached-result))
      (let ((new-result (apply #'org-ql--select args)))
        (cond ((or (not query-cache)
                   (not buffer-unmodified-p))
               (puthash (current-buffer)
                        (list (buffer-modified-tick)
                              (let ((table (make-hash-table :test 'org-ql-hash-test)))
                                (puthash args (or new-result 'org-ql-nil) table)
                                table))
                        org-ql-cache))
              (t (puthash args (or new-result 'org-ql-nil) query-cache)))
        new-result))))

(cl-defun org-ql--select (&key preamble-re predicate action narrow &allow-other-keys)
  "Return results of mapping function ACTION across entries in current buffer matching function PREDICATE.
If NARROW is non-nil, buffer will not be widened."
  ;; Since the mappings are stored in the variable `org-ql-predicates', macros like `flet'
  ;; can't be used, so we do it manually (this is same as the equivalent `flet' expansion).
  ;; Mappings are stored in the variable because it allows predicates to be defined with a
  ;; macro, which allows documentation to be easily generated for them.

  ;; MAYBE: Lift the `flet'-equivalent out of this function so it isn't done for each buffer.
  (let (orig-fns)
    (--each org-ql-predicates
      ;; Save original function mappings.
      (let ((name (plist-get it :name)))
        (push (list :name name :fn (symbol-function name)) orig-fns)))
    (unwind-protect
        (progn
          (--each org-ql-predicates
            ;; Set predicate functions.
            (fset (plist-get it :name) (plist-get it :fn)))
          ;; Run query.
          (save-excursion
            (save-restriction
              (unless narrow
                (widen))
              (goto-char (point-min))
              (when (org-before-first-heading-p)
                (outline-next-heading))
              ;; `cl-loop' makes this double-while much clearer than the expanded form.
              (cond (preamble-re (cl-loop while (re-search-forward preamble-re nil t)
                                          do (outline-back-to-heading 'invisible-ok)
                                          when (funcall predicate)
                                          collect (funcall action)
                                          do (outline-next-heading)))
                    (t (cl-loop when (funcall predicate)
                                collect (funcall action)
                                while (outline-next-heading)))))))
      (--each orig-fns
        ;; Restore original function mappings.
        (fset (plist-get it :name) (plist-get it :fn))))))

;;;;; Helpers

(defun org-ql--add-markers (element)
  "Return ELEMENT with Org marker text properties added.
ELEMENT should be an Org element like that returned by
`org-element-headline-parser'.  This function should be called
from within ELEMENT's buffer."
  ;; FIXME: `org-agenda-new-marker' works, until it doesn't, because...I don't know.  It sometimes
  ;; raises errors or returns markers that don't point into a buffer.  `copy-marker' always works,
  ;; of course, but maybe it will leave "dangling" markers, which could affect performance over
  ;; time?  I don't know, but for now, it seems that we have to use `copy-marker'.
  (let* ((marker (copy-marker (org-element-property :begin element)))
         (properties (--> (cadr element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (cadr element) properties)
    element))

(defun org-ql--sanity-check-form (form)
  "Signal error if any forms in FORM do not have preconditions met.
Or, when possible, fix the problem."
  (cl-flet ((check (symbol)
                   (cl-case symbol
                     ('done (unless org-done-keywords
                              ;; NOTE: This check needs to be done from within the Org buffer being checked.
                              (error "Variable `org-done-keywords' is nil.  Are you running this from an Org buffer?"))))))
    (cl-loop for elem in form
	     if (consp elem)
	     do (progn
		  (check (car elem))
		  (org-ql--sanity-check-form (cdr elem)))
	     else do (check elem))))

(defun org-ql--parse-time-string (s &optional end)
  "Return Unix timestamp by parsing timestamp string S.
Calls `parse-time-string' and fills in nil second, minute, and
hour values, then calls `float-time'.  When END is non-nil, sets
empty time values to 23:59:59; otherwise, to 00:00:00."
  ;; TODO: Also accept Unix timestamps.
  (cl-macrolet ((fill-with (place value)
                           `(unless (nth ,place parsed-time)
                              (setf (nth ,place parsed-time) ,value))))
    (let ((parsed-time (parse-time-string s)))
      (pcase end
        ('nil (fill-with 0 0)
              (fill-with 1 0)
              (fill-with 2 0))
        (_ (fill-with 0 59)
           (fill-with 1 59)
           (fill-with 2 23)))
      (float-time (apply #'encode-time parsed-time)))))

;;;;; Predicates

(org-ql--defpred clocked (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable" warnings, because we
  ;; pre-process that argument in a macro before this function is called.
  "Return non-nil if current entry was clocked in given period.
If no arguments are specified, return non-nil if entry was
clocked at any time.

If FROM, return non-nil if entry was clocked on or after FROM.
If TO, return non-nil if entry was clocked on or before TO.
If ON, return non-nil if entry was clocked on date ON.

FROM, TO, and ON should be strings parseable by
`parse-time-string' but may omit the time value.

Note: Clock entries are expected to be clocked out.  Currently
clocked entries (i.e. with unclosed timestamp ranges) are
ignored."
  ;; NOTE: FROM and TO are actually expected to be Unix timestamps.  The docstring is written
  ;; for end users, for which the arguments are pre-processed by `org-ql-select'.
  ;; FIXME: This assumes every "clocked" entry is a range.  Unclosed clock entries are not handled.
  (cl-macrolet ((next-timestamp ()
                                `(when (re-search-forward org-clock-line-re end-pos t)
                                   (org-element-property :value (org-element-context))))
                (test-timestamps (pred-form)
                                 `(cl-loop for next-ts = (next-timestamp)
                                           while next-ts
                                           ;; Using `setf' instead of `for beg =` here prevents "unused lexical variable" warnings.
                                           do (setf beg (float-time (org-timestamp-to-time next-ts))
                                                    end (float-time (org-timestamp-to-time next-ts 'end)))
                                           thereis ,pred-form)))
    (save-excursion
      (let ((end-pos (org-entry-end-position))
            beg end)
        (cond ((not (or from to)) (next-timestamp))
              ((and from to) (test-timestamps (and (<= beg to)
                                                   (>= end from))))
              (from (test-timestamps (<= from end)))
              (to (test-timestamps (<= beg to))))))))

(org-ql--defpred category (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES (a list of strings)."
  (when-let ((category (org-get-category (point))))
    (cl-typecase categories
      (null t)
      (otherwise (member category categories)))))

(org-ql--defpred todo (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS (a list of strings)."
  (when-let ((state (org-get-todo-state)))
    (cl-typecase keywords
      (null t)
      (list (member state keywords))
      (symbol (member state (symbol-value keywords)))
      (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

(org-ql--defpred done ()
  "Return non-nil if entry's TODO keyword is in `org-done-keywords'."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (or (apply #'org-ql--predicate-todo org-done-keywords)))

(org-ql--defpred tags (&rest tags)
  "Return non-nil if current heading has one or more of TAGS (a list of strings)."
  ;; TODO: Try to use `org-make-tags-matcher' to improve performance.  It would be nice to not have
  ;; to run `org-get-tags' for every heading, especially with inheritance.
  (when-let ((tags-at (org-ql--get-tags (point) (not org-use-tag-inheritance))))
    (cl-typecase tags
      (null t)
      (otherwise (seq-intersection tags tags-at)))))

(org-ql--defpred level (level-or-comparator &optional level)
  "Return non-nil if current heading's outline level matches arguments.
The following forms are accepted:

  (level NUMBER): Matches if heading level is NUMBER.
  (level NUMBER NUMBER): Matches if heading level is equal to or between NUMBERs.
  (level COMPARATOR NUMBER): Matches if heading level compares to NUMBER with COMPARATOR.

COMPARATOR may be `<', `<=', `>', or `>='."
  ;; NOTE: It might be necessary to take into account `org-odd-levels'; see docstring for
  ;; `org-outline-level'.
  (when-let ((outline-level (org-outline-level)))
    (pcase level-or-comparator
      ((pred numberp) (pcase level
                        ('nil ;; Equality
                         (= outline-level level-or-comparator))
                        ((pred numberp) ;; Between two levels
                         (>= level-or-comparator outline-level level))))
      ((pred symbolp) ;; Compare with function
       (funcall level-or-comparator outline-level level)))))

(org-ql--defpred priority (&optional comparator-or-priority priority)
  "Return non-nil if current heading has a certain priority.
COMPARATOR-OR-PRIORITY should be either a comparator function,
like `<=', or a priority string, like \"A\" (in which case (`='
will be the comparator).  If COMPARATOR-OR-PRIORITY is a
comparator, PRIORITY should be a priority string."
  (let* (comparator)
    (cond ((null priority)
           ;; No comparator given: compare only given priority with =
           (setq priority comparator-or-priority
                 comparator '=))
          (t
           ;; Both comparator and priority given
           (setq comparator comparator-or-priority)))
    (setq comparator (cl-case comparator
                       ;; Invert comparator because higher priority means lower number
                       (< '>)
                       (> '<)
                       (<= '>=)
                       (>= '<=)
                       (= '=)
                       (otherwise (user-error "Invalid comparator: %s" comparator))))
    (setq priority (* 1000 (- org-lowest-priority (string-to-char priority))))
    (when-let ((item-priority (save-excursion
                                (save-match-data
                                  ;; FIXME: Is the save-match-data above necessary?
                                  (when (and (looking-at org-heading-regexp)
                                             (save-match-data
                                               (string-match org-priority-regexp (match-string 0))))
                                    ;; TODO: Items with no priority
                                    ;; should not be the same as B
                                    ;; priority.  That's not very
                                    ;; useful IMO.  Better to do it
                                    ;; like in org-super-agenda.
                                    (org-get-priority (match-string 0)))))))
      (funcall comparator priority item-priority))))

(org-ql--defpred habit ()
  "Return non-nil if entry is a habit."
  (org-is-habit-p))

(org-ql--defpred regexp (&rest regexps)
  "Return non-nil if current entry matches one of REGEXPS (regexp strings)."
  (let ((end (or (save-excursion
                   (outline-next-heading))
                 (point-max))))
    (save-excursion
      (goto-char (line-beginning-position))
      (cl-loop for regexp in regexps
               thereis (save-excursion
                         (re-search-forward regexp end t))))))

(org-ql--defpred heading (regexp)
  "Return non-nil if current entry's heading matches REGEXP (a regexp string)."
  (string-match regexp (org-get-heading 'no-tags 'no-todo)))

(org-ql--defpred property (property &optional value)
  "Return non-nil if current entry has PROPERTY (a string), and optionally VALUE (a string)."
  (pcase property
    ('nil (user-error "Property matcher requires a PROPERTY argument"))
    (_ (pcase value
         ('nil
          ;; Check that PROPERTY exists
          (org-entry-get (point) property))
         (_
          ;; Check that PROPERTY has VALUE
          (string-equal value (org-entry-get (point) property 'selective)))))))

;;;;;; Timestamps

;; TODO: Move active/inactive into (ts) predicate, allowing the first arg to be either
;; inactive/active or the comparator.  Using numeric comparators is more powerful, concise,
;; and language-independent than using from/to.  Alternatively, add :before/:after, but I
;; think the comparators are better.  Also consider using a macro to DRY these out.

(org-ql--defpred ts (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable" warnings, because we
  ;; pre-process that argument in a macro before this function is called.
  "Return non-nil if current entry has a timestamp in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be strings parseable by
`parse-time-string' but may omit the time value."
  ;; TODO: DRY this with the clocked predicate.
  ;; NOTE: FROM and TO are actually expected to be Unix timestamps.  The docstring is written
  ;; for end users, for which the arguments are pre-processed by `org-ql-select'.
  ;; FIXME: This assumes every "clocked" entry is a range.  Unclosed clock entries are not handled.
  (cl-macrolet ((next-timestamp ()
                                `(when (re-search-forward org-element--timestamp-regexp end-pos t)
                                   (save-excursion
                                     (goto-char (match-beginning 0))
                                     (org-element-timestamp-parser))))
                (test-timestamps (pred-form)
                                 `(cl-loop for next-ts = (next-timestamp)
                                           while next-ts
                                           do (setf beg (float-time (org-timestamp-to-time next-ts))
                                                    end (float-time (org-timestamp-to-time next-ts 'end)))
                                           thereis ,pred-form)))
    (save-excursion
      (let ((end-pos (org-entry-end-position))
            beg end)
        (cond ((not (or from to)) (next-timestamp))
              ((and from to) (test-timestamps (and (<= beg to)
                                                   (>= end from))))
              (from (test-timestamps (<= from end)))
              (to (test-timestamps (<= beg to))))))))

(org-ql--defpred ts-active (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable" warnings, because we
  ;; pre-process that argument in a macro before this function is called.
  "Return non-nil if current entry has an active timestamp in given period.
If no arguments are specified, return non-nil if entry has any
active timestamp.

If FROM, return non-nil if entry has an active timestamp on or
after FROM.

If TO, return non-nil if entry has an active timestamp on or
before TO.

If ON, return non-nil if entry has an active timestamp on date
ON.

FROM, TO, and ON should be strings parseable by
`parse-time-string' but may omit the time value."
  ;; TODO: DRY this with the clocked predicate.
  ;; NOTE: FROM and TO are actually expected to be Unix timestamps.  The docstring is written
  ;; for end users, for which the arguments are pre-processed by `org-ql-select'.
  ;; FIXME: This assumes every "clocked" entry is a range.  Unclosed clock entries are not handled.
  (cl-macrolet ((next-timestamp ()
                                `(when (re-search-forward org-element--timestamp-regexp end-pos t)
                                   (save-excursion
                                     (goto-char (match-beginning 0))
                                     (org-element-timestamp-parser))))
                (test-timestamps (pred-form)
                                 `(cl-loop for next-ts = (next-timestamp)
                                           while next-ts
                                           when (string-prefix-p "<" next-ts)
                                           do (setf beg (float-time (org-timestamp-to-time next-ts))
                                                    end (float-time (org-timestamp-to-time next-ts 'end)))
                                           thereis ,pred-form)))
    (save-excursion
      (let ((end-pos (org-entry-end-position))
            beg end)
        (cond ((not (or from to)) (next-timestamp))
              ((and from to) (test-timestamps (and (<= beg to)
                                                   (>= end from))))
              (from (test-timestamps (<= from end)))
              (to (test-timestamps (<= beg to))))))))

(org-ql--defpred ts-inactive (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable" warnings, because we
  ;; pre-process that argument in a macro before this function is called.
  "Return non-nil if current entry has an inactive timestamp in given period.
If no arguments are specified, return non-nil if entry has any
inactive timestamp.

If FROM, return non-nil if entry has an inactive timestamp on or
after FROM.

If TO, return non-nil if entry has an inactive timestamp on or
before TO.

If ON, return non-nil if entry has an inactive timestamp on date
ON.

FROM, TO, and ON should be strings parseable by
`parse-time-string' but may omit the time value."
  ;; TODO: DRY this with the clocked predicate.
  ;; NOTE: FROM and TO are actually expected to be Unix timestamps.  The docstring is written
  ;; for end users, for which the arguments are pre-processed by `org-ql-select'.
  ;; FIXME: This assumes every "clocked" entry is a range.  Unclosed clock entries are not handled.
  (cl-macrolet ((next-timestamp ()
                                `(when (re-search-forward org-element--timestamp-regexp end-pos t)
                                   (save-excursion
                                     (goto-char (match-beginning 0))
                                     (org-element-timestamp-parser))))
                (test-timestamps (pred-form)
                                 `(cl-loop for next-ts = (next-timestamp)
                                           while next-ts
                                           when (string-prefix-p "[" next-ts)
                                           do (setf beg (float-time (org-timestamp-to-time next-ts))
                                                    end (float-time (org-timestamp-to-time next-ts 'end)))
                                           thereis ,pred-form)))
    (save-excursion
      (let ((end-pos (org-entry-end-position))
            beg end)
        (cond ((not (or from to)) (next-timestamp))
              ((and from to) (test-timestamps (and (<= beg to)
                                                   (>= end from))))
              (from (test-timestamps (<= from end)))
              (to (test-timestamps (<= beg to))))))))

;;;;; Date comparison

(defun org-ql--date-type-p (type &optional comparator target-date)
  "Return non-nil if current heading has a date property of TYPE.
TYPE should be a keyword symbol, like :scheduled or :deadline.

With COMPARATOR and TARGET-DATE, return non-nil if entry's
scheduled date compares with TARGET-DATE according to COMPARATOR.
TARGET-DATE may be a string like \"2017-08-05\", or an integer
like one returned by `date-to-day'."
  (when-let (;; FIXME: Add :date selector, since I put it
             ;; in the examples but forgot to actually
             ;; make it.
             (timestamp (org-entry-get (point) (pcase type
                                                 (:deadline "DEADLINE")
                                                 (:scheduled "SCHEDULED")
                                                 (:closed "CLOSED"))))
             (date-element (with-temp-buffer
                             ;; FIXME: Hack: since we're using
                             ;; (org-element-property :type date-element)
                             ;; below, we need this date parsed into an
                             ;; org-element element
                             (insert timestamp)
                             (goto-char 0)
                             (org-element-timestamp-parser))))
    (pcase comparator
      ;; Not comparing, just checking if it has one
      ('nil t)
      ;; Compare dates
      ((pred functionp)
       (let ((target-day-number (cl-typecase target-date
                                  (null (+ (org-get-wdays timestamp) (org-today)))
                                  ;; Append time to target-date because `date-to-day' requires it.
                                  (string (date-to-day (concat target-date " 00:00")))
                                  (integer target-date))))
         (pcase (org-element-property :type date-element)
           ((or 'active 'inactive 'active-range 'inactive-range)
            (funcall comparator
                     (org-time-string-to-absolute
                      (org-element-timestamp-interpreter date-element 'ignore))
                     target-day-number))
           (_ (error "Unknown date-element type \"%s\" in buffer %s at position %s"
                     (org-element-property :type date-element) (current-buffer) (point))))))
      (_ (user-error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string or day-number integer"
                     comparator target-date)))))

(org-ql--defpred planning (&optional comparator target-date)
  "Return non-nil if entry's planning date (deadline or scheduled) compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  ;; FIXME: I think :date selects either :deadline, :scheduled, or :closed, but I'm not sure.
  (org-ql--date-type-p :date comparator target-date))

(org-ql--defpred deadline (&optional comparator target-date)
  "Return non-nil if entry's deadline compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'; or if
omitted, it is determined automatically using
`org-deadline-warning-days'. COMPARATOR should be a
function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  ;; FIXME: This is slightly confusing.  Using plain (deadline) does, and should, select entries
  ;; that have any deadline.  But the common case of wanting to select entries whose deadline is
  ;; within the warning days (either the global setting or that entry's setting) requires the user
  ;; to specify the <= comparator, which is unintuitive.  Maybe it would be better to use that
  ;; comparator by default, and use an 'any comparator to select entries with any deadline.  Of
  ;; course, that would make the deadline selector different from the scheduled, closed, and date
  ;; selectors, which would also be unintuitive.
  (org-ql--date-type-p :deadline comparator target-date))

(org-ql--defpred scheduled (&optional comparator target-date)
  "Return non-nil if entry's scheduled date compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (org-ql--date-type-p :scheduled comparator target-date))

(org-ql--defpred closed (&optional comparator target-date)
  "Return non-nil if entry's closed date compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (org-ql--date-type-p :closed comparator target-date))

(org-ql--defpred date (&optional comparator target-date (type 'active))
  "Return non-nil if Org entry at point has date of TYPE that compares with TARGET-DATE using COMPARATOR.
Checks all Org-formatted timestamp strings in entry.  TYPE may be
`active', `inactive', or `all', to control whether active,
inactive, or all timestamps are checked.  Ranges of each type are
also checked.  TARGET-DATE should be a string parseable by
`date-to-day'.  COMPARATOR should be a function (like `<=')."
  ;; TODO: Deprecate this with a warning, suggest using (ts) instead, and remove (date) from examples.
  ;; MAYBE: This duplicates some code in --date-p, maybe it could be refactored DRYer.
  (let* ((entry-timestamps (save-excursion
                             ;; NOTE: It's important to `save-excursion', otherwise the point will be moved, which will
                             ;; likely cause the action function to fail.  We could wrap the call to the predicate in
                             ;; `save-excursion', but that would do it even when not necessary, which would be slower.
                             (cl-loop while (re-search-forward org-element--timestamp-regexp (org-entry-end-position) t)
                                      collect (match-string 0)))))
    (pcase comparator
      ('nil (pcase type
              ('all entry-timestamps)
              ('active (cl-loop for timestamp in entry-timestamps
                                thereis (string-prefix-p "<" timestamp)))
              ('inactive (cl-loop for timestamp in entry-timestamps
                                  thereis (string-prefix-p "[" timestamp)))
              (_ (user-error "Invalid type for date selector.  May be `active', `inactive', or `all'"))))
      ((pred functionp)
       ;; TODO: Avoid computing target-day-number every time this is called.
       ;; Probably need to make a lambda that has it already defined.
       (let ((target-day-number (cl-typecase target-date
                                  (null nil)  ; Calculated later.
                                  ;; Append time to target-date because `date-to-day' requires it.
                                  (string (date-to-day (concat target-date " 00:00")))
                                  (integer target-date))))
         (cl-loop for timestamp in entry-timestamps
                  for date-element = (with-temp-buffer
                                       ;; MAYBE: Replace with ts.el eventually.
                                       ;; TODO: Parse the element in the re-search-forward loop.
                                       (insert timestamp)
                                       (goto-char 0)
                                       (org-element-timestamp-parser))
                  for this-target-day-number = (or target-day-number
                                                   ;; FIXME: Not sure if it makes sense to check warning
                                                   ;; days for non-planning timestamps, but we'll try it.
                                                   (+ (org-get-wdays timestamp) (org-today)))
                  thereis (when (or (eq 'all type)
                                    (member (org-element-property :type date-element)
                                            (pcase type
                                              ('active '(active active-range))
                                              ('inactive '(inactive inactive-range)))))
                            (funcall comparator (org-time-string-to-absolute
                                                 (org-element-timestamp-interpreter date-element 'ignore))
                                     this-target-day-number)))))
      (_ (user-error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string or day-number integer"
                     comparator target-date)))))

;;;;; Sorting

;; FIXME: These appear to work properly, but it would be good to have tests for them.
;; MAYBE: Add timestamp sorter.  Could be slow in some cases, without clever caching of timestamps per-entry.

(defun org-ql--sort-by (items predicates)
  "Return ITEMS sorted by PREDICATES.
PREDICATES is a list of one or more sorting methods, including:
`deadline', `scheduled', and `priority'."
  ;; FIXME: Test `date' type.
  ;; MAYBE: Use macrolet instead of flet.
  (cl-flet* ((sorter (symbol)
                     (pcase symbol
                       ((or 'deadline 'scheduled)
                        (apply-partially #'org-ql--date-type< (intern (concat ":" (symbol-name symbol)))))
                       ('date #'org-ql--date<)
                       ('priority #'org-ql--priority<)
                       ;; NOTE: 'todo is handled below
                       ;; FIXME: Add more?
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
    (cl-loop for pred in (nreverse predicates)
             do (setq items (if (eq pred 'todo)
                                (sort-by-todo-keyword items)
                              (-sort (sorter pred) items)))
             finally return items)))

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
                       (org-timestamp-format ,ts "%s"))))
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

;;;; Footer

(provide 'org-ql)

;;; org-ql.el ends here
