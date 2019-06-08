;;; org-ql.el --- Query language for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-ql
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (dash "2.13") (org "9.0"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; `org-ql' is a lispy query language for Org files.  It allows you to
;; find Org entries matching certain criteria and perform actions on
;; them, such as collecting their parsed representation with
;; `org-element' (the default action).

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'seq)

(require 'dash)

;;;; Variables

(defvar org-ql--today nil)

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

(cl-defmacro org-ql--defpredicate (name args docstring &rest body)
  "FIXME: docstring"
  (declare (debug (symbolp listp stringp body))
           (indent defun))
  (let ((fn-name (intern (concat "org-ql--predicate-" (symbol-name name))))
        (pred-name (intern (symbol-name name))))
    `(progn
       (push (list :name ',pred-name :fn ',fn-name :docstring ,docstring) org-ql-predicates)
       (cl-defun ,fn-name ,args ,docstring ,@body))))

(cl-defmacro org-ql (buffers-or-files pred-body &key sort narrow markers
                                      (action '(org-element-headline-parser (line-end-position))))
  "Find entries in BUFFERS-OR-FILES that match PRED-BODY, and return the results of running ACTION-FN on each matching entry.

ACTION is a sexp which will be evaluated at each matching entry
with point at the beginning of its heading.  It is passed to
`org-ql--query' as a lambda.  By default, `org-element-headline-parser'
 is called to return an Org element.

SORT is a user defined sorting function, or an unquoted list of
one or more sorting methods, including: `date', `deadline',
`scheduled', `todo', and `priority'.

If NARROW is non-nil, query will run without widening the
buffer (the default is to widen and search the entire buffer).

If MARKERS is non-nil, `org-agenda-ng--add-markers' is used to
add markers to each item, pointing to the item in its source
buffer.  In this case, ACTION should return an Org element."
  (declare (indent defun))
  (setq action (pcase markers
                 ('nil `(lambda ()
                          ,action))
                 (_ `(lambda ()
                       ;; FIXME: Document that, when markers is t, `action' should return an Org
                       ;; headline element, which --add-markers works with.  On the other hand,
                       ;; maybe this should be on the agenda-ng side.
                       (->> ,action
                            org-ql--add-markers)))))
  `(org-ql--query ,buffers-or-files
     ',pred-body
     :action ,action
     :narrow ,narrow
     :sort ',sort))

(defmacro org-ql--flet (fns &rest body)
  ;; FIXME: Docstring.
  ;; MAYBE: Use `noflet'.
  (declare (indent defun) (debug (listp body)))
  `(cl-letf ,(cl-loop for (fn target) in fns
                      collect `((symbol-function ',fn)
                                (symbol-function ,target)))
     ,@body))

;;;; Functions

(cl-defun org-ql--query (buffers-or-files query &key action narrow sort)
  "Return items matching QUERY in BUFFERS-OR-FILES.

QUERY is an `org-ql' query sexp.

ACTION is a function which is called on each matching entry, with
point at the beginning of its heading.  For example,
`org-element-headline-parser' may be used to parse an entry into
an Org element (note that it must be called with a limit
argument, so a lambda must be used to do so).  Also see
`org-ql--add-markers', which may be used to add markers
compatible with Org Agenda code.

If NARROW is non-nil, buffers are not widened.

SORT is either nil, in which case items are not sorted; or one or
a list of defined `org-ql' sorting methods: `date', `deadline',
`scheduled', `todo', and `priority'."
  (declare (indent defun))
  (let* ((sources (pcase buffers-or-files
                    (`nil (list (current-buffer)))
                    ((pred listp) buffers-or-files)
                    (_                  ; Buffer or string
                     (list buffers-or-files))))
         (predicate (byte-compile `(lambda ()
                                     (cl-symbol-macrolet ((today org-ql--today) ; Necessary because of byte-compiling the lambda
                                                          (= #'=)
                                                          (< #'<)
                                                          (> #'>)
                                                          (<= #'<=)
                                                          (>= #'>=))
                                       ,query))))
         (action (byte-compile action))
         ;; TODO: Figure out how to use or reimplement the org-scanner-tags feature.
         ;; (org-use-tag-inheritance t)
         ;; (org-trust-scanner-tags t)
         (org-ql--today (org-today))
         (items (->> sources
                     ;; List buffers
                     (--map (cl-etypecase it
                              (buffer it)
                              (string (or (find-buffer-visiting it)
                                          (when (file-readable-p it)
                                            ;; It feels unintuitive that `find-file-noselect' returns
                                            ;; a buffer if the filename doesn't exist.
                                            (find-file-noselect it))
                                          (user-error "Can't open file: %s" it)))))
                     ;; Filter buffers (i.e. select items)
                     (--map (with-current-buffer it
                              (unless (derived-mode-p 'org-mode)
                                (user-error "Not an Org buffer: %s" (buffer-name)))
                              (org-ql--select-cached :predicate predicate :action action :narrow narrow)))
                     ;; Flatten items
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
      (_ (user-error "SORT must be either nil, or one or a list of the defined sorting methods (see documentation)")))))

(define-hash-table-test 'org-ql-hash-test #'equal (lambda (args)
                                                    (sxhash-equal (prin1-to-string args))))

(defun org-ql--select-cached (&rest args)
  "Return results for ARGS and current buffer using cache."
  ;; MAYBE: Timeout cached queries.  Probably not necessarily since they will be removed when a
  ;; buffer is closed, or when a query is run after modifying a buffer.
  (if-let* ((buffer-cache (gethash (current-buffer) org-ql-cache))
            (query-cache (cadr buffer-cache))
            (modified-tick (car buffer-cache))
            (buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
            (cached-result (gethash args query-cache)))
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
      new-result)))

(cl-defun org-ql--select (&key predicate action narrow)
  "Return results of mapping function ACTION across entries in current buffer matching function PREDICATE.
If NARROW is non-nil, buffer will not be widened."
  ;; Since the mappings are done at runtime, macros like `flet' can't be used, so we do it manually.
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
              (cl-loop when (funcall predicate)
                       collect (funcall action)
                       while (outline-next-heading)))))
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
  "Signal an error if any of the forms in BODY do not have their preconditions met.
Or, when possible, fix the problem."
  (cl-flet ((check (symbol)
                   (cl-case symbol
                     ('done (unless org-done-keywords
                              ;; NOTE: This check needs to be done from within the Org buffer being checked.
                              (error "Variable `org-done-keywords' is nil.  Are you running this from an Org buffer?")))
		     ('habit (unless (featurep 'org-habit)
			       (require 'org-habit))))))
    (cl-loop for elem in form
	     if (consp elem)
	     do (progn
		  (check (car elem))
		  (org-ql--sanity-check-form (cdr elem)))
	     else do (check elem))))

;;;;; Predicates

(org-ql--defpredicate category (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES (a list of strings)."
  (when-let ((category (org-get-category (point))))
    (cl-typecase categories
      (null t)
      (otherwise (member category categories)))))

(org-ql--defpredicate todo (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS (a list of strings)."
  (when-let ((state (org-get-todo-state)))
    (cl-typecase keywords
      (null t)
      (list (member state keywords))
      (symbol (member state (symbol-value keywords)))
      (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

(org-ql--defpredicate done ()
  "Return non-nil if entry's TODO keyword is in `org-done-keywords'."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (or (apply #'org-ql--todo-p org-done-keywords)))

(org-ql--defpredicate tags (&rest tags)
  "Return non-nil if current heading has one or more of TAGS (a list of strings)."
  ;; TODO: Try to use `org-make-tags-matcher' to improve performance.  It would be nice to not have
  ;; to run `org-get-tags-at' for every heading, especially with inheritance.
  (when-let ((tags-at (org-get-tags-at (point) (not org-use-tag-inheritance))))
    (cl-typecase tags
      (null t)
      (otherwise (seq-intersection tags tags-at)))))

(org-ql--defpredicate level (level-or-comparator &optional level)
  "Return non-nil if current heading's outline level matches LEVEL with COMPARATOR.

If LEVEL is nil, LEVEL-OR-COMPARATOR should be an integer level,
which will be tested for equality to the heading's outline level.
If LEVEL is non-nil, LEVEL-OR-COMPARATOR should be a comparator
function (like `<=')."
  ;; NOTE: It might be necessary to take into account `org-odd-levels'; see docstring for
  ;; `org-outline-level'.
  (when-let ((outline-level (org-outline-level)))
    (pcase level
      ;; Check for equality
      ((pred null) (= outline-level level-or-comparator))
      ;; Check with comparator
      (_ (funcall level-or-comparator outline-level level)))))

(org-ql--defpredicate priority (&optional comparator-or-priority priority)
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

(org-ql--defpredicate habit ()
  "Return non-nil if entry is a habit."
  (org-is-habit-p))

(org-ql--defpredicate regexp (regexp)
  "Return non-nil if current entry matches REGEXP (a regexp string)."
  (let ((end (or (save-excursion
                   (outline-next-heading))
                 (point-max))))
    (save-excursion
      (goto-char (line-beginning-position))
      (re-search-forward regexp end t))))

(org-ql--defpredicate heading (regexp)
  "Return non-nil if current entry's heading matches REGEXP (a regexp string)."
  (string-match regexp (org-get-heading 'no-tags 'no-todo)))

(org-ql--defpredicate property (property &optional value)
  "Return non-nil if current entry has PROPERTY (a string), and optionally VALUE (a string)."
  (pcase property
    ('nil (user-error "Property matcher requires a PROPERTY argument."))
    (_ (pcase value
         ('nil
          ;; Check that PROPERTY exists
          (org-entry-get (point) property))
         (_
          ;; Check that PROPERTY has VALUE
          (string-equal value (org-entry-get (point) property 'selective)))))))

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
           ((or 'active 'inactive)
            (funcall comparator
                     (org-time-string-to-absolute
                      (org-element-timestamp-interpreter date-element 'ignore))
                     target-day-number))
           (_ (error "Unknown date-element type: %s" (org-element-property :type date-element))))))
      (_ (user-error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string or day-number integer"
                     comparator target-date)))))

(org-ql--defpredicate planning (&optional comparator target-date)
  "Return non-nil if entry's planning date (deadline or scheduled) compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  ;; FIXME: I think :date selects either :deadline, :scheduled, or :closed, but I'm not sure.
  (org-ql--date-type-p :date comparator target-date))

(org-ql--defpredicate deadline (&optional comparator target-date)
  "Return non-nil if entry's deadline compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  ;; FIXME: This is slightly confusing.  Using plain (deadline) does, and should, select entries
  ;; that have any deadline.  But the common case of wanting to select entries whose deadline is
  ;; within the warning days (either the global setting or that entry's setting) requires the user
  ;; to specify the <= comparator, which is unintuitive.  Maybe it would be better to use that
  ;; comparator by default, and use an 'any comparator to select entries with any deadline.  Of
  ;; course, that would make the deadline selector different from the scheduled, closed, and date
  ;; selectors, which would also be unintuitive.
  (org-ql--date-type-p :deadline comparator target-date))

(org-ql--defpredicate scheduled (&optional comparator target-date)
  "Return non-nil if entry's scheduled date compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (org-ql--date-type-p :scheduled comparator target-date))

(org-ql--defpredicate closed (&optional comparator target-date)
  "Return non-nil if entry's closed date compares with TARGET-DATE using COMPARATOR.
TARGET-DATE should be a string parseable by `date-to-day'.
COMPARATOR should be a function (like `<=')."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (org-ql--date-type-p :closed comparator target-date))

(org-ql--defpredicate date (&optional comparator target-date (type 'active))
  "Return non-nil if Org entry at point has date of TYPE that compares with TARGET-DATE using COMPARATOR.
Checks all Org-formatted timestamp strings in entry.  TYPE may be
`active', `inactive', or `all', to control whether active,
inactive, or all timestamps are checked.  Ranges of each type are
also checked.  TARGET-DATE should be a string parseable by
`date-to-day'.  COMPARATOR should be a function (like `<=')."
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
             (todo-keyword-pos (keyword)
                               ;; MAYBE: Would it be faster to precompute these and do an alist lookup?
                               (cl-position keyword org-todo-keywords-1 :test #'string=))
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
  "Return non-nil if A's deadline or scheduled element property is earlier than B's.
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
