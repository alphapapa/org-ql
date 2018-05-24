;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'seq)

(require 'dash)

;;;; Variables

(defvar org-ql--today nil)

;;;; Macros

(cl-defmacro org-ql (buffers-or-files pred-body &key (action-fn (lambda (element) element)) sort narrow)
  "Find entries in BUFFERS-OR-FILES that match PRED-BODY, and return the results of running ACTION-FN on each matching entry.

ACTION-FN should take a single argument, which will be the result
of calling `org-element-headline-parser' at each matching entry.

SORT is a user defined sorting function, or an unquoted list of
one or more sorting methods, including: `date', `deadline',
`scheduled', and `priority'.

If NARROW is non-nil, query will run without widening the
buffer (the default is to widen and search the entire buffer)."
  (declare (indent defun))
  `(let ((items (org-ql--query ,buffers-or-files
                               (byte-compile (lambda ()
                                               (cl-symbol-macrolet ((= #'=)
                                                                    (< #'<)
                                                                    (> #'>)
                                                                    (<= #'<=)
                                                                    (>= #'>=))
                                                 ,pred-body)))
                               ,action-fn
                               :narrow ,narrow)))
     (cl-typecase ',sort
       (list (org-ql--sort-by items ',sort))
       (function (funcall ,sort items))
       (null items)
       (t (user-error "SORT must be a function or a list of methods (see documentation)")))))

(defmacro org-ql--fmap (fns &rest body)
  (declare (indent defun))
  `(cl-letf ,(cl-loop for (fn target) in fns
                      collect `((symbol-function ',fn)
                                (symbol-function ,target)))
     ,@body))

;;;; Functions

(cl-defun org-ql--query (buffers-or-files pred action-fn &key narrow)
  "FIXME: Add docstring."
  ;; MAYBE: Set :narrow t  for buffers and nil for files.
  (let* ((buffers-or-files (cl-typecase buffers-or-files
                             (null (list (current-buffer)))
                             (buffer (list buffers-or-files))
                             (list buffers-or-files)
                             (string (list buffers-or-files))))
         ;; TODO: Figure out how to use or reimplement the org-scanner-tags feature.
         ;; (org-use-tag-inheritance t)
         ;; (org-trust-scanner-tags t)
         (org-ql--today (org-today)))
    (-flatten-n 1 (--map (with-current-buffer (cl-typecase it
                                                (buffer it)
                                                (string (or (find-buffer-visiting it)
                                                            (find-file-noselect it)
                                                            (user-error "Can't open file: %s" it))))
                           (mapcar action-fn
                                   (org-ql--filter-buffer :pred pred :narrow narrow)))
                         buffers-or-files))))

(cl-defun org-ql--filter-buffer (&key pred narrow)
  "Return positions of matching headings in current buffer.
Headings should return non-nil for any ANY-PREDS and nil for all
NONE-PREDS.  If NARROW is non-nil, buffer will not be widened
first."
  ;; Cache `org-today' so we don't have to run it repeatedly.
  (cl-letf ((today org-ql--today))
    (org-ql--fmap ((category #'org-ql--category-p)
                   (date #'org-ql--date-plain-p)
                   (deadline #'org-ql--deadline-p)
                   (scheduled #'org-ql--scheduled-p)
                   (closed #'org-ql--closed-p)
                   (habit #'org-ql--habit-p)
                   (priority #'org-ql--priority-p)
                   (todo #'org-ql--todo-p)
                   (done #'org-ql--done-p)
                   (tags #'org-ql--tags-p)
                   (property #'org-ql--property-p)
                   (regexp #'org-ql--regexp-p)
                   (org-back-to-heading #'outline-back-to-heading))
      (save-excursion
        (save-restriction
          (unless narrow
            (widen))
          (goto-char (point-min))
          (when (org-before-first-heading-p)
            (outline-next-heading))
          (cl-loop when (funcall pred)
                   collect (org-element-headline-parser (line-end-position))
                   while (outline-next-heading)))))))

;;;;; Predicates

(defun org-ql--category-p (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES."
  (when-let ((category (org-get-category (point))))
    (cl-typecase categories
      (null t)
      (otherwise (member category categories)))))

(defun org-ql--todo-p (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS."
  (when-let ((state (org-get-todo-state)))
    (cl-typecase keywords
      (null t)
      (list (member state keywords))
      (symbol (member state (symbol-value keywords)))
      (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

(defsubst org-ql--done-p ()
  (apply #'org-ql--todo-p org-done-keywords-for-agenda))

(defun org-ql--tags-p (&rest tags)
  "Return non-nil if current heading has one or more of TAGS."
  ;; TODO: Try to use `org-make-tags-matcher' to improve performance.  It would be nice to not have
  ;; to run `org-get-tags-at' for every heading, especially with inheritance.
  (when-let ((tags-at (org-get-tags-at (point) (not org-use-tag-inheritance))))
    (cl-typecase tags
      (null t)
      (otherwise (seq-intersection tags tags-at)))))

(defun org-ql--date-p (type &optional comparator target-date)
  "Return non-nil if current heading has a date property of TYPE.
TYPE should be a keyword symbol, like :scheduled or :deadline.

With COMPARATOR and TARGET-DATE, return non-nil if entry's
scheduled date compares with TARGET-DATE according to COMPARATOR.
TARGET-DATE may be a string like \"2017-08-05\", or an integer
like one returned by `date-to-day'."
  (when-let ((timestamp (pcase type
                          ;; FIXME: Add :date selector, since I put it
                          ;; in the examples but forgot to actually
                          ;; make it.
                          (:deadline (org-entry-get (point) "DEADLINE"))
                          (:scheduled (org-entry-get (point) "SCHEDULED"))
                          (:closed (org-entry-get (point) "CLOSED"))))
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
                                  ;; Append time to target-date
                                  ;; because `date-to-day' requires it
                                  (string (date-to-day (concat target-date " 00:00")))
                                  (integer target-date))))
         (pcase (org-element-property :type date-element)
           ((or 'active 'inactive)
            (funcall comparator
                     (org-time-string-to-absolute
                      (org-element-timestamp-interpreter date-element 'ignore))
                     target-day-number))
           (error "Unknown date-element type: %s" (org-element-property :type date-element)))))
      (otherwise (user-error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string or day-number integer"
                             comparator target-date)))))

(defsubst org-ql--date-plain-p (&optional comparator target-date)
  (org-ql--date-p :date comparator target-date))
(defsubst org-ql--deadline-p (&optional comparator target-date)
  ;; FIXME: This is slightly confusing.  Using plain (deadline) does, and should, select entries
  ;; that have any deadline.  But the common case of wanting to select entries whose deadline is
  ;; within the warning days (either the global setting or that entry's setting) requires the user
  ;; to specify the <= comparator, which is unintuitive.  Maybe it would be better to use that
  ;; comparator by default, and use an 'any comparator to select entries with any deadline.  Of
  ;; course, that would make the deadline selector different from the scheduled, closed, and date
  ;; selectors, which would also be unintuitive.
  (org-ql--date-p :deadline comparator target-date))
(defsubst org-ql--scheduled-p (&optional comparator target-date)
  (org-ql--date-p :scheduled comparator target-date))
(defsubst org-ql--closed-p (&optional comparator target-date)
  (org-ql--date-p :closed comparator target-date))

(defun org-ql--priority-p (&optional comparator-or-priority priority)
  "Return non-nil if current heading has a certain priority.
COMPARATOR-OR-PRIORITY should be either a comparator function,
like `<=', or a priority string, like \"A\" (in which case (\` =)
'will be the comparator).  If COMPARATOR-OR-PRIORITY is a
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

(defun org-ql--habit-p ()
  (org-is-habit-p))

(defun org-ql--regexp-p (regexp)
  "Return non-nil if current entry matches REGEXP."
  (let ((end (or (save-excursion
                   (outline-next-heading))
                 (point-max))))
    (save-excursion
      (goto-char (line-beginning-position))
      (re-search-forward regexp end t))))

(defun org-ql--property-p (property &optional value)
  "Return non-nil if current entry has PROPERTY, and optionally VALUE."
  (pcase property
    ('nil (user-error "Property matcher requires a PROPERTY argument."))
    (_ (pcase value
         ('nil
          ;; Check that PROPERTY exists
          (org-entry-get (point) property))
         (_
          ;; Check that PROPERTY has VALUE
          (string-equal value (org-entry-get (point) property 'selective)))))))

;;;;; Sorting

;; FIXME: These appear to work properly, but it would be good to have tests for them.

(defun org-ql--sort-by (items predicates)
  "Return ITEMS sorted by PREDICATES.
PREDICATES is a list of one or more sorting methods, including:
`deadline', `scheduled', and `priority'."
  ;; FIXME: Test `date' type.
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
                                          (sorted-groups (--sort (let* ((it-pos (todo-keyword-pos (car it)))
                                                                        (other-pos (todo-keyword-pos (car other))))
                                                                   (cond ((and it-pos other-pos)
                                                                          (< it-pos other-pos))
                                                                         (it-pos t)
                                                                         (other-pos nil)))
                                                                 grouped-items)))
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
  (cl-flet ((ts (ts)
                (when ts
                  (org-timestamp-format ts "%s"))))
    (let* ((a-ts (ts a))
           (b-ts (ts b)))
      (cond ((and a-ts b-ts)
             (string< a-ts b-ts))
            (a-ts t)
            (b-ts nil)))))

(defun org-ql--priority< (a b)
  "Return non-nil if A's priority is higher than B's.
A and B are Org headline elements."
  (cl-flet ((priority (item)
                      (org-element-property :priority item)))
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
