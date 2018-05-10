;; -*- lexical-binding: t; -*-


;;; Commentary:

;; This is just a proof-of-concept for how the agenda code might be
;; written in a more functional way, to avoid making multiple passes
;; through each file when building a multi-day agenda.

;; Unfortunately, however, it's more like a proof-of-nope, because
;; it's significantly slower than the existing agenda code.  Profiling
;; shows that =org-element-parse-buffer= is just not a fast function,
;; so most of the time spent is in that function, parsing the whole
;; buffer into a lisp tree.  The existing agenda code is not written
;; in a functional style, and it makes multiple passes through each
;; file when preparing a multi-day agenda view, but it avoids parsing
;; the /entire/ buffer the way =org-element-parse-buffer= does.

;; So I guess this approach won't work, unless someone can come up
;; with a version of =org-element-parse-buffer= that's about 10x
;; faster.

;;;; Principles

;;;;; Work on elements

;; We want to work on elements as returned by =org-element-parse-buffer=.  We only convert to strings at the very end, when we send the list of those to =org-agenda-finalize-entries=.

;;;;; Preserve the call to =org-agenda-finalize-entries=

;; We want to preserve the final call to =org-agenda-finalize-entries= to preserve compatibility with other packages and functions.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'dash)
(require 'cl-lib)
(require 'seq)

;;;; Macros

(defmacro org-agenda-ng--fmap (fns &rest body)
  (declare (indent defun))
  `(cl-letf ,(cl-loop for (fn target) in fns
                      collect `((symbol-function ',fn)
                                (symbol-function ,target)))
     ,@body))

(cl-defmacro org-agenda-ng (files pred-body)
  "Display an agenda-like buffer of entries in FILES that match PRED-BODY."
  (declare (indent defun))
  `(org-agenda-ng--agenda ,files
                          (byte-compile (lambda ()
                                          ,pred-body))
                          #'org-agenda-ng--format-element))

(cl-defmacro org-ql (files pred-body &key (action-fn (lambda (element) (list element))))
  "Find entries in FILES that match PRED-BODY, and return the results of running ACTION-FN on each matching entry.

ACTION-FN should take a single argument, which will be the result
of calling `org-element-headline-parser' at each matching entry."
  (declare (indent defun))
  `(org-ql--query ,files
                  (byte-compile (lambda ()
                                  ,pred-body))
                  #',action-fn))


;;;; Commands

;; TODO: Move the action-fn down into --filter-buffer, so users can avoid calling the
;; headline-parser when they don't need it.

(defvar org-ql--today nil)

(cl-defun org-ql--query (files pred action-fn)
  (setq files (cl-typecase files
                (null (list (buffer-file-name (current-buffer))))
                (list files)
                (string (list files))))
  (mapc 'find-file-noselect files)
  (let* ((org-use-tag-inheritance t)
         (org-scanner-tags nil)
         (org-trust-scanner-tags t)
         (org-ql--today (org-today)))
    (-flatten-n 1 (--map (with-current-buffer (find-buffer-visiting it)
                           (mapcar action-fn
                                   (org-agenda-ng--filter-buffer :pred pred)))
                         files))))

(cl-defun org-agenda-ng--agenda (files pred action-fn)
  (let* ((entries (org-ql--query files pred action-fn))
         (result-string (org-agenda-finalize-entries entries 'agenda))
         (target-buffer (get-buffer-create "test-agenda-ng")))
    (with-current-buffer target-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert result-string)
      (read-only-mode 1)
      (pop-to-buffer (current-buffer))
      (org-agenda-finalize))))

;;;; Functions

(cl-defun org-agenda-ng--filter-buffer (&key pred)
  "Return positions of matching headings in current buffer.
Headings should return non-nil for any ANY-PREDS and nil for all
NONE-PREDS."
  ;; Cache `org-today' so we don't have to run it repeatedly.
  (cl-letf ((today org-ql--today))
    (org-agenda-ng--fmap ((category #'org-agenda-ng--category-p)
                          (date #'org-agenda-ng--date-plain-p)
                          (deadline #'org-agenda-ng--deadline-p)
                          (scheduled #'org-agenda-ng--scheduled-p)
                          (closed #'org-agenda-ng--closed-p)
                          (habit #'org-agenda-ng--habit-p)
                          (priority #'org-agenda-ng--priority-p)
                          (todo #'org-agenda-ng--todo-p)
                          (done #'org-agenda-ng--done-p)
                          (tags #'org-agenda-ng--tags-p)
                          (org-back-to-heading #'outline-back-to-heading))
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (org-before-first-heading-p)
         (outline-next-heading))
       (cl-loop when (funcall pred)
                collect (org-element-headline-parser (line-end-position))
                while (outline-next-heading))))))

(defun org-agenda-ng--format-relative-date (difference)
  "Return relative date string for DIFFERENCE.
DIFFERENCE should be an integer number of days, positive for
dates in the past, and negative for dates in the future."
  (cond ((> difference 0)
         (format "%sd ago" difference))
        ((< difference 0)
         (format "in %sd" (* -1 difference)))
        (t "today")))

;;;; Faces/properties

(defun org-agenda-ng--add-markers (element)
  "Return ELEMENT with marker properties added."
  (let* ((marker (org-agenda-new-marker (org-element-property :begin element)))
         (properties (--> (second element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (second element) properties)
    element))

(defun org-agenda-ng--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with its text-properties set according to its property list.
Its property list should be the second item in the list, as returned by `org-element-parse-buffer'."
  (let* ((properties (second element))
         ;; Remove the :parent property, which so bloats the size of
         ;; the properties list that it makes it essentially
         ;; impossible to debug, because Emacs takes approximately
         ;; forever to show it in the minibuffer or with
         ;; `describe-text-properties'.  FIXME: Shouldn't be necessary
         ;; anymore since we're not parsing the whole buffer.

         ;; Also, remove ":" from key symbols.  FIXME: It would be
         ;; better to avoid this somehow.  At least, we should use a
         ;; function to convert plists to alists, if possible.
         (properties (cl-loop for (key val) on properties by #'cddr
                              for key = (intern (cl-subseq (symbol-name key) 1))
                              unless (member key '(parent))
                              append (list key val)))
         (title (--> (org-agenda-ng--add-faces element)
                     (org-element-property :raw-value it)
                     (org-link-display-format it)
                     ))
         (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                 (org-agenda-ng--add-todo-face it)))
         ;; FIXME: Figure out whether I should use `org-agenda-use-tag-inheritance' or `org-use-tag-inheritance', etc.
         (tag-list (if org-use-tag-inheritance
                       (if-let ((marker (or (org-element-property :org-hd-marker element)
                                            (org-element-property :org-marker element)
                                            (org-element-property :begin element))))
                           (org-get-tags-at marker (not org-use-tag-inheritance))
                         ;; No marker found
                         (warn "No marker found for item: %s" title)
                         (org-element-property :tags element))
                     (org-element-property :tags element)))
         (tag-string (-some--> tag-list
                               (s-join ":" it)
                               (s-wrap it ":")
                               (org-add-props it nil 'face 'org-tag)))
         (level (org-element-property :level element))
         (category (org-element-property :category element))
         (priority-string (-some->> (org-element-property :priority element)
                                    (char-to-string)
                                    (format "[#%s]")
                                    (org-agenda-ng--add-priority-face)))
         (habit-property (org-with-point-at (org-element-property :begin element)
                           (when (org-is-habit-p)
                             (org-habit-parse-todo))))
         (due-string (pcase (org-element-property :relative-due-date element)
                       ('nil "")
                       (string (format " %s " (org-add-props string nil 'face 'underline)))))
         (string (s-join " " (list todo-keyword priority-string title due-string tag-string))))
    (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
    ;; Add all the necessary properties and faces to the whole string
    (--> string
         ;; FIXME: Use proper prefix
         (concat "  " it)
         (org-add-props it properties
           'todo-state todo-keyword
           'tags tag-list
           'org-habit-p habit-property))))

(defun org-agenda-ng--add-faces (element)
  (->> element
       (org-agenda-ng--add-scheduled-face)
       (org-agenda-ng--add-deadline-face)))

(defun org-agenda-ng--add-priority-face (string)
  "Return STRING with priority face added."
  (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
    (let ((face (org-get-priority-face (string-to-char (match-string 2)))))
      (org-add-props string nil 'face face 'font-lock-fontified t))))

(defun org-agenda-ng--add-scheduled-face (element)
  "Add faces to ELEMENT's title for its scheduled status."
  ;; NOTE: Also adding prefix
  (if-let ((scheduled-date (org-element-property :scheduled element)))
      (let* ((show-all (or (eq org-agenda-repeating-timestamp-show-all t)
                           (member todo-keyword org-agenda-repeating-timestamp-show-all)))
             (raw-value (org-element-property :raw-value scheduled-date))
             (sexp-p (string-prefix-p "%%" raw-value))
             (today-day-number (org-today))
             (current-day-number
              ;; FIXME: This is supposed to be the, shall we say,
              ;; pretend, or perspective, day number that this pass
              ;; through the agenda is being made for.  We need to
              ;; either set this in the calling function, set it here,
              ;; or accomplish this in a different way.  See
              ;; `org-agenda-get-scheduled' and where `date' is set in
              ;; `org-agenda-list'.
              today-day-number)
             (scheduled-day-number (org-time-string-to-absolute
                                    (org-element-timestamp-interpreter scheduled-date 'ignore)))
             (difference-days (- today-day-number scheduled-day-number))
             (relative-due-date (pcase difference-days
                                  (0 "today")
                                  (_ (org-add-props (org-agenda-ng--format-relative-date difference-days) nil
                                       'help-echo (org-element-property :raw-value scheduled-date)))))
             (repeat-day-number (cond (sexp-p (org-time-string-to-absolute scheduled-date))
                                      ((< today-day-number scheduled-day-number) scheduled-day-number)
                                      (t (org-time-string-to-absolute
                                          raw-value
                                          (if show-all
                                              current-day-number
                                            today-day-number)
                                          'future
                                          ;; FIXME: I don't like
                                          ;; calling `current-buffer'
                                          ;; here.  If the element has
                                          ;; a marker, we should use
                                          ;; that.
                                          (current-buffer)
                                          (org-element-property :begin element)))))
             (todo-keyword (org-element-property :todo-keyword element))
             (face (cond ((member todo-keyword org-done-keywords) 'org-agenda-done)
                         ((= today-day-number scheduled-day-number) 'org-scheduled-today)
                         ((> today-day-number scheduled-day-number) 'org-scheduled-previously)
                         (t 'org-scheduled)))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (second element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date)))
             (prefix (cl-destructuring-bind (first next) org-agenda-scheduled-leaders
                       (cond ((> scheduled-day-number today-day-number)
                              ;; Future
                              first)
                             ((and (not show-all)
                                   (= repeat today-day-number)))
                             ((= today-day-number scheduled-day-number)
                              ;; Today
                              first)
                             (t
                              ;; Subsequent reminders.  Count from base schedule.
                              (format next (1+ (- today-day-number scheduled-day-number))))))))
        (list (car element)
              properties))
    ;; Not scheduled
    element))

(defun org-agenda-ng--add-deadline-face (element)
  "Add faces to ELEMENT's title for its deadline status.
Also store relative due date as string in `:relative-due-date'
property."
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
             (difference-days (- today-day-number deadline-day-number))
             (relative-due-date (pcase difference-days
                                  (0 "today")
                                  (_ (org-add-props (org-agenda-ng--format-relative-date difference-days) nil
                                       'help-echo (org-element-property :raw-value deadline-date)))))
             (todo-keyword (org-element-property :todo-keyword element))
             (done-p (member todo-keyword org-done-keywords))
             (today-p (= today-day-number deadline-day-number))
             (deadline-passed-fraction (--> (- deadline-day-number today-day-number)
                                            (float it)
                                            (/ it (max org-deadline-warning-days 1))
                                            (- 1 it)))
             (face (org-agenda-deadline-face deadline-passed-fraction))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (second element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date))))
        (list (car element)
              properties))
    ;; No deadline
    element))

(defun org-agenda-ng--add-todo-face (keyword)
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;; Predicates

(defun org-agenda-ng--category-p (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES."
  (when-let ((category (org-get-category (point))))
    (cl-typecase categories
      (null t)
      (otherwise (member category categories)))))

(defun org-agenda-ng--todo-p (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS."
  (when-let ((state (org-get-todo-state)))
    (cl-typecase keywords
      (null t)
      (list (member state keywords))
      (symbol (member state (symbol-value keywords)))
      (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

(defsubst org-agenda-ng--done-p ()
  (apply #'org-agenda-ng--todo-p org-done-keywords-for-agenda))

(defun org-agenda-ng--tags-p (&rest tags)
  "Return non-nil if current heading has TAGS."
  ;; TODO: Try to use `org-make-tags-matcher' to improve performance.
  (when-let ((tags-at (org-get-tags-at (point)
                                       ;; FIXME: Would be nice to not check this for every heading checked.
                                       ;; FIXME: Figure out whether I should use `org-agenda-use-tag-inheritance' or `org-use-tag-inheritance', etc.
                                       ;; (not (member 'agenda org-agenda-use-tag-inheritance))
                                       org-use-tag-inheritance)))
    (cl-typecase tags
      (null t)
      (otherwise (seq-intersection tags tags-at)))))

(defun org-agenda-ng--date-p (type &optional comparator target-date)
  ;; TODO: Make this a macro so we don't have to quote comparator and test it for every item.

  ;; TODO: Make separate date, scheduled, deadline macros.
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
                                  ;; Append time to target-date
                                  ;; because `date-to-day' requires it
                                  (null (+ (org-get-wdays timestamp) (org-today)))
                                  (string (date-to-day (concat target-date " 00:00")))
                                  (integer target-date))))
         (pcase (org-element-property :type date-element)
           ((or 'active 'inactive)
            (funcall comparator
                     (org-time-string-to-absolute
                      (org-element-timestamp-interpreter date-element 'ignore))
                     target-day-number))
           (error "Unknown date-element type: %s" (org-element-property :type date-element)))))
      (otherwise (error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string" comparator target-date)))))

(defsubst org-agenda-ng--date-plain-p (&optional comparator target-date)
  (org-agenda-ng--date-p :date comparator target-date))
(defsubst org-agenda-ng--deadline-p (&optional comparator target-date)
  ;; FIXME: This is slightly confusing.  Using plain (deadline) does, and should, select entries
  ;; that have any deadline.  But the common case of wanting to select entries whose deadline is
  ;; within the warning days (either the global setting or that entry's setting) requires the user
  ;; to specify the <= comparator, which is unintuitive.  Maybe it would be better to use that
  ;; comparator by default, and use an 'any comparator to select entries with any deadline.  Of
  ;; course, that would make the deadline selector different from the scheduled, closed, and date
  ;; selectors, which would also be unintuitive.
  (org-agenda-ng--date-p :deadline comparator target-date))
(defsubst org-agenda-ng--scheduled-p (&optional comparator target-date)
  (org-agenda-ng--date-p :scheduled comparator target-date))
(defsubst org-agenda-ng--closed-p (&optional comparator target-date)
  (org-agenda-ng--date-p :closed comparator target-date))

(defmacro org-agenda-ng--priority-p (&optional comparator-or-priority priority)
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
    `(when-let ((item-priority (save-excursion
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
       (funcall ',comparator ,priority item-priority))))

(defun org-agenda-ng--habit-p ()
  (org-is-habit-p))
