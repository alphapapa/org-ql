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

(cl-defmacro org-agenda-ng (files &rest pred-body)
  (declare (indent defun))
  `(org-agenda-ng--agenda :files ,files
                          :pred (lambda ()
                                  ,@pred-body)))

;;;; Commands

(cl-defun org-agenda-ng--agenda (&key files pred)
  (setq files (cl-typecase files
                (null (list (buffer-file-name (current-buffer))))
                (list files)
                (string (list files))))
  (mapc 'find-file-noselect files)
  (let* ((org-use-tag-inheritance t)
         (entries (-flatten (--map (with-current-buffer (find-buffer-visiting it)
                                     (mapcar #'org-agenda-ng--format-element
                                             (org-agenda-ng--filter-buffer :pred pred)))
                                   files)))
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
  (org-agenda-ng--fmap ((category #'org-agenda-ng--category-p)
                        (date #'org-agenda-ng--date-p)
                        (habit #'org-agenda-ng--habit-p)
                        (todo #'org-agenda-ng--todo-p)
                        (tags #'org-agenda-ng--tags-p))
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (org-before-first-heading-p)
       (outline-next-heading))
     (cl-loop when (funcall pred)
              collect (let* ((next-heading (save-excursion
                                             ;; Limit next search to up to the next heading.  It
                                             ;; would be nice to avoid doing this extra
                                             ;; re-search-forward, but I don't see any way to avoid
                                             ;; it.
                                             (outline-next-heading)
                                             (point)))
                             (limit (save-excursion
                                      ;; Skip drawers and planning lines (see
                                      ;; `org-agenda-get-some-entry-text'.  I wish there were a
                                      ;; cleaner, more canonical way to do this.)
                                      (if (re-search-forward (rx (or (repeat 2 "\n")
                                                                     (regexp ;; This is org-drawer-regexp
                                                                      "^[ 	]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ 	]*$")
                                                                     (eval (concat "^[ \t]*" org-keyword-time-regexp
                                                                                   ".*\n?"))))
                                                             next-heading 'noerror)
                                          (point)
                                        (point-max)))))
                        ;; NOTE: As an alternative to the two searches above, we could just move one
                        ;; or two lines down so that the headline parser can be sure to get the
                        ;; planning lines.  And that does work fine in my limited testing.  But then
                        ;; we have to keep in mind that the headline parser will never see property
                        ;; drawers, so if we need to search properties, we have to do that manually
                        ;; in predicates.  So I'm going to leave it this way for now.  It should
                        ;; probably be profiled both ways to see what the performance impact is.
                        (org-element-headline-parser limit))
              while (outline-next-heading)))))

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
         (tag-list (if org-agenda-use-tag-inheritance
                       (if-let ((marker (or (org-element-property :org-hd-marker element)
                                            (org-element-property :org-marker element)
                                            (org-element-property :begin element))))
                           (org-with-point-at marker
                             (org-get-tags-at))
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
         (string (s-join " " (list todo-keyword priority-string title tag-string))))
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
                              (plist-put it :title title)))
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
  "Add faces to ELEMENT's title for its deadline status."
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
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
                              (plist-put it :title title))))
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

(defun org-agenda-ng--tags-p (&rest tags)
  "Return non-nil if current heading has TAGS."
  ;; TODO: Try to use `org-make-tags-matcher' to improve performance.
  (when-let ((tags-at (org-get-tags-at (point)
                                       ;; FIXME: Would be nice to not check this for every heading checked.
                                       (not (member 'agenda org-agenda-use-tag-inheritance)))))
    (cl-typecase tags
      (null t)
      (otherwise (seq-intersection tags tags-at)))))

(defun org-agenda-ng--date-p (type &optional comparator target-date)
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
      ((and (pred functionp) (guard target-date))
       (let ((target-day-number (cl-typecase target-date
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
      (otherwise (error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string" comparator target-date)))))

(defun org-agenda-ng--habit-p ()
  (org-is-habit-p))
