;; -*- lexical-binding: t; -*-


;;; Commentary:

;; This is just a proof-of-concept for how the agenda code might be
;; written in a more functional way, to avoid making multiple passes
;; through each file when building a multi-day agenda.

;; It also functions as a kind of "query language" for Org buffers.

;;;; Principles

;;;;; Try to imitate traditional Org agenda code's results and methods

;; The traditional agenda code is complicated, but well-optimized.  We should imitate it where
;; possible.  We should also attempt to return the same results as the traditional agenda code
;; (ultimately, that is; but whether we reach that level of complexity depends on, e.g. performance
;; achieved, whether it looks like a feasible alternative, etc).  At the least, we should return
;; results in the same format, so they can be used by other code that uses agenda output
;; (e.g. org-agenda-finalize-entries, org-agenda-finalize, org-super-agenda, etc).  Of course, this
;; goal does not necessarily apply to the "query language"-like features, and ideally the
;; agenda-like features should build upon those.

;;;;; Preserve the call to =org-agenda-finalize-entries=

;; We want to preserve the final call to org-agenda-finalize-entries to preserve compatibility
;; with other packages and functions.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'dash)
(require 'cl-lib)
(require 'seq)

(require 'org-ql)

;;;; Macros

;; FIXME: DRY these two macros.

(cl-defmacro org-agenda-ng (&rest args)
  "Display an agenda-like buffer of entries in FILES that match PRED.

FILES-OR-PRED is a sexp that is evaluated to get the list of
files to scan.

PRED is a predicate sexp which is evaluated on each entry to test
whether it matches.  This should be a form used by `org-ql'.

The predicate may be passed as FILES-OR-PRED and PRED may be left
nil, in which case the list of files will automatically be set to
the value of calling `org-agenda-files'.

SORT is a list of sorting keys: `deadline', `scheduled',
`date' (matching either `deadline' or `scheduled'), `priority',
or `todo'.

NARROW, when non-nil, means to respect narrowing in buffers.
When nil, buffers are widened before being searched."
  (declare (indent defun)
           (advertised-calling-convention '(files-or-pred &optional pred &key sort narrow) nil))
  (cl-macrolet ((set-keyword-args (args)
                                  `(setq sort (plist-get ,args :sort)
                                         narrow (plist-get ,args :narrow))))
    (let ((files '(org-agenda-files))
          pred sort narrow)
      ;; Parse args manually (so we can leave FILES nil for a default argument).
      (pcase args
        (`(,arg-files ,arg-pred . ,(and rest (guard (keywordp (car rest)))))
         (setq files arg-files
               pred arg-pred)
         (set-keyword-args rest))
        (`(,arg-pred . ,(and rest (guard (keywordp (car rest)))))
         (setq pred arg-pred)
         (set-keyword-args rest)))
      ;; Call --agenda
      `(org-agenda-ng--agenda ,files
         (byte-compile (lambda ()
                         (cl-symbol-macrolet ((= #'=) (< #'<) (> #'>) (<= #'<=) (>= #'>=))
                           ,pred)))
         :sort ',sort))))

;;;; Functions

;; TODO: Move the action-fn down into --filter-buffer, so users can avoid calling the
;; headline-parser when they don't need it.

(cl-defun org-agenda-ng--agenda (files pred &key action-fn sort)
  ;; `org-ql--query' uses `org-element-headline-parser' by default, which we then map
  ;; `org-agenda-ng--format-element' across to get formatted, propertized strings for the agenda.
  ;; NOTE: `org-element-headline-parser' should remain the low-level action function, because the
  ;; sorting functions work on Org elements (so e.g. if we put `org-agenda-ng--format-element' as
  ;; the low-level action function, sorting would do nothing, because the sorting functions would
  ;; not find any data to sort on).  IOW we have to call `org-agenda-ng--format-element' here, not
  ;; pass it to lower functions.
  (declare (indent defun))
  (let* ((entries (mapcar #'org-agenda-ng--format-element
                          (org-ql--query files
                            pred
                            :sort sort)))
         (result-string (org-agenda-finalize-entries entries 'agenda))
         (target-buffer (get-buffer-create "test-agenda-ng")))
    (with-current-buffer target-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert result-string)
      (read-only-mode 1)
      (pop-to-buffer (current-buffer))
      (org-agenda-finalize))))

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
         (properties (--> (cadr element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (cadr element) properties)
    element))

(defun org-agenda-ng--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with its text-properties set according to its property list.
Its property list should be the second item in the list, as returned by `org-element-parse-buffer'."
  (let* ((properties (cadr element))
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
         ;; TODO: --add-faces is used to add the :relative-due-date property, but that fact is
         ;; hidden by doing it through --add-faces (which calls --add-scheduled-face and
         ;; --add-deadline-face), and doing it in this form that gets the title hides it even more.
         ;; Adding the relative due date property should probably be done explicitly and separately
         ;; (which would also make it easier to do it independently of faces, etc).
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
                           (with-current-buffer (marker-buffer marker)
                             ;; I wish `org-get-tags-at' used the correct buffer automatically.
                             (org-get-tags-at marker (not org-use-tag-inheritance)))
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
    (let ((face (org-get-priority-face (string-to-char (match-string 2 string)))))
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
             (relative-due-date (org-add-props (org-agenda-ng--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value scheduled-date)))
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
             (properties (--> (cadr element)
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
             (relative-due-date (org-add-props (org-agenda-ng--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value deadline-date)))
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
             (properties (--> (cadr element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date))))
        (list (car element)
              properties))
    ;; No deadline
    element))

(defun org-agenda-ng--add-todo-face (keyword)
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;; Footer

(provide 'org-agenda-ng)

;;; org-agenda-ng.el ends here
