;;; org-ql-view.el --- Agenda-like view based on org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql
;; Package-Requires: ((transient) (map "2.0"))

;;; Commentary:

;; This library is part of the package `org-ql'; it's not a standalone
;; library.  It displays strings in buffers similar to Org Agenda
;; buffers.

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
(require 'map)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'seq)
(require 'rx)
(require 'subr-x)

(require 'org-ql)

(require 'dash)
(require 's)
(require 'org-super-agenda)
(require 'ov)
(require 'ts)

;;;; Compatibility

(when (version< org-version "9.2")
  (defalias 'org-get-tags #'org-get-tags-at))

;;;; Faces

(defface org-ql-view-due-date
  '((t (:slant italic :weight bold)))
  "Face for due dates in `org-ql-view' views."
  :group 'org-ql)

;;;; Variables

(defvar org-ql-view-buffer-name-prefix "*Org QL View:"
  "Prefix for names of `org-ql-view' buffers.")

(defvar org-ql-view-buffer nil
  "Optionally set the target buffer for `org-ql-view' commands.
Includes `org-ql-search'.  Helpful when passing a buffer argument
down a chain of function calls would be awkward.")

(defvar org-ql-view-map
  (let ((map (copy-keymap org-agenda-mode-map)))
    (define-key map "r" #'org-ql-view-refresh)
    (define-key map "v" #'org-ql-view-dispatch)
    (define-key map (kbd "C-x C-s") #'org-ql-view-save)
    map)
  "Keymap for `org-ql-view', `org-ql-search', and `org-ql-views' views.
Based on `org-agenda-mode-map'.")

(defvar org-ql-view-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-ql-view-switch)
    (define-key map [mouse-1] #'org-ql-view-switch)
    (define-key map "c" #'org-ql-view-customize)
    map)
  "Keymap for `org-ql-view' view list buffer.")

;; For refreshing results buffers.
(defvar-local org-ql-view-buffers-files nil)
(defvar-local org-ql-view-query nil)
(defvar-local org-ql-view-sort nil)
(defvar-local org-ql-view-narrow nil)
(defvar-local org-ql-view-super-groups nil)
(defvar-local org-ql-view-title nil)

;;;; Customization

(defgroup org-ql-view nil
  "Options for `org-ql-view'."
  :group 'org-ql)

(defcustom org-ql-view-display-buffer-action nil
  "Action argument passed through `pop-to-buffer' to `display-buffer', which see."
  :type '(cons function alist))

(defcustom org-ql-view-list-side 'right
  "Which side to show the view list on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom org-ql-view-no-other-window nil
  "Whether `other-window' commands should cycle through `org-ql-views' sidebar.
See info node `(elisp)Cyclic Window Ordering'."
  :type 'boolean)

(defcustom org-ql-view-list-slot 0
  "Side-window slot for Org QL Views list buffer."
  :type 'integer)

(defcustom org-ql-view-sidebar-sort-views nil
  "Sort `org-ql-views' in `org-ql-view-sidebar'."
  :type 'boolean)

(defcustom org-ql-views
  (list (cons "Overview: Agenda-like"
              (list :buffers-files #'org-agenda-files
                    :query '(and (not (done))
                                 (or (habit)
                                     (deadline auto)
                                     (scheduled :to today)
                                     (ts-active :on today)))
                    :sort '(date priority todo)
                    :super-groups 'org-super-agenda-groups
                    :title "Agenda-like"))
        (cons "Overview: NEXT tasks"
              (list :buffers-files #'org-agenda-files
                    :query '(todo "NEXT")
                    :sort '(priority date)
                    :super-groups 'org-super-agenda-groups
                    :title "Overview: NEXT tasks"))
        (cons "Calendar: Today"
              (list :buffers-files #'org-agenda-files
                    :query '(ts-active :on today)
                    :title "Today"
                    :super-groups 'org-super-agenda-groups
                    :sort '(priority)))
        (cons "Calendar: This week"
              (lambda ()
                "Show items with an active timestamp during this calendar week."
                (interactive)
                (let* ((ts (ts-now))
                       (beg-of-week (->> ts
                                         (ts-adjust 'day (- (ts-dow (ts-now))))
                                         (ts-apply :hour 0 :minute 0 :second 0)))
                       (end-of-week (->> ts
                                         (ts-adjust 'day (- 6 (ts-dow (ts-now))))
                                         (ts-apply :hour 23 :minute 59 :second 59))))
                  (org-ql-search (org-agenda-files)
                    `(ts-active :from ,beg-of-week
                                :to ,end-of-week)
                    :title "This week"
                    :super-groups 'org-super-agenda-groups
                    :sort '(priority)))))
        (cons "Calendar: Next week"
              (lambda ()
                "Show items with an active timestamp during the next calendar week."
                (interactive)
                (let* ((ts (ts-adjust 'day 7 (ts-now)))
                       (beg-of-week (->> ts
                                         (ts-adjust 'day (- (ts-dow (ts-now))))
                                         (ts-apply :hour 0 :minute 0 :second 0)))
                       (end-of-week (->> ts
                                         (ts-adjust 'day (- 6 (ts-dow (ts-now))))
                                         (ts-apply :hour 23 :minute 59 :second 59))))
                  (org-ql-search (org-agenda-files)
                    `(ts-active :from ,beg-of-week
                                :to ,end-of-week)
                    :title "Next week"
                    :super-groups 'org-super-agenda-groups
                    :sort '(priority)))))
        (cons "Review: Recently timestamped" #'org-ql-view-recent-items)
        (cons (propertize "Review: Dangling tasks"
                          'help-echo "Tasks whose ancestor is done")
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (ancestors (done)))
                    :title (propertize "Review: Dangling tasks"
                                       'help-echo "Tasks whose ancestor is done")
                    :sort '(date priority todo)
                    :super-groups '((:auto-parent t))))
        (cons (propertize "Review: Stale tasks"
                          'help-echo "Tasks without a timestamp in the past 2 weeks")
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (not (ts :from -14)))
                    :title (propertize "Review: Stale tasks"
                                       'help-echo "Tasks without a timestamp in the past 2 weeks")
                    :sort '(date priority todo)
                    :super-groups '((:auto-parent t))))
        (cons (propertize "Review: Stuck projects"
                          'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (descendants (todo))
                                 (not (descendants (todo "NEXT"))))
                    :title (propertize "Review: Stuck projects"
                                       'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
                    :sort '(priority date)
                    :super-groups 'org-super-agenda-groups)))
  "Alist of `org-ql-view' commands."
  :type
  '(alist
    :key-type (string :tag "Name")
    :value-type
    (choice (function :tag "Function which calls `org-ql-search'")
            (plist :tag "Org QL Search"
                   :options (((const :tag "Buffers/files" :buffers-files)
                              (choice (function-item :tag "Org Agenda Files" org-agenda-files)
                                      (repeat :tag "Buffer or file names" string)
                                      (function :tag "Function which returns a list of buffers and/or files" list)))
                             ((const :tag "Query" :query) (sexp :tag "org-ql query sexp"))
                             ((const :tag "Search title" :title) string)
                             ((const :tag "Sort-by" :sort) (repeat
                                                            (choice (const date)
                                                                    (const deadline)
                                                                    (const scheduled)
                                                                    (const todo)
                                                                    (const priority)
                                                                    (const random)
                                                                    (function :tag "Custom comparator"))))
                             ((const :tag "Group-by" :super-groups)
                              (choice (variable-item :tag "Default org-super-agenda groups" org-super-agenda-groups)
                                      (sexp :tag "org-super-agenda grouping expression")
                                      (variable :tag "Variable holding org-super-agenda  grouping expression"))))))))

;;;; Commands

;;;###autoload
(defun org-ql-view (&optional name)
  "Choose and display the `org-ql-views' view NAME.
Interactively, prompt for NAME."
  (interactive (list (completing-read "View: " (mapcar #'car org-ql-views))))
  (let* ((view (alist-get name org-ql-views nil nil #'string=))
         (window (--find (string-prefix-p org-ql-view-buffer-name-prefix (buffer-name (window-buffer it)))
                         (window-list)))
         (org-ql-view-display-buffer-action (when (and window (not org-ql-view-display-buffer-action))
                                              (cons #'display-buffer-same-window nil))))
    (when window
      (select-window window))
    (cl-typecase view
      (function (call-interactively view))
      (list (-let* (((&plist :buffers-files :query :sort :narrow :super-groups :title) view)
                    (super-groups (cl-typecase super-groups
                                    (symbol (symbol-value super-groups))
                                    (list super-groups))))
              (org-ql-search buffers-files query
                :super-groups super-groups :narrow narrow :sort sort :title title
                :buffer org-ql-view-buffer))))))

;;;###autoload
(cl-defun org-ql-view-recent-items
    (&key num-days (type 'ts)
          (files (org-agenda-files))
          (groups '((:auto-parent t)
                    (:auto-todo t))))
  "Show items in FILES from last NUM-DAYS days with timestamps of TYPE.
TYPE may be `ts', `ts-active', `ts-inactive', `clocked', or
`closed'."
  (interactive (list :num-days (read-number "Days: ")
                     :type (->> '(ts ts-active ts-inactive clocked closed)
                                (completing-read "Timestamp type: ")
                                intern)))
  ;; It doesn't make much sense to use other date-based selectors to
  ;; look into the past, so to prevent confusion, we won't allow them.
  (-let* ((query (pcase-exhaustive type
                   ((or 'ts 'ts-active 'ts-inactive)
                    `(,type :from ,(- num-days) :to 0))
                   ((or 'clocked 'closed)
                    `(,type :from ,(- num-days) :to 0)))))
    (org-ql-search files query
      :title "Recent items"
      :sort '(date priority todo)
      :super-groups groups)))

;;;###autoload
(cl-defun org-ql-view-sidebar (&key (slot org-ql-view-list-slot))
  "Show `org-ql-view' view list sidebar."
  ;; TODO: Update sidebar when `org-ql-views' changes.
  (interactive)
  (select-window
   (or (get-buffer-window (org-ql-view--list-buffer))
       (display-buffer-in-side-window
        (org-ql-view--list-buffer)
        (list (cons 'side org-ql-view-list-side)
              (cons 'slot slot)
              (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                             (cons 'no-other-window org-ql-view-no-other-window))))))))

(defun org-ql-view-switch ()
  "Switch to view at point."
  (interactive)
  (let ((key (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (unless (string-empty-p key)
      (ov-clear :org-ql-view-selected)
      (ov (point-at-bol) (1+ (point-at-eol)) :org-ql-view-selected t
          'face '(:weight bold :inherit highlight))
      (org-ql-view key))))

(defun org-ql-view-refresh (&optional prompt)
  "Refresh current `org-ql-search' buffer.
If PROMPT is non-nil (interactively, with prefix), prompt to
update search arguments."
  (interactive "P")
  (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (old-pos (point))
         (defaults (list org-ql-view-buffers-files
                         org-ql-view-query
                         :sort org-ql-view-sort
                         :narrow org-ql-view-narrow
                         :super-groups org-ql-view-super-groups
                         :title org-ql-view-title))
         (org-ql-view-buffer (current-buffer)))
    (if prompt
        (call-interactively #'org-ql-search)
      (apply #'org-ql-search defaults))
    ;; Now in the results buffer.
    (rename-buffer (format "%s %s*"
                           org-ql-view-buffer-name-prefix
                           (or org-ql-view-title org-ql-view-query)))
    (goto-char (point-min))
    (or (when (search-forward current-line nil t)
          (beginning-of-line))
        (goto-char old-pos))
    (message "View refreshed")))

(defun org-ql-view-save ()
  "Save current `org-ql-search' buffer to `org-ql-views'."
  (interactive)
  (let* ((name (read-string "Save view as: " org-ql-view-title))
         (plist (list :buffers-files org-ql-view-buffers-files
                      :query org-ql-view-query
                      :sort org-ql-view-sort
                      :narrow org-ql-view-narrow
                      :super-groups org-ql-view-super-groups
                      :title name)))
    (when (or (not (map-elt org-ql-views name nil))
              (yes-or-no-p (format "Overwrite view \"%s\"?" name)))
      (setf (map-elt org-ql-views name nil #'equal) plist)
      (customize-set-variable 'org-ql-views org-ql-views)
      (customize-mark-to-save 'org-ql-views))))

(defun org-ql-view-delete ()
  "Delete current view (with confirmation)."
  (interactive)
  (when (yes-or-no-p (format "Delete view \"%s\"?" org-ql-view-title))
    (setf org-ql-views
          (--remove (equal (car it) org-ql-view-title)
                    org-ql-views))
    (customize-set-variable 'org-ql-views org-ql-views)
    (customize-mark-to-save 'org-ql-views)))

(defun org-ql-view-customize ()
  "Customize view at point in `org-ql-view-sidebar' buffer."
  (interactive)
  (let ((key (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (customize-option 'org-ql-views)
    (search-forward (concat "Name: " key))))

;;;; Functions

(defun org-ql-view--list-buffer ()
  "Return view list buffer."
  (with-current-buffer (get-buffer-create "*Org QL View List*")
    (use-local-map org-ql-view-list-map)
    (setf buffer-read-only t
          mode-line-format nil
          header-line-format (propertize " Org QL Views"
                                         'face 'header-line))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (->> org-ql-views
           (-map #'car)
           (-sort (if org-ql-view-sidebar-sort-views
                      #'string<
                    #'ignore))
           (s-join "\n")
           insert))
    (current-buffer)))

(cl-defun org-ql-view--display (&key (buffer org-ql-view-buffer) header string)
  "Display STRING in `org-ql-view' BUFFER.

BUFFER may be a buffer, or a string naming a buffer, which is
reused if it already exists.  `org-ql-view-buffer' is used by
default.

HEADER is a string displayed in the buffer's header line.

The following special variables, if non-nil, are set
buffer-locally to preserve their value in the buffer for
subsequent refreshing of the buffer: `org-ql-view-buffers-files',
`org-ql-view-query', `org-ql-view-sort', `org-ql-view-narrow',
`org-ql-view-super-groups', `org-ql-title.'"
  (declare (indent defun))
  (let* ((vars (list 'org-ql-view-buffers-files 'org-ql-view-query
                     'org-ql-view-sort 'org-ql-view-narrow
                     'org-ql-view-super-groups 'org-ql-view-title))
         ;; Save the values of variables which are set buffer-locally in the
         ;; results buffer, which we want to override and set buffer-locally again.
         (vals (cl-loop for symbol in vars
                        collect (cons symbol (symbol-value symbol))))
         (buffer (cl-etypecase buffer
                   (string (org-ql-view--buffer buffer))
                   (null (org-ql-view--buffer buffer))
                   (buffer buffer))))
    (with-current-buffer buffer
      (use-local-map org-ql-view-map)
      ;; Prepare buffer, saving data for refreshing.
      (cl-loop for symbol in vars
               do (progn
                    (kill-local-variable symbol)
                    (set (make-local-variable symbol) (alist-get symbol vals nil nil #'equal))))
      (setf header-line-format header)
      ;; Clear buffer, insert entries, etc.
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert string "\n")
        (pop-to-buffer (current-buffer) org-ql-view-display-buffer-action)
        (org-agenda-finalize)
        (goto-char (point-min))))))

(defun org-ql-view--header-line-format (buffers-files query &optional title)
  "Return `header-line-format' for BUFFERS-FILES and QUERY.
If TITLE, prepend it to the header."
  (let* ((title (if title
                    (concat (propertize "View:" 'face 'transient-argument)
                            title " ")
                  ""))
         (query-formatted (org-ql-view--format-query query))
         (query-width (length query-formatted))
         (query-propertized (propertize (org-ql-view--font-lock-string 'emacs-lisp-mode query-formatted)
                                        'help-echo query-formatted))
         (available-width (max 0 (- (window-width)
                                    (length "In: ")
                                    (length "Query: ")
                                    query-width 4)))
         (buffers-files-formatted (format "%S" buffers-files))
         (buffers-files-formatted (propertize (->> buffers-files-formatted
                                                   (org-ql-view--font-lock-string 'emacs-lisp-mode)
                                                   (s-truncate available-width))
                                              'help-echo buffers-files-formatted)))
    (concat title
            (propertize "Query:" 'face 'transient-argument)
            query-propertized "  "
            (propertize "In:" 'face 'transient-argument)
            buffers-files-formatted)))

(defun org-ql-view--format-query (query)
  "Return QUERY formatted as a string.
Makes QUERY more readable, e.g. timestamp objects are replaced
with human-readable strings."
  (cl-labels ((rec (form)
                   (cl-typecase form
                     (ts (ts-format form))
                     (cons (cons (rec (car form))
                                 (rec (cdr form))))
                     (otherwise form))))
    (format "%S" (rec query))))

(defun org-ql-view--font-lock-string (mode s)
  "Return string S font-locked according to MODE."
  ;; FIXME: Is this the proper way to do this?  It works, but I feel like there must be a built-in way...
  (with-temp-buffer
    (delay-mode-hooks
      (insert s)
      (funcall mode)
      (font-lock-ensure)
      (buffer-string))))

(defun org-ql-view--buffer (&optional name)
  "Return `org-ql-view' buffer, creating it if necessary.
If NAME is non-nil, return buffer by that name instead of using
default buffer."
  (with-current-buffer (get-buffer-create (or name (concat org-ql-view-buffer-name-prefix "*")))
    (unless (eq major-mode 'org-agenda-mode)
      (org-agenda-mode)
      (setf buffer-read-only t))
    (current-buffer)))

(defun org-ql-view--format-relative-date (difference)
  ;; MAYBE: Make this a `defsubst'.
  "Return relative date string for DIFFERENCE.
DIFFERENCE should be an integer number of days, positive for
dates in the past, and negative for dates in the future."
  (cond ((> difference 0)
         (format "%sd ago" difference))
        ((< difference 0)
         (format "in %sd" (* -1 difference)))
        (t "today")))

;;;; Transient

;; This section uses `transient' to allow the user to easily modify
;; and refresh views.

;; NOTE: I don't really know what I'm doing here.  Even though the
;; Transient manual is written very well, not everything is covered in
;; it, so I'm having to try to imitate examples from `magit-transient'.

(require 'transient)

(defclass org-ql-view--variable (transient-variable)
  ;; FIXME: We don't need :scope, but maybe a slot has to be defined.
  ((scope       :initarg :scope)))

(cl-defmethod transient-infix-set ((obj org-ql-view--variable) value)
  "Set Org QL View variable defined by OBJ to VALUE."
  (let ((variable (oref obj variable)))
    (oset obj value value)
    (set (make-local-variable (oref obj variable)) value)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

(define-transient-command org-ql-view-dispatch ()
  "Show Org QL View dispatcher."
  [["Edit"
    ("t" org-ql-view--transient-title)
    ("q" org-ql-view--transient-query)
    ("i" org-ql-view--transient-in)
    ("s" org-ql-view--transient-sort)
    ("g" org-ql-view--transient-super-groups)]]
  [["View"
    ("r" "Refresh" org-ql-view-refresh)
    ("v" "Select" org-ql-view)]
   [""
    ("C-s" "Save" org-ql-view-save)
    ("C-k" "Delete" org-ql-view-delete)]])

(defun org-ql-view--format-transient-key-value (key value)
  "Return KEY and VALUE formatted for display in Transient."
  ;; `window-width' minus 15 is about right.  I think there's no way
  ;; to determine it automatically, because we can't know which column
  ;; Transient is starting at.
  (let ((max-width (- (window-width) 15)))
    (format "%s: %s" (propertize key 'face 'transient-argument)
            (s-truncate max-width (format "%s" value)))))

(defun org-ql-view--format-transient-lisp-argument (key value)
  "Return KEY and VALUE (a Lisp object) formatted for display in Transient."
  ;; `window-width' minus 15 is about right.  I think there's no way
  ;; to determine it automatically, because we can't know which column
  ;; Transient is starting at.
  (s-truncate (- (window-width) 15)
              (concat (propertize key 'face 'transient-argument) ": "
                      (->> value
                           org-ql-view--format-query
                           (org-ql-view--font-lock-string 'emacs-lisp-mode)))))

(define-infix-command org-ql-view--transient-title ()
  ;; TODO: Add an asterisk or something when the view has been modified but not saved.
  :description (lambda () (org-ql-view--format-transient-key-value "Title" org-ql-view-title))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-title
  :prompt "Title: "
  :reader (lambda (prompt _initial-input history)
            ;; FIXME: Figure out how to integrate initial-input.
            (read-string prompt (when org-ql-view-title
                                  (format "%s" org-ql-view-title))
                         history)))

(define-infix-command org-ql-view--transient-query ()
  :description (lambda () (org-ql-view--format-transient-lisp-argument "Query" org-ql-view-query))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-query
  :prompt "Query: "
  :reader (lambda (prompt _initial-input history)
            ;; FIXME: Figure out how to integrate initial-input.
            (let ((query (read-string prompt (when org-ql-view-query
                                               (format "%S" org-ql-view-query))
                                      history)))
              (if (or (string-prefix-p "(" query)
                      (string-prefix-p "\"" query))
                  ;; Read sexp query.
                  (read query)
                ;; Parse non-sexp query into sexp query.
                (org-ql--plain-query query)))))

(define-infix-command org-ql-view--transient-in ()
  :description (lambda () (org-ql-view--format-transient-lisp-argument "In buffers/files" org-ql-view-buffers-files))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-buffers-files
  :prompt "Buffers/files: "
  :reader (lambda (_prompt _initial-input _history)
            ;; FIXME: Figure out how to integrate initial-input and history.
            (org-ql-view--complete-buffers-files)))

(define-infix-command org-ql-view--transient-super-groups ()
  :description (lambda ()
                 (org-ql-view--format-transient-lisp-argument "Group by" org-ql-view-super-groups))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-super-groups
  :prompt "Group by: "
  :reader (lambda (_prompt _initial-input _history)
            ;; FIXME: Figure out how to integrate initial-input and history.
            (org-ql-view--complete-super-groups)))

(define-infix-command org-ql-view--transient-sort ()
  :description
  (lambda ()
    (org-ql-view--format-transient-lisp-argument "Sort by" (or org-ql-view-sort 'buffer-order)))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-sort
  :prompt "Sort: "
  :reader (lambda (_prompt _initial-input _history)
            ;; FIXME: Figure out how to integrate initial-input and history.
            (org-ql-view--complete-sort)))

;;;; Faces/properties

(defun org-ql-view--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not element)
      ""
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
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           ;; TODO: --add-faces is used to add the :relative-due-date property, but that fact is
           ;; hidden by doing it through --add-faces (which calls --add-scheduled-face and
           ;; --add-deadline-face), and doing it in this form that gets the title hides it even more.
           ;; Adding the relative due date property should probably be done explicitly and separately
           ;; (which would also make it easier to do it independently of faces, etc).
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                   (org-ql-view--add-todo-face it)))
           ;; FIXME: Figure out whether I should use `org-agenda-use-tag-inheritance' or `org-use-tag-inheritance', etc.
           (tag-list (if org-use-tag-inheritance
                         ;; FIXME: Note that tag inheritance cannot be used here unless markers are
                         ;; added, otherwise we can't go to the item's buffer to look for inherited
                         ;; tags.  (Or does `org-element-headline-parser' parse inherited tags too?  I
                         ;; forget...)
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               ;; I wish `org-get-tags' used the correct buffer automatically.
                               (org-get-tags marker (not org-use-tag-inheritance)))
                           ;; No marker found
                           (warn "No marker found for item: %s" title)
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                                      (char-to-string)
                                      (format "[#%s]")
                                      (org-ql-view--add-priority-face)))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
           ;; FIXME: Use proper prefix
           (concat "  " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(defun org-ql-view--add-faces (element)
  "Return ELEMENT with deadline and scheduled faces added."
  (->> element
       (org-ql-view--add-scheduled-face)
       (org-ql-view--add-deadline-face)))

(defun org-ql-view--add-priority-face (string)
  "Return STRING with priority face added."
  (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
    (let ((face (org-get-priority-face (string-to-char (match-string 2 string)))))
      (org-add-props string nil 'face face 'font-lock-fontified t))))

(defun org-ql-view--add-scheduled-face (element)
  "Add faces to ELEMENT's title for its scheduled status."
  ;; NOTE: Also adding prefix
  (if-let ((scheduled-date (org-element-property :scheduled element)))
      (let* ((todo-keyword (org-element-property :todo-keyword element))
             (today-day-number (org-today))
             ;; (current-day-number
             ;; NOTE: Not currently used, but if we ever implement a more "traditional" agenda that
             ;; shows perspective of multiple days at once, we'll need this, so I'll leave it for now.
             ;;  ;; FIXME: This is supposed to be the, shall we say,
             ;;  ;; pretend, or perspective, day number that this pass
             ;;  ;; through the agenda is being made for.  We need to
             ;;  ;; either set this in the calling function, set it here,
             ;;  ;; or accomplish this in a different way.  See
             ;;  ;; `org-agenda-get-scheduled' and where `date' is set in
             ;;  ;; `org-agenda-list'.
             ;;  today-day-number)
             (scheduled-day-number (org-time-string-to-absolute
                                    (org-element-timestamp-interpreter scheduled-date 'ignore)))
             (difference-days (- today-day-number scheduled-day-number))
             (relative-due-date (org-add-props (org-ql-view--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value scheduled-date)))
             ;; FIXME: Unused for now:
             ;; (show-all (or (eq org-agenda-repeating-timestamp-show-all t)
             ;;               (member todo-keyword org-agenda-repeating-timestamp-show-all)))
             ;; FIXME: Unused for now: (sexp-p (string-prefix-p "%%" raw-value))
             ;; FIXME: Unused for now: (raw-value (org-element-property :raw-value scheduled-date))
             ;; FIXME: I don't remember what `repeat-day-number' was for, but we aren't using it.
             ;; But I'll leave it here for now.
             ;; (repeat-day-number (cond (sexp-p (org-time-string-to-absolute scheduled-date))
             ;;                          ((< today-day-number scheduled-day-number) scheduled-day-number)
             ;;                          (t (org-time-string-to-absolute
             ;;                              raw-value
             ;;                              (if show-all
             ;;                                  current-day-number
             ;;                                today-day-number)
             ;;                              'future
             ;;                              ;; FIXME: I don't like
             ;;                              ;; calling `current-buffer'
             ;;                              ;; here.  If the element has
             ;;                              ;; a marker, we should use
             ;;                              ;; that.
             ;;                              (current-buffer)
             ;;                              (org-element-property :begin element)))))
             (face (cond ((member todo-keyword org-done-keywords) 'org-agenda-done)
                         ((= today-day-number scheduled-day-number) 'org-scheduled-today)
                         ((> today-day-number scheduled-day-number) 'org-scheduled-previously)
                         (t 'org-scheduled)))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (cadr element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date))))
        (list (car element)
              properties))
    ;; Not scheduled
    element))

(defun org-ql-view--add-deadline-face (element)
  "Add faces to ELEMENT's title for its deadline status.
Also store relative due date as string in `:relative-due-date'
property."
  ;; FIXME: In my config, doesn't apply orange for approaching deadline the same way the Org Agenda does.
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
             (difference-days (- today-day-number deadline-day-number))
             (relative-due-date (org-add-props (org-ql-view--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value deadline-date)))
             ;; FIXME: Unused for now: (todo-keyword (org-element-property :todo-keyword element))
             ;; FIXME: Unused for now: (done-p (member todo-keyword org-done-keywords))
             ;; FIXME: Unused for now: (today-p (= today-day-number deadline-day-number))
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

(defun org-ql-view--add-todo-face (keyword)
  "Return KEYWORD with TODO face added."
  (when-let* ((org-done-keywords org-done-keywords-for-agenda)
              (face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;;; Completion

(defun org-ql-view--complete-buffers-files ()
  "Return value for `org-ql-view-buffers-files' using completion."
  (if (and org-ql-view-buffers-files
           (bufferp org-ql-view-buffers-files))
      ;; Buffers can't be input by name, so if the default value is a buffer, just use it.
      ;; TODO: Find a way to fix this.
      org-ql-view-buffers-files
    (pcase-exhaustive (completing-read "Buffers/Files: "
                                       (list 'buffer 'agenda 'directory 'all)
                                       nil nil (when org-ql-view-buffers-files
                                                 (let ((print-length nil))
                                                   (prin1-to-string (cons 'list org-ql-view-buffers-files)))))
      ((or "" "buffer") (current-buffer))
      ("agenda" (org-agenda-files))
      ("all" (--select (equal (buffer-local-value 'major-mode it) 'org-mode)
                       (buffer-list)))
      ("directory" (org-ql-search-directories-files))
      ((and form (guard (rx bos "("))) (-flatten (eval (read form))))
      (else (s-split (rx (1+ space)) else)))))

(defun org-ql-view--complete-super-groups ()
  "Return value for `org-ql-view-super-groups' using completion."
  (when (bound-and-true-p org-super-agenda-auto-selector-keywords)
    (let ((keywords (cl-loop for type in org-super-agenda-auto-selector-keywords
                             collect (substring (symbol-name type) 6))))
      (pcase (completing-read "Group by: "
                              (append (list "Don't group"
                                            "Global super-groups")
                                      keywords)
                              nil nil (when org-ql-view-super-groups
                                        (format "%S" org-ql-view-super-groups)))
        ("Global super-groups" org-super-agenda-groups)
        ((or "" "Don't group") nil)
        ((and keyword (guard (member keyword keywords)))
         (list (list (intern (concat ":auto-" keyword)))))
        (else (read else))))))

(defun org-ql-view--complete-sort ()
  "Return value for `org-ql-view-sort' using completion."
  (let ((input (->> (completing-read-multiple "Sort by: "
                                              (list "buffer-order"
                                                    "date"
                                                    "deadline"
                                                    "priority"
                                                    "scheduled"
                                                    "todo")
                                              nil nil (when org-ql-view-sort
                                                        (prin1-to-string org-ql-view-sort)))
                    (--remove (equal "buffer-order" it)))))
    (pcase input
      ('nil nil)
      ((and (pred listp) sort)
       ;; Multiple sorters.
       (mapcar #'intern sort))
      (sort ;; One sorter.
       (intern sort)))))

;;;; Footer

(provide 'org-ql-view)

;;; org-ql-view.el ends here
