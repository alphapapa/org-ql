;;; org-ql-view.el --- Agenda-like view based on org-ql  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql

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
(require 'crm)
(require 'map)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'seq)
(require 'rx)
(require 'subr-x)

(require 'org-ql)

(declare-function org-ql-search "org-ql-search" t)
(declare-function org-ql-search--org-link-store-props "org-ql-search" t)
(declare-function org-ql--normalize-query "org-ql" t t)

(require 'dash)
(require 's)
(require 'org-super-agenda)
(require 'ov)
(require 'ts)

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
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-agenda-mode-map)
    (define-key map "g" #'org-ql-view-refresh)
    (define-key map "r" #'org-ql-view-refresh)
    (define-key map "q" #'bury-buffer)
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
                    :sort '(todo priority date)
                    :super-groups 'org-super-agenda-groups
                    :title "Agenda-like"))
        (cons "Overview: NEXT tasks"
              (list :buffers-files #'org-agenda-files
                    :query '(todo "NEXT")
                    :sort '(date priority)
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
                    :sort '(todo priority date)
                    :super-groups '((:auto-parent t))))
        (cons (propertize "Review: Stale tasks"
                          'help-echo "Tasks without a timestamp in the past 2 weeks")
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (not (ts :from -14)))
                    :title (propertize "Review: Stale tasks"
                                       'help-echo "Tasks without a timestamp in the past 2 weeks")
                    :sort '(todo priority date)
                    :super-groups '((:auto-parent t))))
        (cons (propertize "Review: Stuck projects"
                          'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (descendants (todo))
                                 (not (descendants (todo "NEXT"))))
                    :title (propertize "Review: Stuck projects"
                                       'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
                    :sort '(date priority)
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
                                                                    (const reverse)
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
      :sort '(todo priority date)
      :super-groups groups)))

;;;###autoload
(cl-defun org-ql-view-sidebar (&key (slot org-ql-view-list-slot))
  "Show `org-ql-view' view list sidebar.
SLOT is passed to `display-buffer-in-side-window', which see."
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
  (let ((key (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-empty-p key)
      (ov-clear :org-ql-view-selected)
      (ov (pos-bol) (1+ (pos-eol)) :org-ql-view-selected t
          'face '(:weight bold :inherit highlight))
      (org-ql-view key))))

(defun org-ql-view-refresh (&optional prompt)
  "Refresh current `org-ql-search' buffer.
If PROMPT is non-nil (interactively, with prefix), prompt to
update search arguments."
  (interactive "P")
  (unless org-ql-view-buffers-files
    (user-error "Not an Org QL View buffer"))
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
                           (or org-ql-view-title org-ql-view-query))
                   ;; Avoid any errors in case a buffer by this name already exists.
                   'unique)
    (goto-char (point-min))
    (or (when (search-forward current-line nil t)
          (beginning-of-line))
        (goto-char old-pos))
    (message "View refreshed")))

(defun org-ql-view-save ()
  "Save current `org-ql-search' buffer to `org-ql-views'."
  (interactive)
  (let* ((name (read-string "Save view as: " org-ql-view-title))
         ;; Bind `org-ql-view-title' to the name that was read, in case
         ;; it's different, which `org-ql-view--plist' will pick up.
         (org-ql-view-title name)
         (plist (org-ql-view--plist (current-buffer))))
    (when (or (not (map-elt org-ql-views name nil))
              (yes-or-no-p (format "Overwrite view \"%s\"?" name)))
      (setf (map-elt org-ql-views name nil #'equal) plist)
      (customize-set-variable 'org-ql-views org-ql-views)
      (customize-mark-to-save 'org-ql-views)
      (custom-save-all))))

(defun org-ql-view-delete ()
  "Delete current view (with confirmation)."
  (interactive)
  (when (yes-or-no-p (format "Delete view \"%s\"?" org-ql-view-title))
    (setf org-ql-views
          (--remove (equal (car it) org-ql-view-title)
                    org-ql-views))
    (customize-set-variable 'org-ql-views org-ql-views)
    (customize-mark-to-save 'org-ql-views)
    (custom-save-all)))

(defun org-ql-view-customize ()
  "Customize view at point in `org-ql-view-sidebar' buffer."
  (interactive)
  (let ((key (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (customize-option 'org-ql-views)
    (search-forward (concat "Name: " key))))

;;;; Functions

(defun org-ql-view--plist (buffer)
  "Return plist describing Org QL View in BUFFER."
  (with-current-buffer buffer
    (list :buffers-files org-ql-view-buffers-files
          :query org-ql-view-query
          :sort org-ql-view-sort
          :narrow org-ql-view-narrow
          :super-groups org-ql-view-super-groups
          :title org-ql-view-title)))

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

(defvar bookmark-make-record-function)

(cl-defun org-ql-view--display (&key (buffer org-ql-view-buffer) header strings)
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
      (setq-local bookmark-make-record-function #'org-ql-view-bookmark-make-record)
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
        (dolist (string strings)
          (insert string "\n"))
        (insert "\n")
        (pop-to-buffer (current-buffer) org-ql-view-display-buffer-action)
        (org-agenda-finalize)
        (goto-char (point-min))))))

(cl-defun org-ql-view--header-line-format (&key buffers-files query title)
  "Return `header-line-format' for BUFFERS-FILES and QUERY.
If TITLE, prepend it to the header."
  (let* ((title (if title
                    (concat (propertize "View:" 'face 'transient-argument)
                            title " ")
                  ""))
         (query-formatted (when query
                            (org-ql-view--format-query query)))
         (query-width (when query-formatted
                        (length query-formatted)))
         (query-propertized (when query-formatted
                              (propertize (org-ql-view--font-lock-string 'emacs-lisp-mode query-formatted)
                                          'help-echo query-formatted)))
         (available-width (max 0 (- (window-width) (length "In: ")
                                    (length "Query: ") (or query-width 0)
                                    4)))
         (buffers-files-formatted (when buffers-files
                                    (format "%s" (org-ql-view--contract-buffers-files buffers-files))))
         (buffers-files-formatted (when buffers-files-formatted
                                    (propertize (->> buffers-files-formatted
                                                     (org-ql-view--font-lock-string 'emacs-lisp-mode)
                                                     (s-truncate available-width))
                                                'help-echo buffers-files-formatted))))
    (concat title
            (when query (propertize "Query:" 'face 'transient-argument))
            (when query query-propertized)
            (when query "  ")
            (when buffers-files
              (propertize "In:" 'face 'transient-argument))
            (when buffers-files-formatted
              buffers-files-formatted))))

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

;;;; Bookmarks

;; Support for Emacs bookmarks.

(require 'bookmark)

(defun org-ql-view-bookmark-make-record ()
  "Return a bookmark record for the current Org QL View buffer."
  (cl-labels ((file-nameize (b-f)
                (abbreviate-file-name
                 (cl-typecase b-f
                   (string b-f)
                   (buffer (or (buffer-file-name b-f)
                               (when (buffer-base-buffer b-f)
                                 (buffer-file-name (buffer-base-buffer b-f)))))
                   (t (user-error "Only file-backed buffers can be bookmarked by Org QL View: %s" b-f))))))
    (-let* ((plist (org-ql-view--plist (current-buffer)))
            ((&plist :buffers-files) plist))
      ;; Replace buffers with their filenames, and signal error if any are not file-backed.
      (setf plist (plist-put plist :buffers-files
                             (cl-etypecase buffers-files
                               (symbol buffers-files)
                               (string buffers-files)
                               (buffer (file-nameize buffers-files))
                               (list (mapcar #'file-nameize buffers-files)))))
      (list (concat "Org QL View: " org-ql-view-title)
            (cons 'org-ql-view-plist plist)
            (cons 'handler #'org-ql-view-bookmark-handler)
            (cons 'position (point))))))

;;;###autoload
(defun org-ql-view-bookmark-handler (bookmark)
  "Show Org QL View BOOKMARK in current buffer."
  ;; FIXME: `pcase-let*' is easier to use to destructure this, but if I use
  ;; that, I want to use map 2.1 for the extra convenience, but I can't force
  ;; that to be installed into the makem.sh sandbox, so I just use `-let*' here.
  (-let* ((record (bookmark-get-bookmark-record bookmark))
          ((&alist 'org-ql-view-plist) record)
          ((&plist :buffers-files :query :super-groups :narrow :sort :title)
           org-ql-view-plist)
          (super-groups (cl-etypecase super-groups
                          (symbol (symbol-value super-groups))
                          (list super-groups))))
    (org-ql-search buffers-files query
      :super-groups super-groups :narrow narrow :sort sort :title title)
    ;; HACK: `bookmark--jump-via' expects that, when the handler returns, the current buffer
    ;; (not merely the selected window's buffer) is the one to be displayed.  However,
    ;; `org-ql-view--display' uses `with-current-buffer', which resets the current buffer to
    ;; the one that was active when the bookmark was jumped to.  So we set the buffer before
    ;; returning.  It might be better to refactor `org-ql-view--display' into a function that
    ;; returns a buffer which can be displayed by the calling function.  But that would be
    ;; more complicated and might introduce bugs elsewhere, so we'll just do this for now.
    (set-buffer (window-buffer (selected-window)))))

;;;; Links

;; This section implements support for Org links to Org QL searches.  Since the
;; links are stored from and opened in `org-ql-view' buffers, this section resides
;; in this file.  However, since the links are to `org-ql-search' searches rather
;; than `org-ql-view' saved views, the link type is "org-ql-search".

(org-link-set-parameters "org-ql-search"
                         :follow #'org-ql-view--link-follow
                         :store #'org-ql-view--link-store)

;; We require the URL libraries in the functions to hopefully avoid
;; loading them until they're needed.

(eval-when-compile
  (require 'url-parse)
  (require 'url-util))

(defun org-ql-view--link-follow (path &optional _ignored)
  "Open Org QL query for current buffer at PATH.
PATH should be the part of an \"org-ql-search:\" URL after the
protocol.  See, e.g. `org-ql-view--link-store'.

The optional, second argument is temporarily _IGNORED for
purposes of compatibility with changes in Org 9.4."
  (require 'url-parse)
  (require 'url-util)
  (declare-function url-path-and-query "url-parse")
  (when (version<= "9.3" (org-version))
    ;; Org 9.3+ makes a backward-incompatible change to link escaping.
    ;; I don't think it would be a good idea to try to guess whether
    ;; the string received by this function was made with or without
    ;; that change, so we'll just test the current version of Org.
    ;; Any links created with older Org versions and then opened with
    ;; newer ones will have to be recreated.
    (setf path (url-unhex-string path)))
  (pcase-let* ((`(,query . ,params) (url-path-and-query
                                     (url-parse-make-urlobj "org-ql-search" nil nil nil nil
                                                            path)))
               (query (url-unhex-string query))
               (params (when params (url-parse-query-string params)))
               ;; `url-parse-query-string' returns "improper" alists, which makes this awkward.
               (sort (when-let* ((stored-string (alist-get "sort" params nil nil #'string=))
                                 (read-value (read stored-string)))
                       ;; Ensure the value is either a symbol or list of symbols (which excludes lambdas).
                       (unless (or (symbolp read-value) (cl-every #'symbolp read-value))
                         (error "CAUTION: Link not opened because unsafe sort parameter detected: %s"
                                read-value))
                       read-value))
               (org-super-agenda-allow-unsafe-groups nil) ; Disallow unsafe group selectors.
               (groups (--when-let (alist-get "super-groups" params nil nil #'string=)
                         (read it)))
               (title (--when-let (alist-get "title" params nil nil #'string=)
                        (read it)))
               (buffers-files (--if-let (alist-get "buffers-files" params nil nil #'string=)
                                  (org-ql-view--expand-buffers-files (read it))
                                (current-buffer))))
    (unless (or (bufferp buffers-files)
                (stringp buffers-files)
                (cl-every #'stringp buffers-files))
      (error "CAUTION: Link not opened because unsafe buffers-files parameter detected: %s" buffers-files))
    (when (or (listp query)
              (string-match (rx bol (0+ space) "(") query))
      ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
      (org-ql--ask-unsafe-query query))
    (org-ql-search buffers-files query
      :sort sort
      :super-groups groups
      :title title)))

(defun org-ql-view--link-store ()
  "Store a link to the current Org QL view.
When opened, the link searches the buffer it's opened from."
  (require 'url-parse)
  (require 'url-util)
  (when org-ql-view-query
    ;; Only Org QL View buffers should have `org-ql-view-query' set.
    (cl-labels ((prompt-for (buffers-files)
                  (pcase-exhaustive
                      (completing-read (format "Make link that searches: ")
                                       '("file link is in" "files currently searched")
                                       nil t nil nil "file link is in")
                    ("file link is in" nil)
                    ("files currently searched" buffers-files)))
                (strings-or-file-buffers-p (thing)
                  (cl-etypecase thing
                    (list (cl-every #'strings-or-file-buffers-p thing))
                    (string thing)
                    (buffer (or (buffer-file-name thing)
                                ;; TODO: Should indirect buffers be allowed?  Maybe not, since their
                                ;; narrowing isn't preserved.  On the other hand, it's possible to
                                ;; accidentally make a search view for an indirect buffer that's
                                ;; since been widened, and forcing the user to manually change that
                                ;; would be awkward, and trying to communicate the problem would be
                                ;; difficult, so maybe it's okay to allow it.
                                (when (buffer-base-buffer thing)
                                  (buffer-file-name (buffer-base-buffer thing))))))))
      (unless (strings-or-file-buffers-p org-ql-view-buffers-files)
        (user-error "%s" "Views that search non-file-backed buffers can't be linked to"))
      (let* ((query-string (--if-let (org-ql--query-sexp-to-string org-ql-view-query)
                               it (org-ql-view--format-query org-ql-view-query)))
             (buffers-files (prompt-for (org-ql-view--contract-buffers-files org-ql-view-buffers-files)))
             (params (list (when buffers-files
                             (list "buffers-files" (prin1-to-string buffers-files)))
                           (when org-ql-view-super-groups
                             (list "super-groups" (prin1-to-string org-ql-view-super-groups)))
                           (when org-ql-view-sort
                             (list "sort" (prin1-to-string org-ql-view-sort)))
                           (when org-ql-view-title
                             (list "title" (prin1-to-string org-ql-view-title)))))
             (filename (concat (url-hexify-string query-string)
                               "?" (url-build-query-string (delete nil params))))
             (url (url-recreate-url (url-parse-make-urlobj "org-ql-search" nil nil nil nil
                                                           filename))))
        (org-ql-search--org-link-store-props
         :type "org-ql-search"
         :link url
         :description (concat "org-ql-search: " org-ql-view-title))))
    t))

;;;; Transient

;; This section uses `transient' to allow the user to easily modify
;; and refresh views.

;; NOTE: I don't really know what I'm doing here.  Even though the
;; Transient manual is written very well, not everything is covered in
;; it, so I'm having to try to imitate examples from `magit-transient'.

(require 'eieio-core)

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

(transient-define-prefix org-ql-view-dispatch ()
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

(transient-define-infix org-ql-view--transient-title ()
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

(transient-define-infix org-ql-view--transient-query ()
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
                (org-ql--query-string-to-sexp query)))))

(transient-define-infix org-ql-view--transient-in ()
  :description (lambda () (org-ql-view--format-transient-lisp-argument "In buffers/files" org-ql-view-buffers-files))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-buffers-files
  :prompt "Buffers/files: "
  :reader (lambda (_prompt _initial-input _history)
            ;; This doesn't use the Transient `initial-input'
            ;; argument, but it gives the same result.
            (org-ql-view--complete-buffers-files)))

(transient-define-infix org-ql-view--transient-super-groups ()
  :description (lambda ()
                 (org-ql-view--format-transient-lisp-argument "Group by" org-ql-view-super-groups))
  :class 'org-ql-view--variable
  :argument ""
  :variable 'org-ql-view-super-groups
  :prompt "Group by: "
  :reader (lambda (_prompt _initial-input _history)
            ;; FIXME: Figure out how to integrate initial-input and history.
            (org-ql-view--complete-super-groups)))

(transient-define-infix org-ql-view--transient-sort ()
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

(defalias 'org-ql-view--resolve-element-properties
  ;; It would be preferable to define this as an inline function, but
  ;; that would mean that users would have to recompile org-ql when
  ;; upgrading to Org 9.7 or else get weird errors.
  ;; TODO(someday): Define `org-ql-view--resolve-element-properties' as inline.
  (if (version<= "9.7" org-version)
      (lambda (node)
        "Resolve NODE's properties using `org-element-properties-resolve'."
        ;; Silence warnings about `org-element-properties-resolve'
        ;; being unresolved on earlier Org versions.
        (with-no-warnings
          (org-element-properties-resolve node 'force-undefer)))
    #'identity))

(defun org-ql-view--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not element)
      ""
    (setf element (org-ql-view--resolve-element-properties element))
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
                       (org-element-property :raw-value it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                           (org-ql-view--add-todo-face it)))
           (tag-list (if org-use-tag-inheritance
                         ;; MAYBE: Use our own variable instead of `org-use-tag-inheritance'.
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               (org-with-wide-buffer
                                (goto-char marker)
                                (cl-loop for type in (org-ql--tags-at marker)
                                         unless (or (eq 'org-ql-nil type)
                                                    (not type))
                                         append type)))
                           ;; No marker found
                           (display-warning 'org-ql (format "No marker found for item: %s" title))
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           (category (or (org-element-property :CATEGORY element)
                         (when-let ((marker (or (org-element-property :org-hd-marker element)
                                                (org-element-property :org-marker element))))
                           (org-with-point-at marker
                             (or (org-get-category)
                                 (when buffer-file-name
			           (file-name-sans-extension
			            (file-name-nondirectory buffer-file-name))))))
                         ""))
           (priority-string (-some->> (org-element-property :priority element)
                              (char-to-string)
                              (format "[#%s]")
                              (org-ql-view--add-priority-face)))
           (habit-property (org-with-point-at (or (org-element-property :org-hd-marker element)
                                                  (org-element-property :org-marker element))
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
             'org-category category
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
  "Return KEYWORD with Org to-do face added."
  (let* ((org-done-keywords org-done-keywords-for-agenda)
         (face (org-get-todo-face keyword)))
    (when face
      (add-text-properties 0 (length keyword) (list 'face face) keyword))
    keyword))

;;;;; Completion

;; These functions are somewhat regrettable because of the need to keep them
;; in sync, but it seems worth it to provide users with the flexibility.

(declare-function org-ql-search-directories-files "org-ql-search" t)

(defun org-ql-view--contract-buffers-files (buffers-files)
  "Return BUFFERS-FILES in its \"contracted\" form.
The contracted form is \"org-agenda-files\" if BUFFERS-FILES
matches the value of `org-agenda-files' (either the function or
the variable), \"org-directory\" if it matches the value of
`org-ql-search-directories-files', or \"buffer\" if it is the
current buffer.  Otherwise BUFFERS-FILES is returned unchanged."
  ;; Used in `org-ql-view--complete-buffers-files' and
  ;; `org-ql-view--header-line-format'.
  (cl-labels ((expand-files (list)
                (--map (cl-typecase it
                         (string (expand-file-name it))
                         (otherwise it))
                       list)))
    ;; TODO: Test this more exhaustively.
    (pcase buffers-files
      ((pred listp)
       (pcase (expand-files buffers-files)
         ((pred (seq-set-equal-p (mapcar #'expand-file-name (org-agenda-files))))
          "org-agenda-files")
         ((and (guard (file-exists-p org-directory))
               (pred (seq-set-equal-p (org-ql-search-directories-files
                                       :directories (list org-directory)))))
          "org-directory")
         (_ buffers-files)))
      ((pred (equal (current-buffer)))
       "buffer")
      ((or 'org-agenda-files '(function org-agenda-files))
       "org-agenda-files")
      ((and (pred bufferp) (guard (buffer-file-name buffers-files)))
       (buffer-file-name buffers-files))
      (_ buffers-files))))

(defun org-ql-view--complete-buffers-files ()
  "Return value for `org-ql-view-buffers-files' using completion."
  (cl-labels ((initial-input ()
                (when org-ql-view-buffers-files
                  (org-ql-view--contract-buffers-files
                   org-ql-view-buffers-files))))
    (if (and org-ql-view-buffers-files
             (bufferp org-ql-view-buffers-files))
        ;; Buffers can't be input by name, so if the default value is a buffer, just use it.
        ;; TODO: Find a way to fix this.
        org-ql-view-buffers-files
      (org-ql-view--expand-buffers-files
       (completing-read "Buffers/Files: "
                        (list 'buffer 'org-agenda-files 'org-directory 'all)
                        nil nil (initial-input))))))

(defun org-ql-view--expand-buffers-files (buffers-files)
  "Return BUFFERS-FILES expanded to a list of files or buffers.
The counterpart to `org-ql-view--contract-buffers-files'."
  (pcase-exhaustive buffers-files
    ("all" (--select (equal (buffer-local-value 'major-mode it) 'org-mode)
                     (buffer-list)))
    ("org-agenda-files" (org-agenda-files))
    ("org-directory" (org-ql-search-directories-files))
    ((or "" "buffer") (current-buffer))
    ((pred bufferp) buffers-files)
    ((pred listp) buffers-files)
    ;; A single filename.
    ((pred stringp) buffers-files)))

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
  ;; Use space to separate sorting predicates, not comma.
  (let* ((crm-separator (rx space))
         (crm-local-completion-map (let ((map (copy-keymap crm-local-completion-map)))
                                     (define-key map (kbd "SPC") nil)
                                     map))
         (input (->> (completing-read-multiple "Sort by: "
                                               (list "buffer-order"
                                                     "date"
                                                     "deadline"
                                                     "priority"
                                                     "random"
                                                     "reverse"
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
