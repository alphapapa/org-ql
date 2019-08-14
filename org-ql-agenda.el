;;; org-ql-agenda.el --- Agenda-like view based on org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library is part of the package `org-ql'; it's not a standalone
;; library.  It displays buffers similar to Org Agenda buffers, based
;; on `org-ql' queries.

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

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'seq)
(require 'rx)

(require 'org-ql)

(require 'dash)
(require 's)

;;;; Compatibility

(defvar org-super-agenda-auto-selector-keywords)
(defvar org-super-agenda-groups)
(defvar org-super-agenda-mode)
(declare-function org-super-agenda--group-items "ext:org-super-agenda")

(when (version< org-version "9.2")
  (defalias 'org-get-tags #'org-get-tags-at))

;;;; Faces

(defface org-ql-agenda-due-date
  '((t (:slant italic :weight bold)))
  "Face for due dates in `org-ql-agenda' views.")

;;;; Variables

(defvar org-ql-agenda-buffer-name "*Org-QL-Agenda*"
  "Name of default `org-ql-agenda' buffer.")

;; For refreshing results buffers.
(defvar org-ql-buffers-files)
(defvar org-ql-query)
(defvar org-ql-sort)
(defvar org-ql-narrow)
(defvar org-ql-super-groups)
(defvar org-ql-title)

;;;; Macros

;; FIXME: DRY these two macros.

(cl-defmacro org-ql-agenda (&rest args)
  "Display an agenda-like buffer of entries in FILES that match QUERY.

FILES-OR-QUERY is a sexp that is evaluated to get the list of
buffers and files to scan.

QUERY is an `org-ql' query.  The query may be passed as
FILES-OR-QUERY and QUERY may be left nil, in which case the list
of files will automatically be set to the value of calling
`org-agenda-files'.

SORT is passed to `org-ql', which see..

NARROW, when non-nil, means to respect narrowing in buffers.
When nil, buffers are widened before being searched.

BUFFER, when non-nil, is a buffer or buffer name to display the
agenda in, rather than the default.

SUPER-GROUPS is used to bind variable `org-super-agenda-groups',
which see.  If t, the existing value of `org-super-agenda-groups'
is used, rather than binding it locally."
  (declare (indent defun)
           (advertised-calling-convention (files-or-query &optional query &key sort narrow buffer super-groups title) nil))
  (cl-macrolet ((set-keyword-args (args)
                                  `(setq sort (plist-get ,args :sort)
                                         narrow (plist-get ,args :narrow)
                                         buffer (plist-get ,args :buffer)
                                         super-groups (plist-get ,args :super-groups)
                                         title (plist-get ,args :title))))
    (let ((files '(org-agenda-files))
          query sort narrow buffer super-groups)
      ;; Parse args manually (so we can leave FILES nil for a default argument).
      ;; TODO: DRY this and org-ql, I think.
      (pcase args
        (`(,arg-files ,arg-pred . ,(and rest (guard (keywordp (car rest)))))
         ;; Files, query, and keyword args (FIXME: Can I combine this and the next one?  Does it
         ;; matter if rest is nil or starts with a keyword?)
         (setq files arg-files
               query arg-pred)
         (set-keyword-args rest))
        (`(,arg-pred . ,(and rest (guard (keywordp (car rest)))))
         ;; Query and keyword args, no files
         (setq query arg-pred)
         (set-keyword-args rest))
        (`(,arg-files ,arg-pred)
         ;; Files and query, no keywords
         (setq files arg-files
               query arg-pred))
        (`(,arg-pred)
         ;; Only query
         (setq query arg-pred)))
      (when (eq super-groups t)
        (setq super-groups org-super-agenda-groups))
      ;; Call --agenda
      `(org-ql-agenda--agenda ,files
         ;; TODO: Probably better to just use eval on org-ql rather than reimplementing parts of it here.
         ',query
         :sort ',sort
         :buffer ,buffer
         :narrow ,narrow
         :super-groups ',super-groups
         :title ,title))))

;;;; Commands

;; TODO: This is called `org-ql-search' but it's in org-ql-agenda.el because
;; it uses `org-ql-agenda--agenda'.  Maybe this could be better organized.

;;;###autoload
(cl-defun org-ql-search (buffers-files query &key narrow groups sort title)
  "Read QUERY and search with `org-ql'.
Interactively, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.
Interactively, may also be:

- `buffer': search the current buffer
- `all': search all Org buffers
- `agenda': search buffers returned by the function `org-agenda-files'
- An expression which evaluates to a list of files/buffers
- A space-separated list of file or buffer names

GROUPS: An `org-super-agenda' group set.  See variable
`org-super-agenda-groups'.

NARROW: When non-nil, don't widen buffers before
searching. Interactively, with prefix, leave narrowed.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority'.

TITLE: An optional string displayed in the header."
  (declare (indent defun))
  (interactive (list (pcase-exhaustive (completing-read "Buffers/Files: "
                                                        (list 'buffer 'agenda 'all))
                       ("agenda" (org-agenda-files))
                       ("all" (--select (equal (buffer-local-value 'major-mode it) 'org-mode)
                                        (buffer-list)))
                       ("buffer" (current-buffer))
                       ((and form (guard (rx bos "("))) (-flatten (eval (read form))))
                       (else (s-split (rx (1+ space)) else)))
                     (read-minibuffer "Query: ")
                     :narrow (eq current-prefix-arg '(4))
                     :groups (pcase (completing-read "Group by: "
                                                     (cons "Don't group"
                                                           (cl-loop for type in org-super-agenda-auto-selector-keywords
                                                                    collect (substring (symbol-name type) 6))))
                               ("Don't group" nil)
                               (property (list (list (intern (concat ":auto-" property))))))
                     :sort (pcase (completing-read "Sort by: "
                                                   (list "Don't sort"
                                                         "date"
                                                         "deadline"
                                                         "priority"
                                                         "scheduled"
                                                         "todo"))
                             ("Don't sort" nil)
                             (sort (intern sort)))))
  (org-ql-agenda--agenda buffers-files
    query
    :narrow narrow
    :sort sort
    :super-groups groups
    :title title
    :buffer "*Org QL Search*"))

(defun org-ql-search-refresh ()
  "Refresh current `org-ql-search' buffer."
  (interactive)
  (org-ql-agenda--agenda org-ql-buffers-files
    org-ql-query
    :sort org-ql-sort
    :narrow org-ql-narrow
    :super-groups org-ql-super-groups
    :title org-ql-title
    :buffer (current-buffer)))

;;;; Functions

;; TODO: Move the action-fn down into --filter-buffer, so users can avoid calling the
;; headline-parser when they don't need it.

(cl-defun org-ql-agenda--agenda (buffers-files query &key entries sort buffer narrow super-groups title)
  "FIXME: Docstring"
  (declare (indent defun))
  (when (and super-groups (not org-super-agenda-mode))
    (user-error "`org-super-agenda-mode' must be activated to use grouping"))
  (let* ((org-super-agenda-groups super-groups)
         (entries (or entries
                      (--> (org-ql-select buffers-files
                             query
                             :sort sort
                             :narrow narrow
                             :action (lambda ()
                                       (->> (org-element-headline-parser (line-end-position))
                                            org-ql--add-markers))))))
         (string (--> entries
                      (mapcar #'org-ql-agenda--format-element it)
                      (cond ((bound-and-true-p org-super-agenda-mode) (org-super-agenda--group-items it))
                            (t it))
                      (s-join "\n" it)))
         (buffer (cl-etypecase buffer
                   (string (org-ql-agenda--buffer buffer))
                   (null (org-ql-agenda--buffer buffer))
                   (buffer buffer)))
         (map (copy-keymap org-agenda-mode-map))
         (inhibit-read-only t))
    (define-key map "g" #'org-ql-search-refresh)
    (with-current-buffer buffer
      (use-local-map map)
      ;; Prepare buffer, saving data for refreshing.
      (setq-local org-ql-buffers-files buffers-files)
      (setq-local org-ql-query query)
      (setq-local org-ql-sort sort)
      (setq-local org-ql-narrow narrow)
      (setq-local org-ql-super-groups super-groups)
      (setq-local org-ql-title title)
      (setq-local header-line-format (org-ql-agenda--header-line-format buffers-files query title))
      ;; Clear buffer, insert entries, etc.
      (erase-buffer)
      (insert string)
      (pop-to-buffer (current-buffer))
      (org-agenda-finalize)
      (goto-char (point-min)))))

(defun org-ql-agenda-block (query)
  "Insert items for QUERY into current buffer.
QUERY should be an `org-ql' query form.  Like other agenda block
commands, it searches files returned by function
`org-agenda-files'.  Intended to be used as a user-defined
function in `org-agenda-custom-commands'.  QUERY corresponds to
the `match' item in the custom command form."
  (when-let* ((from (org-agenda-files nil 'ifmode))
              (items (org-ql-select from
                       query :action 'element-with-markers)))
    ;; Not sure if calling the prepare function is necessary, but let's follow the pattern.
    (org-agenda-prepare)
    ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
    ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
    ;;  (org-agenda--insert-overriding-header (org-ql-agenda--header-line-format from query))
    (insert (org-add-props (org-ql-agenda--header-line-format from query)
		nil 'face 'org-agenda-structure) "\n")
    ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
    ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
    ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
    (->> items
         (-map #'org-ql-agenda--format-element)
         org-agenda-finalize-entries
         insert)))

(defalias 'org-ql-block 'org-ql-agenda-block)

(defun org-ql-agenda--header-line-format (buffers-files query &optional title)
  "Return header-line-format for BUFFERS-FILES and QUERY."
  (let* ((title (if title
                    (concat (propertize "View:" 'face 'org-agenda-structure)
                            title " ")
                  ""))
         (query-formatted (format "%S" query))
         (query-formatted (propertize (org-ql-agenda--font-lock-string 'emacs-lisp-mode query-formatted)
                                      'help-echo query-formatted))
         (query-width (length query-formatted))
         (available-width (- (window-width)
                             (length "In: ")
                             (length "Query: ")
                             query-width 4))
         (buffers-files-formatted (format "%S" buffers-files))
         (buffers-files-formatted (propertize (->> buffers-files-formatted
                                                   (org-ql-agenda--font-lock-string 'emacs-lisp-mode)
                                                   (s-truncate available-width))
                                              'help-echo buffers-files-formatted)))
    (concat title
            (propertize "Query:" 'face 'org-agenda-structure)
            query-formatted "  "
            (propertize "In:" 'face 'org-agenda-structure)
            buffers-files-formatted)))

(defun org-ql-agenda--font-lock-string (mode s)
  "Return string S font-locked according to MODE."
  ;; FIXME: Is this the proper way to do this?  It works, but I feel like there must be a built-in way...
  (with-temp-buffer
    (delay-mode-hooks
      (insert s)
      (funcall mode)
      (font-lock-ensure)
      (buffer-string))))

(defun org-ql-agenda--buffer (&optional name)
  "Return Agenda NG buffer, creating it if necessary.
If NAME is non-nil, return buffer by that name instead of using
default buffer."
  (with-current-buffer (get-buffer-create (or name org-ql-agenda-buffer-name))
    (unless (eq major-mode 'org-agenda-mode)
      (org-agenda-mode))
    (current-buffer)))

(defun org-ql-agenda--format-relative-date (difference)
  ;; MAYBE: Make this a `defsubst'.
  "Return relative date string for DIFFERENCE.
DIFFERENCE should be an integer number of days, positive for
dates in the past, and negative for dates in the future."
  (cond ((> difference 0)
         (format "%sd ago" difference))
        ((< difference 0)
         (format "in %sd" (* -1 difference)))
        (t "today")))

;;;; Faces/properties

(defun org-ql-agenda--format-element (element)
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
           (title (--> (org-ql-agenda--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                   (org-ql-agenda--add-todo-face it)))
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
                                      (org-ql-agenda--add-priority-face)))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-agenda-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
           ;; FIXME: Use proper prefix
           (concat "  " it)
           (org-add-props it properties
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(defun org-ql-agenda--add-faces (element)
  "Return ELEMENT with deadline and scheduled faces added."
  (->> element
       (org-ql-agenda--add-scheduled-face)
       (org-ql-agenda--add-deadline-face)))

(defun org-ql-agenda--add-priority-face (string)
  "Return STRING with priority face added."
  (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
    (let ((face (org-get-priority-face (string-to-char (match-string 2 string)))))
      (org-add-props string nil 'face face 'font-lock-fontified t))))

(defun org-ql-agenda--add-scheduled-face (element)
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
             (relative-due-date (org-add-props (org-ql-agenda--format-relative-date difference-days) nil
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

(defun org-ql-agenda--add-deadline-face (element)
  "Add faces to ELEMENT's title for its deadline status.
Also store relative due date as string in `:relative-due-date'
property."
  ;; FIXME: In my config, doesn't apply orange for approaching deadline the same way the Org Agenda does.
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
             (difference-days (- today-day-number deadline-day-number))
             (relative-due-date (org-add-props (org-ql-agenda--format-relative-date difference-days) nil
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

(defun org-ql-agenda--add-todo-face (keyword)
  "Return KEYWORD with TODO face added."
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;; Footer

(provide 'org-ql-agenda)

;;; org-ql-agenda.el ends here
