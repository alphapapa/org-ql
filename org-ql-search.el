;;; org-ql-search.el --- Search commands for org-ql  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 's)

(require 'org-ql)
(require 'org-ql-view)

;;;; Variables

(defvar org-ql-views)

;; From `org-super-agenda'.
(defvar org-super-agenda-mode)
(defvar org-super-agenda-groups)
(defvar org-super-agenda-auto-selector-keywords)
(declare-function org-super-agenda--group-items "ext:org-super-agenda")

;; For refreshing results buffers.
(defvar org-ql-view-buffers-files)
(defvar org-ql-view-query)
(defvar org-ql-view-sort)
(defvar org-ql-view-narrow)
(defvar org-ql-view-super-groups)
(defvar org-ql-view-title)

;;;; Commands

;;;###autoload
(cl-defun org-ql-search (buffers-files query &key buffer narrow super-groups sort title)
  "Read QUERY and search with `org-ql'.
Interactively, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.
Interactively, may also be:

- `buffer': search the current buffer
- `all': search all Org buffers
- `agenda': search buffers returned by the function `org-agenda-files'
- An expression which evaluates to a list of files/buffers
- A space-separated list of file or buffer names

SUPER-GROUPS: An `org-super-agenda' group set.  See variable
`org-super-agenda-groups'.

NARROW: When non-nil, don't widen buffers before
searching. Interactively, with prefix, leave narrowed.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority'.

TITLE: An optional string displayed in the header.

BUFFER: Optionally, a buffer or name of a buffer in which to
display the results.  Set automatically when nil."
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
                     :super-groups (when (bound-and-true-p org-super-agenda-auto-selector-keywords)
                                     (pcase (completing-read "Group by: "
                                                             (append (list "Don't group"
                                                                           "Global super-groups")
                                                                     (cl-loop for type in org-super-agenda-auto-selector-keywords
                                                                              collect (substring (symbol-name type) 6))))
                                       ("Global super-groups" org-super-agenda-groups)
                                       ("Don't group" nil)
                                       (property (list (list (intern (concat ":auto-" property)))))))
                     :sort (pcase (completing-read "Sort by: "
                                                   (list "Don't sort"
                                                         "date"
                                                         "deadline"
                                                         "priority"
                                                         "scheduled"
                                                         "todo"))
                             ("Don't sort" nil)
                             (sort (intern sort)))))
  (let* ((results (org-ql-select buffers-files query
                    :action 'element-with-markers
                    :narrow narrow
                    :sort sort))
         (strings (-map #'org-ql-view--format-element results))
         ;; Bind variables for `org-ql-view--display' to set.
         (org-ql-view-buffers-files buffers-files)
         (org-ql-view-query query)
         (org-ql-view-sort sort)
         (org-ql-view-narrow narrow)
         (org-ql-view-super-groups super-groups)
         (org-ql-view-title title))
    (when super-groups
      (setf strings (org-super-agenda--group-items strings)))
    (org-ql-view--display
      :buffer (or buffer
                  (format "*Org QL Search: %s*"
                          (or title
                              (format "%S in %S" query buffers-files))))
      :header (org-ql-view--header-line-format buffers-files query title)
      :string (s-join "\n" strings))))

(defun org-ql-search-refresh ()
  "Refresh current `org-ql-search' buffer."
  (interactive)
  (let ((old-pos (point)))
    (org-ql-search org-ql-view-buffers-files
      org-ql-view-query
      :sort org-ql-view-sort
      :narrow org-ql-view-narrow
      :super-groups org-ql-view-super-groups
      :title org-ql-view-title
      :buffer (current-buffer))
    (goto-char old-pos)))

(defun org-ql-search-save ()
  "Save current `org-ql-search' buffer to `org-ql-views'."
  (interactive)
  (let* ((name (read-string "Save view as: "))
         (buffers-files-sexp (cl-etypecase org-ql-view-buffers-files
                               (string org-ql-view-buffers-files)
                               (list `(list ,@org-ql-view-buffers-files))
                               (null nil)))
         (function `(lambda ()
                      (interactive)
                      (org-ql-search ,buffers-files-sexp
                        ',org-ql-view-query
                        :sort ',org-ql-view-sort
                        :narrow ,org-ql-view-narrow
                        :groups ',org-ql-view-super-groups
                        :title ,name))))
    (map-put org-ql-views name function #'equal)
    (customize-set-variable 'org-ql-views org-ql-views)
    (customize-mark-to-save 'org-ql-views)))

(defun org-ql-search-block (query)
  "Insert items for QUERY into current buffer.
QUERY should be an `org-ql' query form.  Like other agenda block
commands, it searches files returned by function
`org-agenda-files'.  Intended to be used as a user-defined
function in `org-agenda-custom-commands'.  QUERY corresponds to
the `match' item in the custom command form.  Inserts a newline
after the block."
  (when-let* ((from (org-agenda-files nil 'ifmode))
              (items (org-ql-select from
                       query :action 'element-with-markers)))
    ;; Not sure if calling the prepare function is necessary, but let's follow the pattern.
    (org-agenda-prepare)
    ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
    ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
    ;;  (org-agenda--insert-overriding-header (org-ql-search--header-line-format from query))
    (insert (org-add-props (org-ql-view--header-line-format from query)
		nil 'face 'org-agenda-structure) "\n")
    ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
    ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
    ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
    (->> items
         (-map #'org-ql-view--format-element)
         org-agenda-finalize-entries
         insert)
    (insert "\n")))

;;;###autoload
(defalias 'org-ql-block 'org-ql-search-block)

;;;; Footer

(provide 'org-ql-search)

;;; org-ql-search.el ends here
