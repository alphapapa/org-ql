;;; org-ql-search.el --- Search commands for org-ql  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library is part of the package `org-ql'; it's not a standalone
;; library.  It implements search commands for Org buffers.

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

(require 'dash)
(require 'f)
(require 'map)
(require 'org-super-agenda)
(require 's)

(require 'org-ql)
(require 'org-ql-view)

(declare-function org-ql--normalize-query "org-ql" t t)

;;;; Compatibility

(defalias 'org-ql-search--link-heading-search-string
  (cond ((fboundp 'org-link--normalize-string) #'org-link--normalize-string)
        ((fboundp 'org-link-heading-search-string) #'org-link-heading-search-string)
        ((fboundp 'org-make-org-heading-search-string) #'org-make-org-heading-search-string)
        (t (error "org-ql: Unable to define alias `org-ql-search--link-heading-search-string'.  This may affect links in dynamic blocks.  Please report this as a bug"))))

(defalias 'org-ql-search--org-make-link-string
  (cond ((fboundp 'org-link-make-string) #'org-link-make-string)
        ((fboundp 'org-make-link-string) #'org-make-link-string)
        (t (error "org-ql: Unable to define alias `org-ql-search--org-make-link-string'.  Please report this as a bug"))))

(defalias 'org-ql-search--org-link-store-props
  (cond ((fboundp 'org-link-store-props) #'org-link-store-props)
        ((fboundp 'org-store-link-props) #'org-store-link-props)
        (t (error "org-ql: Unable to define alias `org-ql-search--org-link-store-props'.  Please report this as a bug"))))

(defalias 'org-ql--org-hide-archived-subtrees
  (if (version<= "9.6" org-version)
      'org-fold-hide-archived-subtrees
    'org-hide-archived-subtrees))

(defalias 'org-ql--org-show-context
  (if (version<= "9.6" org-version)
      'org-fold-show-context
    'org-show-context))

;;;; Variables

(defvar org-ql-block-header nil
  "Optional string overriding default header in `org-ql-block' agenda blocks.")

;;;; Customization

(defgroup org-ql-search nil
  "Options for `org-ql-search' commands."
  :group 'org-ql)

(defcustom org-ql-search-directories-files-regexp "\.org$"
  "Regular expression to match Org filenames in `org-directory'.
Files matching this regexp will be searched.  By default,
\".org\" files are matched, but you may also select to include
\".org_archive\" files, or use a custom regexp."
  :type '(radio (const :tag "Normal \".org\" files" :value "\.org$")
                (const :tag "Also include \".org_archive\" files" "\.org\\(_archive\\)?$")
                (string :tag "Custom regular expression")))

(defcustom org-ql-search-directories-files-recursive nil
  "Recurse into subdirectories by default in `org-ql-search-directories-files'.
This should probably be disabled by default, because
e.g. `org-directory' may include deeply nested directories of
non-Org files, such as a \".git\" directory, Org attachments
directories, etc, which would make it slow to list the
`org-directory' files recursively."
  :type 'boolean)

;;;; Commands

;;;###autoload
(cl-defun org-ql-sparse-tree (query &key keep-previous (buffer (current-buffer)))
  "Show a sparse tree for QUERY in BUFFER and return number of results.
The tree will show the lines where the query matches, and any
other context defined in `org-show-context-detail', which see.

QUERY is an `org-ql' query in either sexp or string form (see
Info node `(org-ql)Queries').

When KEEP-PREVIOUS is non-nil (interactively, with prefix), the
outline is not reset to the overview state before finding
matches, which allows stacking calls to this command.

Runs `org-occur-hook' after making the sparse tree."
  ;; Code based on `org-occur'.
  (interactive (list (read-string "Query: ")
                     :keep-previous current-prefix-arg))
  (with-current-buffer buffer
    (unless keep-previous
      ;; We don't do highlighting, because queries aren't regexps, but
      ;; we remove existing `org-occur' highlights, just in case.
      (org-remove-occur-highlights nil nil t)
      (org-overview))
    (let ((num-results 0)
          (query (pcase-exhaustive query
                   ((and (pred stringp)
                         (rx bos (0+ blank) (or "(" "\"")))
                    ;; Read sexp query from string.
                    (read query))
                   ((pred stringp)
                    ;; Parse string query into sexp query.
                    (org-ql--query-string-to-sexp query))
                   ((pred listp)
                    ;; Sexp query.
                    query))))
      (org-ql-select buffer query
        :action (lambda ()
                  (org-ql--org-show-context 'occur-tree)
                  (cl-incf num-results)))
      (unless org-sparse-tree-open-archived-trees
        (org-ql--org-hide-archived-subtrees (point-min) (point-max)))
      (run-hooks 'org-occur-hook)
      (unless (get-buffer-window buffer)
        (pop-to-buffer buffer))
      (message "%d matches" num-results)
      num-results)))

;;;###autoload
(cl-defun org-ql-search (buffers-files query &key narrow super-groups sort title
                                       (buffer org-ql-view-buffer))
  "Search for QUERY with `org-ql'.
Interactively, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.
Interactively, may also be:

- `buffer': search the current buffer
- `all': search all Org buffers
- `agenda': search buffers returned by the function `org-agenda-files'
- `directory': search Org files in `org-directory'
- A space-separated list of file or buffer names

QUERY: An `org-ql' query in either sexp or non-sexp form (see
Info node `(org-ql)Queries').

SUPER-GROUPS: An `org-super-agenda' group set.  See variable
`org-super-agenda-groups' and Info node `(org-super-agenda)Group
selectors'.

NARROW: When non-nil, don't widen buffers before
searching.  Interactively, with prefix, leave narrowed.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority' (see Info node `(org-ql)Listing / acting-on results').

TITLE: An optional string displayed in the header.

BUFFER: Optionally, a buffer or name of a buffer in which to
display the results.  By default, the value of
`org-ql-view-buffer' is used, and a new buffer is created if
necessary."
  (declare (indent defun))
  (interactive (list (org-ql-view--complete-buffers-files)
                     (read-string "Query: " (when org-ql-view-query
                                              (format "%S" org-ql-view-query)))
                     :narrow (or org-ql-view-narrow (equal current-prefix-arg '(4)))
                     :super-groups (org-ql-view--complete-super-groups)
                     :sort (org-ql-view--complete-sort)))
  ;; NOTE: Using `with-temp-buffer' is a hack to work around the fact that `make-local-variable'
  ;; does not work reliably from inside a `let' form when the target buffer is current on entry
  ;; to or exit from the `let', even though `make-local-variable' is actually done in
  ;; `org-ql-view--display'.  So we do all this within a temp buffer, which works around it.
  (with-temp-buffer
    (let* ((query (cl-etypecase query
                    (string (if (or (string-prefix-p "(" query)
                                    (string-prefix-p "\"" query))
                                ;; Read sexp query.
                                (read query)
                              ;; Parse non-sexp query into sexp query.
                              (org-ql--query-string-to-sexp query)))
                    (list query)))
           (results (org-ql-select buffers-files query
                      :action 'element-with-markers
                      :narrow narrow
                      :sort sort))
           (strings (-map #'org-ql-view--format-element results))
           (buffer (or buffer (format "%s %s*" org-ql-view-buffer-name-prefix (or title query))))
           (header (org-ql-view--header-line-format
                    :buffers-files buffers-files :query query :title title))
           ;; Bind variables for `org-ql-view--display' to set.
           (org-ql-view-buffers-files buffers-files)
           (org-ql-view-query query)
           (org-ql-view-sort sort)
           (org-ql-view-narrow narrow)
           (org-ql-view-super-groups super-groups)
           (org-ql-view-title title))
      (when super-groups
        (let ((org-super-agenda-groups (cl-etypecase super-groups
                                         (symbol (symbol-value super-groups))
                                         (list super-groups))))
          (setf strings (org-super-agenda--group-items strings))))
      (org-ql-view--display :buffer buffer :header header :strings strings))))

;;;###autoload
(defun org-ql-search-block (query)
  "Insert items for QUERY into current buffer.
QUERY should be an `org-ql' query form.  Intended to be used as a
user-defined function in `org-agenda-custom-commands'.  QUERY
corresponds to the `match' item in the custom command form.

Like other agenda block commands, it searches files returned by
function `org-agenda-files'.  Inserts a newline after the block.

If `org-ql-block-header' is non-nil, it is used as the header
string for the block, otherwise a the header is formed
automatically from the query."
  (let (narrow-p old-beg old-end)
    (when-let* ((from (pcase org-agenda-restrict
                        ('nil (org-agenda-files nil 'ifmode))
                        (_ (prog1 org-agenda-restrict
                             (with-current-buffer org-agenda-restrict
			       ;; Narrow the buffer; remember to widen it later.
			       (setf old-beg (point-min) old-end (point-max)
                                     narrow-p t)
			       (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end))))))
                (items (org-ql-select from query
                         :action 'element-with-markers
                         :narrow narrow-p)))
      (when narrow-p
        ;; Restore buffer's previous restrictions.
        (with-current-buffer from
          (narrow-to-region old-beg old-end)))
      ;; Not sure if calling the prepare function is necessary, but let's follow the pattern.
      (org-agenda-prepare)
      ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
      ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
      ;;  (org-agenda--insert-overriding-header (or org-ql-block-header (org-ql-agenda--header-line-format from query)))
      (insert (org-add-props (or org-ql-block-header (org-ql-view--header-line-format
                                                      :buffers-files from :query query))
                  nil 'face 'org-agenda-structure) "\n")
      ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
      ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
      ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
      (->> items
           (-map #'org-ql-view--format-element)
           org-agenda-finalize-entries
           insert)
      (insert "\n"))))

;;;###autoload
(defalias 'org-ql-block 'org-ql-search-block)

;;;; Dynamic blocks

;; This section implements support for Org dynamic blocks.  See Info node `(org)Dynamic blocks'.

(require 'org-table)

(cl-defun org-dblock-write:org-ql (params)
  "Insert content for org-ql dynamic block at point according to PARAMS.
Valid parameters include:

  :query    An Org QL query expression in either sexp or string
            form.

  :columns  A list of columns, including `heading', `todo',
            `property',`priority',`deadline',`scheduled',`closed'.
            Each column may also be specified as a list with the
            second element being a header string.  For example,
            to abbreviate the priority column: (priority \"P\").
            For certain columns, like `property', arguments may
            be passed by specifying the column type itself as a
            list.  For example, to display a column showing the
            values of a property named \"milestone\", with the
            header being abbreviated to \"M\":

              ((property \"milestone\") \"M\").

  :sort     One or a list of Org QL sorting methods
            (see `org-ql-select').

  :take     Optionally take a number of results from the front (a
            positive number) or the end (a negative number) of
            the results.

  :ts-format  Optional format string used to format
              timestamp-based columns.

For example, an org-ql dynamic block header could look like
this (must be a single line in the Org buffer):

  #+BEGIN: org-ql :query (todo \"UNDERWAY\")
:columns (priority todo heading) :sort (priority date)
:ts-format \"%Y-%m-%d %H:%M\""
  (-let* (((&plist :query :columns :sort :ts-format :take) params)
          (query (cl-etypecase query
                   (string (org-ql--query-string-to-sexp query))
                   (list ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
                    (org-ql--ask-unsafe-query query)
                    query)))
          (columns (or columns '(heading todo (priority "P"))))
          ;; MAYBE: Custom column functions.
          (format-fns
           ;; NOTE: Backquoting this alist prevents the lambdas from seeing
           ;; the variable `ts-format', so we use `list' and `cons'.
           (list (cons 'todo (lambda (element)
                               (org-element-property :todo-keyword element)))
                 (cons 'heading (lambda (element)
                                  (let ((normalized-heading
                                         (org-ql-search--link-heading-search-string (org-element-property :raw-value element))))
                                    (org-ql-search--org-make-link-string normalized-heading (org-link-display-format normalized-heading)))))
                 (cons 'priority (lambda (element)
                                   (--when-let (org-element-property :priority element)
                                     (char-to-string it))))
                 (cons 'deadline (lambda (element)
                                   (--when-let (org-element-property :deadline element)
                                     (ts-format ts-format (ts-parse-org-element it)))))
                 (cons 'scheduled (lambda (element)
                                    (--when-let (org-element-property :scheduled element)
                                      (ts-format ts-format (ts-parse-org-element it)))))
                 (cons 'closed (lambda (element)
                                 (--when-let (org-element-property :closed element)
                                   (ts-format ts-format (ts-parse-org-element it)))))
                 (cons 'property (lambda (element property)
                                   (org-element-property (intern (concat ":" (upcase property))) element)))))
          (elements (org-ql-query :from (current-buffer)
                                  :where query
                                  :select '(org-ql-view--resolve-element-properties
                                            (org-element-headline-parser (line-end-position)))
                                  :order-by sort)))
    (when take
      (setf elements (cl-etypecase take
                       ((and integer (satisfies cl-minusp)) (-take-last (abs take) elements))
                       (integer (-take take elements)))))
    (cl-labels ((format-element (element)
                  (string-join (cl-loop for column in columns
                                        collect (or (pcase-exhaustive column
                                                      ((pred symbolp)
                                                       (funcall (alist-get column format-fns) element))
                                                      (`((,column . ,args) ,_header)
                                                       (apply (alist-get column format-fns) element args))
                                                      (`(,column ,_header)
                                                       (funcall (alist-get column format-fns) element)))
                                                    ""))
                               " | ")))
      ;; Table header
      (insert "| " (string-join (--map (pcase it
                                         ((pred symbolp) (capitalize (symbol-name it)))
                                         (`(,_ ,name) name))
                                       columns)
                                " | ")
              " |" "\n")
      (insert "|- \n")                  ; Separator hline
      (dolist (element elements)
        (insert "| " (format-element element) " |" "\n"))
      (delete-char -1)
      (org-table-align))))

;;;; Functions

(defvar org-ql-search-directories-files-error
  ;; Workaround to silence byte-compiler which thinks having this string in an
  ;; argument's default value form is a too-long docstring.
  "No DIRECTORIES given, and `org-directory' doesn't exist")

(cl-defun org-ql-search-directories-files
    (&key (directories
           (if (file-exists-p org-directory)
               (list org-directory)
             (user-error org-ql-search-directories-files-error)))
          (recurse org-ql-search-directories-files-recursive)
          (regexp org-ql-search-directories-files-regexp))
  "Return list of matching files in DIRECTORIES.
When RECURSE is non-nil, recurse into subdirectories.  When
REGEXP is non-nil, only return files that match REGEXP."
  (let ((files (->> directories
                    (--map (f-files it nil recurse))
                    -flatten)))
    (if regexp
        (--select (string-match regexp it)
                  files)
      files)))

;;;; Footer

(provide 'org-ql-search)

;;; org-ql-search.el ends here
