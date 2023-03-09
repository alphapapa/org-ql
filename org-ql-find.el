;;; org-ql-find.el --- Find headings with completion using org-ql  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>

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

;;; Commentary:

;; This library provides a way to quickly find and go to Org entries
;; selected with Emacs's built-in completions API (so it works with
;; packages that extend it, like Vertico, Marginalia, etc).  It works
;; like `helm-org-ql' but does not require Helm.

;;; Code:

(require 'cl-lib)

(require 'org)
(require 'org-ql)
(require 'org-ql-search)
(require 'org-ql-completing-read)

;;;; Customization

(defgroup org-ql-find nil
  "Options for `org-ql-find'."
  :group 'org-ql)

(defcustom org-ql-find-goto-hook '(org-show-entry org-reveal)
  "Functions called when selecting an entry."
  :type 'hook)

(defcustom org-ql-find-display-buffer-action '(display-buffer-same-window)
  "Display buffer action list for `org-ql-find'.
See function `display-buffer'."
  :type 'sexp)

;;;; Functions

;;;###autoload
(cl-defun org-ql-find (buffers-files &key query-prefix query-filter
                                     (prompt "Find entry: "))
  "Go to an Org entry in BUFFERS-FILES selected by searching entries with `org-ql'.
Interactively, with universal prefix, select multiple buffers to
search with completion and PROMPT.

QUERY-PREFIX may be a string to prepend to the query (e.g. use
\"heading:\" to only search headings, easily creating a custom
command that saves the user from having to type it).

QUERY-FILTER may be a function through which the query the user
types is filtered before execution (e.g. it could replace spaces
with commas to turn multiple tokens, which would normally be
treated as multiple predicates, into multiple arguments to a
single predicate)."
  (interactive
   (list (if current-prefix-arg
             (mapcar #'get-buffer
                     (completing-read-multiple
                      "Buffers: "
                      (cl-loop for buffer in (buffer-list)
                               when (eq 'org-mode (buffer-local-value 'major-mode buffer))
                               collect (buffer-name buffer))
                      nil t))
           (progn
             (unless (eq major-mode 'org-mode)
               (user-error "This is not an Org buffer: %S" (current-buffer)))
             (current-buffer)))))
  (let ((marker (org-ql-completing-read buffers-files
                  :query-prefix query-prefix
                  :query-filter query-filter
                  :prompt prompt)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (display-buffer (current-buffer) org-ql-find-display-buffer-action)
      (select-window (get-buffer-window (current-buffer)))
      (run-hook-with-args 'org-ql-find-goto-hook))))

;;;###autoload
(defun org-ql-refile (marker)
  "Refile current entry to MARKER (interactively, one selected with `org-ql').
Interactive completion uses files listed in `org-refile-targets',
which see (but only the files are used)."
  (interactive (let ((buffers-files (delete-dups
                                     ;; Always include the current buffer.
                                     (cons (current-buffer)
                                           (cl-loop for (files-spec . _candidate-spec) in org-refile-targets
                                                    append (cl-typecase files-spec
                                                             (null (list (current-buffer)))
                                                             (symbol (pcase (funcall files-spec)
                                                                       ((and (pred stringp) file) (list file))
                                                                       ((and (pred listp) files) files)))
                                                             (list files-spec)))))))
                 (list (org-ql-completing-read buffers-files :prompt "Refile to: "))))
  (org-refile nil nil
              ;; The RFLOC argument:
              (list
               ;; Name
               (org-with-point-at marker
                 (nth 4 (org-heading-components)))
               ;; File
               (buffer-file-name (marker-buffer marker))
               ;; nil
               nil
               ;; Position
               marker)))

;;;###autoload
(defun org-ql-find-in-agenda ()
  "Call `org-ql-find' on `org-agenda-files'."
  (interactive)
  (org-ql-find (org-agenda-files)))

;;;###autoload
(defun org-ql-find-in-org-directory ()
  "Call `org-ql-find' on files in `org-directory'."
  (interactive)
  (org-ql-find (org-ql-search-directories-files)))

(provide 'org-ql-find)

;;; org-ql-find.el ends here
