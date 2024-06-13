;;; org-ql-find.el --- Find headings with completion using org-ql  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Adam Porter

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

(declare-function org-ql--normalize-query "org-ql" t t)

;;;; Customization

(defgroup org-ql-find nil
  "Options for `org-ql-find'."
  :group 'org-ql)

(defcustom org-ql-find-goto-hook '(org-show-entry org-reveal)
  "Functions called when selecting an entry."
  ;; TODO: Add common choices, including `org-tree-to-indirect-buffer'.
  :type 'hook)

(defcustom org-ql-find-display-buffer-action '(display-buffer-same-window)
  "Display buffer action list for `org-ql-find'.
See function `display-buffer'."
  :type 'sexp)

;;;; Commands

;;;###autoload
(cl-defun org-ql-find (buffers-files &key query-prefix query-filter
                                     (prompt "Find entry: "))
  "Go to an Org entry in BUFFERS-FILES selected by searching entries with `org-ql'.
Interactively, search the buffers and files relevant to the
current buffer (i.e. in `org-agenda-mode', the value of
`org-ql-view-buffers-files' or `org-agenda-contributing-files';
in `org-mode', that buffer).  With universal prefix, select
multiple buffers to search with completion and PROMPT.

QUERY-PREFIX may be a string to prepend to the query (e.g. use
\"heading:\" to only search headings, easily creating a custom
command that saves the user from having to type it).

QUERY-FILTER may be a function through which the query the user
types is filtered before execution (e.g. it could replace spaces
with commas to turn multiple tokens, which would normally be
treated as multiple predicates, into multiple arguments to a
single predicate)."
  (interactive (list (org-ql-find--buffers)))
  (let ((marker (org-ql-completing-read buffers-files
                  :query-prefix query-prefix
                  :query-filter query-filter
                  :prompt prompt)))
    (set-buffer (or (buffer-base-buffer (marker-buffer marker))
                    (marker-buffer marker)))
    (pop-to-buffer (current-buffer) org-ql-find-display-buffer-action)
    (without-restriction
      (goto-char marker)
      (run-hook-with-args 'org-ql-find-goto-hook))
    (when (equal (current-buffer) (marker-buffer marker))
      ;; Ensure point is still within visible portion of buffer.  (If
      ;; `org-tree-to-indirect-buffer' is used in `org-ql-find-goto-hook',
      ;; the buffer will have been changed and it won't matter; otherwise,
      ;; the buffer could have been narrowed to a region excluding the
      ;; selected entry.)
      (let ((end-of-subtree (org-with-point-at marker
                              (org-end-of-subtree 'invisible-ok))))
        (unless (and (<= (point-min) marker)
                     (>= (point-max) end-of-subtree))
          (widen)
          (goto-char marker))))))

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

;;;###autoload
(defun org-ql-find-path (buffers-files)
  "Call `org-ql-find' to search outline paths in BUFFERS-FILES.
Interactively, search the buffers and files relevant to the
current buffer (i.e. in `org-agenda-mode', the value of
`org-ql-view-buffers-files' or `org-agenda-contributing-files';
in `org-mode', that buffer).  With universal prefix, select
multiple buffers to search with completion and PROMPT."
  (interactive (list (org-ql-find--buffers)))
  (let ((org-ql-default-predicate 'outline-path))
    (org-ql-find buffers-files)))

;;;###autoload
(cl-defun org-ql-open-link (buffers-files &key query-prefix query-filter
                                          (prompt "Open link: "))
  "Open a link selected with `org-ql-completing-read'.
Links found in entries matching the input query are offered as
candidates, and the selected one is opened with
`org-open-at-point'.  Arguments BUFFERS-FILES, QUERY-FILTER,
QUERY-PREFIX, and PROMPT are passed to `org-ql-completing-read',
which see.

Interactively, search the buffers and files relevant to the
current buffer (i.e. in `org-agenda-mode', the value of
`org-ql-view-buffers-files' or `org-agenda-contributing-files';
in `org-mode', that buffer).  With universal prefix, select
multiple buffers to search with completion and PROMPT."
  (interactive (list (org-ql-find--buffers)))
  (let* ((marker (org-ql-completing-read buffers-files
                   :query-prefix query-prefix
                   :query-filter query-filter
                   :prompt prompt
                   :action-filter #'identity
                   :action (lambda ()
                             (save-excursion
                               (cl-loop with limit = (org-entry-end-position)
                                        while (re-search-forward org-link-any-re limit t)
                                        for link = (string-trim (match-string 0))
                                        do (progn
                                             (set-text-properties 0 (length link) '(face org-link) link)
                                             (setf link (org-link-display-format link)))
                                        collect (cons link (copy-marker (match-beginning 0))))))
                   :snippet (lambda (&rest _)
                              "")
                   :path (lambda (marker)
                           (org-with-point-at marker
                             (let* ((path (thread-first (org-get-outline-path t t)
                                                        (org-format-outline-path (window-width) nil "")
                                                        (org-split-string "")))
                                    (formatted-path (if org-ql-completing-read-reverse-paths
                                                        (concat "\\" (string-join (reverse path) "\\"))
                                                      (concat "/" (string-join path "/")))))
                               formatted-path))))))
    (org-with-point-at marker
      (org-open-at-point))))

;;;; Functions

(defun org-ql-find--buffers ()
  "Return list of buffers to search in.
In a mode derived from `org-agenda-mode', return the value of
`org-ql-view-buffers-files' or `org-agenda-contributing-files'.
In a mode derived from `org-mode', return the current buffer.
When `current-prefix-arg', read a list of buffers in `org-mode'
with completion.  To be used in `org-ql-find' commands'
interactive forms."
  (if current-prefix-arg
      (mapcar #'get-buffer
              (completing-read-multiple
               "Buffers: "
               (cl-loop for buffer in (buffer-list)
                        when (eq 'org-mode (buffer-local-value 'major-mode buffer))
                        collect (buffer-name buffer))
               nil t))
    (cond ((derived-mode-p 'org-agenda-mode) (or org-ql-view-buffers-files
                                                 org-agenda-contributing-files))
          ((derived-mode-p 'org-mode) (current-buffer))
          (t (user-error "This is not an Org-related buffer: %S" (current-buffer))))))

(provide 'org-ql-find)

;;; org-ql-find.el ends here
