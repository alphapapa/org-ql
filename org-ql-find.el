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

;;;; Customization

(defgroup org-ql-find nil
  "Options for `org-ql-find'."
  :group 'org-ql)

(defcustom org-ql-find-reverse-paths t
  "Whether to reverse Org outline paths in `org-ql-find' results."
  :type 'boolean)

(defcustom org-ql-find-goto-hook '(org-show-entry)
  "Functions called when selecting an entry."
  :type 'hook)

(defface org-ql-find-snippet '((t (:inherit font-lock-comment-face)))
  "Snippets.")

;;;; Functions

;;;###autoload
(cl-defun org-ql-find (buffers-files &key query-prefix
                                     (prompt "Find entry: "))
  "Go to an Org entry in BUFFERS-FILES selected by searching entries with `org-ql'.
Interactively, with universal prefix, select multiple buffers to
search with completion.

If QUERY-PREFIX, prepend it to the query (e.g. use \"heading:\"
to only search headings, easily creating a custom command that
saves the user from having to type it)."
  (interactive
   (list (if current-prefix-arg
             (mapcar #'get-buffer
                     (completing-read-multiple
                      "Buffers: "
                      (mapcar #'buffer-name
                              (cl-remove-if-not (lambda (buffer)
                                                  (eq 'org-mode (buffer-local-value 'major-mode buffer)))
                                                (buffer-list))) nil t))
           (current-buffer))))
  ;; Emacs's completion API is not always easy to understand,
  ;; especially when using "programmed completion."  This code was
  ;; made possible by the example Clemens Radermacher shared at
  ;; <https://github.com/radian-software/selectrum/issues/114#issuecomment-744041532>.
  (let ((table (make-hash-table :test #'equal))
        (window-width (window-width)))
    (cl-labels ((action
                 () (font-lock-ensure (point-at-bol) (point-at-eol))
                 (let* ((path (thread-first (org-get-outline-path t t)
                                            (org-format-outline-path window-width nil "")
                                            (org-split-string "")))
                        (path (if org-ql-find-reverse-paths
                                  (string-join (nreverse path) "\\")
                                (string-join path "/"))))
                   (puthash path (point-marker) table)
                   path))
                (affix (completions)
                       (cl-loop for completion in completions
                                for marker = (gethash completion table)
                                for todo-state = (if-let (it (org-entry-get marker "TODO"))
                                                     (concat (propertize it
                                                                         'face (org-get-todo-face it))
                                                             " ")
                                                   "")
                                for snippet = (if-let (it (snippet marker))
                                                  (propertize (concat " " it)
                                                              'face 'org-ql-find-snippet)
                                                "")
                                collect (list completion todo-state snippet)))
                (annotate (candidate)
                          (or (snippet (gethash candidate table)) ""))
                (snippet (marker)
                         (org-with-point-at marker
                           (org-end-of-meta-data t)
                           (unless (org-at-heading-p)
                             (let ((end (min (+ (point) 51)
                                             (org-entry-end-position))))
                               (truncate-string-to-width
                                (replace-regexp-in-string "\n" " " (buffer-substring (point) end)
                                                          t t)
                                50 nil nil t)))))
                (group (candidate transform)
                       (pcase transform
                         (`nil (buffer-name (marker-buffer (gethash candidate table))))
                         (_ candidate)))
                (try (string _table _pred point &optional _metadata)
                     (cons string point))
                (all (string table pred _point)
                     (all-completions string table pred))
                (collection (str _pred flag)
                            (pcase flag
                              ('metadata (list 'metadata
                                               (cons 'group-function #'group)
                                               (cons 'affixation-function #'affix)
                                               (cons 'annotation-function #'annotate)))
                              (`t (unless (string-empty-p str)
                                    (org-ql-select buffers-files (org-ql--query-string-to-sexp (concat query-prefix str))
                                      :action #'action))))))
      (let* ((completion-styles '(org-ql-find))
             (completion-styles-alist (list (list 'org-ql-find #'try #'all "Org QL Find")))
             (selected (completing-read prompt #'collection nil))
             (marker (gethash selected table)))
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (display-buffer (current-buffer))
          (run-hook-with-args 'org-ql-find-goto-hook))))))

;;;###autoload
(defun org-ql-find-heading (buffers-files)
  "Go to an Org entry in BUFFERS-FILES selected by searching with `org-ql'.
Only headings are searched (using the \"heading:\" predicate).
Interactively, with universal prefix, select multiple buffers to
search with completion."
  (interactive
   (list (if current-prefix-arg
             (mapcar #'get-buffer
                     (completing-read-multiple
                      "Buffers: "
                      (mapcar #'buffer-name
                              (cl-remove-if-not (lambda (buffer)
                                                  (eq 'org-mode (buffer-local-value 'major-mode buffer)))
                                                (buffer-list))) nil t))
           (current-buffer))))
  (org-ql-find buffers-files :prompt "Find heading: " :query-prefix "heading:"))

;;;###autoload
(defun org-ql-find-path (buffers-files)
  "Go to an Org entry in BUFFERS-FILES selected by searching with `org-ql'.
Only outline paths are searched (using the \"outline-path:\"
predicate).  Interactively, with universal prefix, select
multiple buffers to search with completion."
  (interactive
   (list (if current-prefix-arg
             (mapcar #'get-buffer
                     (completing-read-multiple
                      "Buffers: "
                      (mapcar #'buffer-name
                              (cl-remove-if-not (lambda (buffer)
                                                  (eq 'org-mode (buffer-local-value 'major-mode buffer)))
                                                (buffer-list))) nil t))
           (current-buffer))))
  (org-ql-find buffers-files :prompt "Find outline path: " :query-prefix "outline-path:"))

(provide 'org-ql-find)

;;; org-ql-find.el ends here
