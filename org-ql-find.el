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

(defcustom org-ql-find-snippet-function #'org-ql-find--snippet-simple
  ;; TODO: I'd like to make the -regexp one the default, but with
  ;; default Emacs completion affixation, it can sometimes be a bit
  ;; slow, and I don't want that to be a user's first impression.  It
  ;; may be possible to further optimize the -regexp one so that it
  ;; can be used by default.  In the meantime, the -simple one seems
  ;; fast enough for general use.
  "Function used to annotate results in `org-ql-find'.
Function is called at entry beginning.  (When set to
`org-ql-find--snippet-regexp', it is called with a regexp
matching plain query tokens.)"
  :type '(choice (function-item :tag "Show context around search terms" org-ql-find--snippet-regexp)
                 (function-item :tag "Show first N characters" org-ql-find--snippet-simple)
                 (function :tag "Custom function")))

(defcustom org-ql-find-snippet-length 51
  "Size of snippets of entry content to include in `org-ql-find' annotations.
Only used when `org-ql-find-snippet-function' is set to
`org-ql-find--snippet-regexp'."
  :type 'integer)

(defcustom org-ql-find-snippet-minimum-token-length 3
  "Query tokens shorter than this many characters are ignored.
That is, they are not included when gathering entry snippets.
This avoids too-small tokens causing performance problems."
  :type 'integer)

(defcustom org-ql-find-snippet-prefix nil
  "String prepended to snippets.
For an experience like `org-rifle', use a newline."
  :type '(choice (const :tag "None (shown on same line)" nil)
                 (const :tag "New line (shown under heading)" "\n")
                 string))

(defface org-ql-find-snippet '((t (:inherit font-lock-comment-face)))
  "Snippets.")

;;;; Functions

;;;###autoload
(cl-defun org-ql-find (buffers-files &key query-prefix query-filter
                                     (prompt "Find entry: "))
  "Go to an Org entry in BUFFERS-FILES selected by searching entries with `org-ql'.
Interactively, with universal prefix, select multiple buffers to
search with completion.

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
        (window-width (window-width))
        query-tokens snippet-regexp)
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
                          (while-no-input
                            ;; Using `while-no-input' here doesn't make it as
                            ;; responsive as, e.g. Helm while typing, but it seems to
                            ;; help a little when using the org-rifle-style snippets.
                            (or (snippet (gethash candidate table)) "")))
                (snippet (marker)
                         (org-with-point-at marker
                           (or (funcall org-ql-find-snippet-function snippet-regexp)
                               (org-ql-find--snippet-simple))))
                (group (candidate transform)
                       (pcase transform
                         (`nil (buffer-name (marker-buffer (gethash candidate table))))
                         (_ candidate)))
                (try (string _table _pred point &optional _metadata)
                     (cons string point))
                (all (string table pred _point)
                     (all-completions string table pred))
                (collection (str _pred flag)
                            (when query-prefix
                              (setf str (concat query-prefix str)))
                            (pcase flag
                              ('metadata (list 'metadata
                                               (cons 'group-function #'group)
                                               (cons 'affixation-function #'affix)
                                               (cons 'annotation-function #'annotate)))
                              (`t (unless (string-empty-p str)
                                    (when query-filter
                                      (setf str (funcall query-filter str)))
                                    (pcase org-ql-find-snippet-function
                                      ('org-ql-find--snippet-regexp
                                       (setf query-tokens
                                             ;; Remove any tokens that specify predicates or are too short.
                                             (--select (not (or (string-match-p (rx bos (1+ (not (any ":"))) ":") it)
                                                                (< (length it) org-ql-find-snippet-minimum-token-length)))
                                                       (split-string str nil t (rx space)))
                                             snippet-regexp
                                             (when query-tokens
                                               ;; Limiting each context word to 15 characters
                                               ;; prevents excessively long, non-word strings
                                               ;; from ending up in snippets, which can
                                               ;; adversely affect performance.
                                               (rx-to-string `(seq (optional (repeat 1 3 (repeat 1 15 (not space)) (0+ space)))
                                                                   bow (or ,@query-tokens) (0+ (not space))
                                                                   (optional (repeat 1 3 (0+ space) (repeat 1 15 (not space))))))))))
                                    (org-ql-select buffers-files (org-ql--query-string-to-sexp str)
                                      :action #'action))))))
      ;; NOTE: It seems that the `completing-read' machinery can call,
      ;; abort, and re-call the collection function while the user is
      ;; typing, which can interrupt the machinery Org uses to prepare
      ;; an Org buffer when an Org file is loaded.  This results in,
      ;; e.g. the buffer being left in fundamental-mode, unprepared to
      ;; be used as an Org buffer, which breaks many things and is
      ;; very confusing for the user.  Ideally, of course, we would
      ;; solve this in `org-ql-select', and we already attempt to, but
      ;; that function is called by the `completing-read' machinery,
      ;; which interrupts it, so we must work around this problem by
      ;; ensuring all of the BUFFERS-FILES are loaded and initialized
      ;; before calling `completing-read'.
      (unless (listp buffers-files)
        ;; Since we map across this argument, we ensure it's a list.
        (setf buffers-files (list buffers-files)))
      (mapc #'org-ql--ensure-buffer buffers-files)
      (let* ((completion-styles '(org-ql-find))
             (completion-styles-alist (list (list 'org-ql-find #'try #'all "Org QL Find")))
             (selected (completing-read prompt #'collection nil))
             (marker (gethash selected table)))
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (display-buffer (current-buffer))
          (select-window (get-buffer-window (current-buffer)))
          (run-hook-with-args 'org-ql-find-goto-hook))))))

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

(defun org-ql-find--snippet-simple (&optional _regexp)
  "Return a snippet of the current entry.
Returns up to `org-ql-find-snippet-length' characters."
  (org-end-of-meta-data t)
  (unless (org-at-heading-p)
    (let ((end (min (+ (point) org-ql-find-snippet-length)
                    (org-entry-end-position))))
      (concat org-ql-find-snippet-prefix
              (truncate-string-to-width
               (replace-regexp-in-string "\n" " " (buffer-substring (point) end)
                                         t t)
               50 nil nil t)))))

(defun org-ql-find--snippet-regexp (regexp)
  "Return a snippet of the current entry's matches for REGEXP."
  ;; REGEXP may be nil if there are no qualifying tokens in the query.
  (when regexp
    (org-end-of-meta-data t)
    (unless (org-at-heading-p)
      (let* ((end (org-entry-end-position))
             (snippets (cl-loop while (re-search-forward regexp end t)
                                concat (match-string 0) concat "…"
                                do (goto-char (match-end 0)))))
        (unless (string-empty-p snippets)
          (concat org-ql-find-snippet-prefix
                  (replace-regexp-in-string (rx (1+ "\n")) "  " snippets t t)))))))

(provide 'org-ql-find)

;;; org-ql-find.el ends here
