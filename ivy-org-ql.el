;;; ivy-org-ql.el --- Ivy commands for org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library includes Ivy commands for `org-ql'.  Note that Ivy
;; is not declared as a package dependency, so this does not cause
;; Ivy to be installed.  In the future, this file may have its own
;; package recipe, which would allow it to be installed separately and
;; declare a dependency on Ivy.

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

(eval-when-compile
  (require 'org-ql)
  (require 'org-ql-search))

;;;; Compatibility

;; Declare Ivy functions since Ivy may not be installed.
(declare-function ivy-read "ext:ivy")

;; Silence byte-compiler about variables.

;;;; Ivy

;; Everything inside `with-eval-after-load'.

;;;###autoload
(with-eval-after-load 'ivy

;;;;; Variables

  ;; TODO: ivy-org-ql-views command.

  ;; TODO: helm-org-ql-buffers-files

;;;;; Customization

  (defgroup ivy-org-ql nil
    "Options for `ivy-org-ql'."
    :group 'org-ql)

  (defcustom ivy-org-ql-reverse-paths t
    "Whether to reverse Org outline paths in `ivy-org-ql' results."
    :type 'boolean)

  (defcustom ivy-org-ql-dynamic-exhibit-delay-ms 250
    "Milliseconds to wait after typing stops before running query."
    :type 'integer)

;;;;; Commands

  (cl-defun ivy-org-ql (buffers-files
                        &key (boolean 'and) (name "ivy-org-ql"))
    "Display results in BUFFERS-FILES for an `org-ql' non-sexp query using Ivy.
Interactively, search the current buffer.  Note that this command
only accepts non-sexp, \"plain\" queries.

NOTE: Atoms in the query are turned into strings where
appropriate, which makes it unnecessary to type quotation marks
around words that are intended to be searched for as indepenent
strings.

All query tokens are wrapped in the operator BOOLEAN (default
`and'; with prefix, `or').

For example, this raw input:

    Emacs git

Is transformed into this query:

    (and \"Emacs\" \"git\")

However, quoted strings remain quoted, so this input:

    \"something else\" (tags \"funny\")

Is transformed into this query:

    (and \"something else\" (tags \"funny\"))"
    (interactive (list (current-buffer)))
    (let ((boolean (if current-prefix-arg 'or boolean))
          (ivy-dynamic-exhibit-delay-ms ivy-org-ql-dynamic-exhibit-delay-ms))
      (ivy-read (format "Query (boolean %s): " (-> boolean symbol-name upcase))
                (lambda (input)
                  (let ((query (org-ql--query-string-to-sexp input))
                        (window-width (window-width)))
                    (when query
                      (org-ql-select buffers-files query
                        :action 'ivy-org-ql--heading-cons))))
                :dynamic-collection t
                :action 'ivy-org-ql-show-marker)))

  (defun ivy-org-ql--heading-cons ()
    (font-lock-ensure (point-at-bol) (point-at-eol))
    ;; TODO: It would be better to avoid calculating the prefix and width
    ;; at each heading, but there's no easy way to do that once in each
    ;; buffer, unless we manually called `org-ql' in each buffer, which
    ;; I'd prefer not to do.  Maybe I should add a feature to `org-ql' to
    ;; call a setup function in a buffer before running queries.
    (let* ((prefix (concat (buffer-name) ":"))
           (heading (org-get-heading t))
           (path (-> (org-get-outline-path)
                   (org-format-outline-path nil nil "")
                   (org-split-string "")))
           (path (if ivy-org-ql-reverse-paths
                     (concat heading "\\" (s-join "\\" (nreverse path)))
                   (concat (s-join "/" path) "/" heading)))
           (str (concat prefix path)))
      ;; ivy doesn't support calling action function with a cons when dynamic-collection is on
      ;; see https://github.com/abo-abo/swiper/issues/2677
      ;; so we had to use text-property to store (point-marker)
      (put-text-property 0 1 'marker (point-marker) str)
      str))

  (defun ivy-org-ql-show-marker (cand)
    (org-ql-show-marker (get-text-property 0 'marker cand)))

  (defun ivy-org-ql-agenda-files ()
    "Search agenda files with `ivy-org-ql', which see."
    (interactive)
    (ivy-org-ql (org-agenda-files) :name "Org Agenda Files"))

  (defun ivy-org-ql-org-directory ()
    "Search Org files in `org-directory' with `ivy-org-ql'."
    (interactive)
    (ivy-org-ql (org-ql-search-directories-files)
                :name "Org Directory Files")))

;; FIXME: ivy-org-ql-save
;; (defun ivy-org-ql-save ()
;;   "Show `ivy-org-ql' search in an `org-ql-search' buffer."
;;   (interactive)
;;   (let ((buffers-files (with-current-buffer (ivy-buffer-get)
;;                          ivy-org-ql-buffers-files))
;;         (query (org-ql--query-string-to-sexp ivy-pattern)))
;;     (ivy-run-after-exit #'org-ql-search buffers-files query)))

;; FIXME: ivy-org-ql-views
;; (defun ivy-org-ql-views ()
;;   "Show an `org-ql' view selected with Ivy."
;;   (interactive)
;;   (ivy :sources ivy-source-org-ql-views))

;;;; Footer

(provide 'ivy-org-ql)

;;; ivy-org-ql.el ends here
