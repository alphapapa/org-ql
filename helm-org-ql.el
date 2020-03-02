;;; helm-org-ql.el --- Helm commands for org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library includes Helm commands for `org-ql'.  Note that Helm
;; is not declared as a package dependency, so this does not cause
;; Helm to be installed.  In the future, this file may have its own
;; package recipe, which would allow it to be installed separately and
;; declare a dependency on Helm.

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

(eval-and-compile

  (eval-when-compile
    ;; Require these when compiling, but not necessarily on load.
    (require 'org)

    (require 'org-ql)
    (require 'org-ql-search))

;;;; Compatibility

  ;; Declare Helm functions since Helm may not be installed.
  (declare-function helm "ext:helm")
  (declare-function helm-run-after-exit "ext:helm")
  (declare-function helm-window "ext:helm-lib")
  (declare-function helm-buffer-get "ext:helm-lib")
  (declare-function helm-make-source "ext:helm-source")
  (declare-function helm-org-goto-marker "ext:helm-org")

  ;; Silence byte-compiler about variables.
  (defvar helm-map)
  (defvar helm-pattern)
  (defvar helm-input-idle-delay)

  (when (require 'helm nil 'noerror)

    ;; Requirements.
    (require 'helm-org)

;;;; Variables

    (defvar helm-org-ql-map
      (let ((map (make-sparse-keymap))
            (mappings '("C-x C-s" helm-org-ql-save)))
        (cl-loop for (key fn) on mappings by #'cddr
                 do (define-key map (kbd key) fn))
        (make-composed-keymap map helm-map))
      "Keymap for `helm-org-ql' sessions.
Based on `helm-map'.")

    (defvar helm-source-org-ql-views
      (helm-make-source "Org QL Views" 'helm-source-sync
        :candidates (lambda ()
                      (->> org-ql-views
                           (-map #'car)
                           (-sort #'string<)))
        :action (list (cons "Show view" #'org-ql-view)))
      "Helm source for `org-ql-views'.")

    (defvar-local helm-org-ql-buffers-files nil
      "Used for `helm-org-ql-save'.")

;;;; Customization

    (defgroup helm-org-ql nil
      "Options for `helm-org-ql'."
      :group 'org-ql)

    (defcustom helm-org-ql-reverse-paths t
      "Whether to reverse Org outline paths in `helm-org-ql' results."
      :type 'boolean)

    (defcustom helm-org-ql-input-idle-delay 0.25
      "Seconds to wait after typing stops before running query."
      :type 'number)

    (defcustom helm-org-ql-actions
      (list (cons "Show heading in source buffer" 'helm-org-ql-show-marker)
            (cons "Show heading in indirect buffer" 'helm-org-ql-show-marker-indirect))
      "Alist of actions for `helm-org-ql' commands."
      :type '(alist :key-type (string :tag "Description")
                    :value-type (function :tag "Command")))

;;;; Commands

;;;###autoload
    (cl-defun helm-org-ql (buffers-files
                           &key (boolean 'and) (name "helm-org-ql"))
      "Display results in BUFFERS-FILES for an `org-ql' non-sexp query using Helm.
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
            (helm-input-idle-delay helm-org-ql-input-idle-delay))
        (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
              :sources (helm-org-ql-source buffers-files :name name))))

;;;###autoload
    (defun helm-org-ql-agenda-files ()
      "Search agenda files with `helm-org-ql', which see."
      (interactive)
      (helm-org-ql (org-agenda-files) :name "Org Agenda Files"))

;;;###autoload
    (defun helm-org-ql-org-directory ()
      "Search Org files in `org-directory' with `helm-org-ql'."
      (interactive)
      (helm-org-ql (org-ql-search-directories-files)
                   :name "Org Directory Files"))

    (defun helm-org-ql-show-marker (marker)
      "Show heading at MARKER."
      (interactive)
      ;; This function is necessary because `helm-org-goto-marker' calls
      ;; `re-search-backward' to go backward to the start of a heading,
      ;; which, when the marker is already at the desired heading, causes
      ;; it to go to the previous heading.  I don't know why it does that.
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-show-entry))

    (defun helm-org-ql-show-marker-indirect (marker)
      "Show heading at MARKER with `org-tree-to-indirect-buffer'."
      (interactive)
      (helm-org-ql-show-marker marker)
      (org-tree-to-indirect-buffer))

    (defun helm-org-ql-save ()
      "Show `helm-org-ql' search in an `org-ql-search' buffer."
      (interactive)
      (let ((buffers-files (with-current-buffer (helm-buffer-get)
                             helm-org-ql-buffers-files))
            (query (org-ql--plain-query helm-pattern)))
        (helm-run-after-exit #'org-ql-search buffers-files query)))

;;;###autoload
    (defun helm-org-ql-views ()
      "Show an `org-ql' view selected with Helm."
      (interactive)
      (helm :sources helm-source-org-ql-views))

;;;; Functions

    (cl-defun helm-org-ql-source (buffers-files &key (name "helm-org-ql"))
      "Return Helm source named NAME that searches BUFFERS-FILES with `helm-org-ql'."
      ;; Expansion of `helm-build-sync-source' macro.
      (helm-make-source name 'helm-source-sync
        :candidates (lambda ()
                      (let* ((query (org-ql--plain-query helm-pattern))
                             (window-width (window-width (helm-window))))
                        (when query
                          (with-current-buffer (helm-buffer-get)
                            (setq helm-org-ql-buffers-files buffers-files))
                          (ignore-errors
                            ;; Ignore errors that might be caused by partially typed queries.
                            (org-ql-select buffers-files query
                              :action `(helm-org-ql--heading ,window-width))))))
        :match #'identity
        :fuzzy-match nil
        :multimatch nil
        :nohighlight t
        :volatile t
        :keymap helm-org-ql-map
        :action helm-org-ql-actions))

    (defun helm-org-ql--heading (window-width)
      "Return string for Helm for heading at point.
WINDOW-WIDTH should be the width of the Helm window."
      (font-lock-ensure (point-at-bol) (point-at-eol))
      ;; TODO: It would be better to avoid calculating the prefix and width
      ;; at each heading, but there's no easy way to do that once in each
      ;; buffer, unless we manually called `org-ql' in each buffer, which
      ;; I'd prefer not to do.  Maybe I should add a feature to `org-ql' to
      ;; call a setup function in a buffer before running queries.
      (let* ((prefix (concat (buffer-name) ":"))
             (width (- window-width (length prefix)))
             (heading (org-get-heading t))
             (path (-> (org-get-outline-path)
                       (org-format-outline-path width nil "")
                       (org-split-string "")))
             (path (if helm-org-ql-reverse-paths
                       (concat heading "\\" (s-join "\\" (nreverse path)))
                     (concat (s-join "/" path) "/" heading))))
        (cons (concat prefix path) (point-marker))))))

;;;; Footer

(provide 'helm-org-ql)

;;; helm-org-ql.el ends here
