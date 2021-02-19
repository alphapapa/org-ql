;;; org-bills-due.el

;;;; Initialize package system

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Set load-path, loading org-ql from ~/src/emacs/org-ql (adjust as necessary)
(setq load-path (append (list "~/src/emacs/org-ql"
                              "~/.emacs.d/load-path")
                        load-path))

;;;; Load packages

;; Built-in
(require 'cl-lib)
(require 'subr-x)

(require 'org)
(require 'org-habit)
(require 'org-agenda)

;; From MELPA
(require 'dash)
(require 's)

;; org-ql
(require 'org-ql)
(require 'org-ql-agenda)

;;;; Main

(-if-let* ((header "Bills due within 3 days")
           (items (org-ql "~/org/main.org"
                    (and (deadline 3)
                         (tags "bills"))
                    :action (org-get-heading 'no-tags 'no-todo)))
           (string (concat "<ul>"
                           (s-join ""
                                   (--map (concat "<li>" it "</li>")
                                          items))
                           "</ul>")))
    (progn
      ;; Send notification
      (call-process "notify-send" nil 0 nil
                    (format "<b>%s</b>" header)
                    string)
      ;; Also output to STDOUT
      (princ (format "%s:\n%s" header (s-join "\n" titles))))
  ;; No bills due
  (kill-emacs 1))
