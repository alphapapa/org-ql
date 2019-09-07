;;; org-ql-item.el --- Item parsing for org-ql results  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

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

;;

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'org-element)

(require 'dash)

(require 'org-ql-macros)

;;;; Structs

(org-ql-defstruct org-ql-item
  buffer marker beg category level todo priority heading tags deadline-element scheduled-element properties
  (deadline-ts nil :accessor-init (when-let* ((element (org-ql-item-deadline-element struct)))
                                    (ts-parse-org-element element)))
  (scheduled-ts nil :accessor-init (when-let* ((element (org-ql-item-scheduled-element struct)))
                                     (ts-parse-org-element element))))

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun org-ql-item-at ()
  "Return `org-ql-item' for entry at point."
  (-let* (((_ (&plist :begin :level :raw-value :priority :tags :todo-keyword :deadline :scheduled :CATEGORY))
           (org-element-headline-parser (line-end-position))))
    (make-org-ql-item :beg begin
                      :category CATEGORY
                      :level level
                      :todo todo-keyword
                      :priority (when priority
                                  (char-to-string priority))
                      :heading raw-value
                      :tags tags
                      :deadline-element deadline
                      :scheduled-element scheduled)))

;;;;; Sorting

(defun org-ql-item--sort-planning (items)
  "Return ITEMS sorted by planning date."
  (cl-flet ((planning-ts (item)
                         (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))))
    (sort items (lambda (a b)
                  (let ((a-ts (planning-ts a))
                        (b-ts (planning-ts b)))
                    (cond ((and a-ts b-ts)
                           (ts< a-ts b-ts))
                          (a-ts t)
                          (b-ts nil)))))))

(defun org-ql-item--sort-todo (items)
  "Return ITEMS sorted by to-do keyword, in to-do keyword order.
Uses `org-todo-keywords', which does not include buffer-local
keywords."
  (cl-flet ((keyword (string)
                     ;; Return keyword without parenthesized options.
                     (unless (string= string "|")
                       (if (string-match (rx (group (minimal-match (1+ anything)))
                                             "(" (1+ anything) ")")
                                         string)
                           (match-string 1 string)
                         string))))
    (let ((keywords (cl-loop with non-done-keywords with done-keywords
                             for (_type . keywords) in (reverse org-todo-keywords)
                             for non-done = (->> keywords
                                                 (--take-while (not (string= it "|")))
                                                 (-map #'keyword))
                             for done = (-map #'keyword
                                              (-slice keywords
                                                      (1+ (or (--find-index (string= it "|")
                                                                            keywords)
                                                              -1))))
                             do (progn
                                  (setf non-done-keywords (append non-done non-done-keywords))
                                  (setf done-keywords (append done done-keywords)))
                             finally return (append non-done-keywords done-keywords))))
      (-sort (lambda (a b)
               (cond ((and (org-ql-item-todo a)
                           (org-ql-item-todo b))
                      (< (or (cl-position (org-ql-item-todo a) keywords :test #'string=) 0)
                         (or (cl-position (org-ql-item-todo b) keywords :test #'string=) 0)))
                     ((org-ql-item-todo a) t)
                     ((org-ql-item-todo b) nil)))
             items))))

;;;; Footer

(provide 'org-ql-item)

;;; org-ql-item.el ends here
