;;; org-ql-sort.el --- Library for sorting org-elements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: 

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

;; This library provides tools for sorting `org-element' items.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'org-element)

;;;; Variables

(defvar org-ql-sorters nil
  "Alist of sort functions defined with `org-ql-sort-define-sorter'.
Maps sorters' short names to function symbols.")

;;;; Macros

(defmacro org-ql-sort-define-sorter (name docstring &rest body)
  "FIXME: Docstring."
  (let ((fn-symbol (intern (concat "org-ql-sort--sort-by-" (symbol-name name)))))
    (map-put org-ql-sorters name fn-symbol)
    `(defun ,fn-symbol (a b)
       ,docstring
       ,@body)))

;;;; Functions




(provide 'org-ql-sort)
;;; org-ql-sort.el ends here
