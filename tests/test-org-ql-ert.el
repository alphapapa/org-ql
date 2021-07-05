;;; test-org-ql-ert.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

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

;;

;;; Code:

;;;; Requirements

(require 'ert)

(require 'org-ql)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(ert-deftest org-ql--normalize-query ()
  (should (equal (org-ql--normalize-query '(ts-active :on "2019-01-01"))
                 `(ts :type active
                      :from ,(ts-apply :hour 0 :minute 0 :second 0 (ts-parse "2019-01-01"))
                      :to ,(ts-apply :hour 23 :minute 59 :second 59 (ts-parse "2019-01-01"))))))


;;;; Footer

(provide 'test-org-ql-ert)

;;; test-org-ql-ert.el ends here
