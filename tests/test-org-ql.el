;;; org-ql.el --- Tests for org-ql                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Adam Porter

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

(require 'org-ql)
(require 'org-ql-agenda)

;;;; Variables



;;;; Functions

;;;; Tests

(describe "org-ql"
  (before-all
    (setq test-buffer (find-file-noselect (concat default-directory "tests/data.org"))))

  (describe "Can select entries clocked..."
    (it "...at any time"
      (expect (org-ql test-buffer
                (clocked)
                :action (org-get-heading t t))
              :to-equal '("Clocked from 2018-12-01 00:00 to 2018-12-10 23:59")))
    (it "...after (:from) a timestamp"
      (expect (org-ql test-buffer
                (clocked :from "2018-11-10")
                :action (org-get-heading t t))
              :to-equal '("Clocked from 2018-12-01 00:00 to 2018-12-10 23:59")))
    (it "...up to (:to) a timestamp"
      (expect (org-ql test-buffer
                (clocked :to "2018-12-30")
                :action (org-get-heading t t))
              :to-equal '("Clocked from 2018-12-01 00:00 to 2018-12-10 23:59")))
    (it "...on a date"
      (expect (org-ql test-buffer
                (clocked :on "2018-12-02")
                :action (org-get-heading t t))
              :to-equal '("Clocked from 2018-12-01 00:00 to 2018-12-10 23:59")))
    (it "...within a date range"
      (expect (org-ql test-buffer
                (clocked :from "2018-12-03" :to "2018-12-11")
                :action (org-get-heading t t))
              :to-equal '("Clocked from 2018-12-01 00:00 to 2018-12-10 23:59")))))

;;; org-ql.el ends here
