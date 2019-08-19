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

(require 'buttercup)

(require 'org-ql)

;;;; Variables

;;;; Functions

(defun org-ql-test-insert-result ()
  "FIXME: docstring"
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (correct-sexp-p (pcase (car sexp)
                              ('org-ql 'org-ql)
                              ('org-ql-expect t)
                              ('org-ql--query-preamble 'query-preamble)
                              ('org-ql--pre-process-query t)
                              (_ nil)))
            (result (pcase sexp
                      (`(org-ql-expect ,args) (org-ql-test--format-result--ql `(org-ql test-buffer
                                                                                 ,@args
                                                                                 :action (org-ql-test-org-get-heading))))
                      (`(org-ql . _) (org-ql-test--format-result--ql sexp))
                      (`(org-ql--query-preamble  . _) (org-ql-test--format-result--query-preamble sexp))
                      (`(org-ql--pre-process-query . _) (format "'%S" (eval sexp)))
                      (_ nil))))
      (progn
        (backward-char 1)
        (insert "\n'" result))
    (user-error "Point must be after an `org-ql' form")))

(defun org-ql-test--format-result--ql (sexp)
  (let* ((value (eval sexp))
         (prefix (if (and value (listp value))
                     "'"
                   "")))
    (format "%S" value)))

(defun org-ql-test--format-result--query-preamble (sexp)
  (-let* (((query preamble) (eval sexp))
          (preamble (when preamble
                      (xr preamble 'brief))))
    (format "`(%S ,(rx %S))" query preamble)))

(defun org-ql-test-show-result ()
  "Show `org-ql-agenda' for `org-ql' form."
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (sexp (pcase sexp
                    (`(org-ql . ,_) (setf (car sexp) 'org-ql-agenda))
                    (`(org-ql-expect ,ql-args . ,_) (setf sexp `(org-ql-agenda test-buffer
                                                                  ,@ql-args)))
                    (_ nil))))

      (eval sexp)
    (user-error "Point must be after an `org-ql' form")))

;;;; Macros

(defmacro org-ql-it (description &rest body)
  "Expand to two specs, one of which tests with preambles and the other without.
Based on Buttercup macro `it'."
  (declare (indent 1) (debug (&define sexp def-body)))
  (if body
      `(progn
         (buttercup-it ,(concat description " (preamble)   ")
                       (lambda ()
                         (let ((org-ql-use-preamble t))
                           ,@body)))
         (buttercup-it ,(concat description " (no preamble)")
                       (lambda ()
                         (let ((org-ql-use-preamble nil))
                           ,@body))))
    `(buttercup-xit ,description)))

(defmacro org-ql-expect (ql-args results)
  "Expand to `expect' test form that expects QL-ARGS to equal RESULTS.
RESULTS should be a list of strings as returned by
`org-ql-test-org-get-heading'."
  (declare (indent defun))
  `(expect (org-ql test-buffer
             ,@ql-args
             :action (org-ql-test-org-get-heading))
           :to-equal ,results))

(defmacro org-ql-then (&rest body)
  "Wrap BODY, setting `ts-now' to return timestamp at 2017-07-05 12:00:00."
  ;; The same time used in `org-super-agenda--test-date', which is where the test data comes from.
  (declare (indent defun))
  `(cl-letf (((symbol-function 'ts-now)
              (lambda ()
                (make-ts :year 2017 :month 7 :day 5 :hour 12 :minute 0 :second 0))))
     ,@body))

;;;; Tests

(describe "org-ql"

  (before-all

    (if (version< (org-version) "9.1")
        (defun org-ql-test-org-get-heading ()
          ;; For Org 9.0.5.
          (substring-no-properties (org-get-heading t t)))
      (defun org-ql-test-org-get-heading ()
        ;; For Org 9.1.9.
        (substring-no-properties (org-get-heading t t t t))))

    (setq test-buffer (find-file-noselect (concat default-directory "tests/data.org"))
          ;; For manual testing:
          ;; test-buffer (find-file-noselect "data.org")
          num-headings (with-current-buffer test-buffer
                         (org-with-wide-buffer
                          (goto-char (point-min))
                          (cl-loop while (re-search-forward org-heading-regexp nil t)
                                   sum 1)))))

  (describe "Query functions/macros"

    (it "org-ql"
      (expect (length (org-ql test-buffer
                        (category)
                        :sort deadline))
              :to-equal num-headings))
    (it "org-ql-select"
      (expect (length (org-ql-select test-buffer
                        '(category)
                        :sort 'deadline))
              :to-equal num-headings))
    (it "org-ql-query"
      (expect (length (org-ql-query :select 'element
                                    :from test-buffer
                                    :where '(category)
                                    :order-by 'date))
              :to-equal num-headings)))

  (it "Query pre-processing"
    (expect (org-ql--pre-process-query '(and "string1" "string2"))
            :to-equal '(and (regexp "string1") (regexp "string2")))
    (expect (org-ql--pre-process-query '(or "string1" "string2"))
            :to-equal '(or (regexp "string1") (regexp "string2")))
    (expect (org-ql--pre-process-query '(and (todo "TODO")
                                             (or "string1" "string2")))
            :to-equal '(and (todo "TODO") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--pre-process-query '(when (todo "TODO")
                                          (or "string1" "string2")))
            :to-equal '(when (todo "TODO") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--pre-process-query '(when "string-cond1"
                                          (or "string1" "string2")))
            :to-equal '(when (regexp "string-cond1") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--pre-process-query '(when (and "string-cond1" "string-cond2")
                                          (or "string1" "string2")))
            :to-equal '(when (and (regexp "string-cond1") (regexp "string-cond2")) (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--pre-process-query '(unless (and "stringcondition1" "stringcond2")
                                          (or "string1" "string2")))
            :to-equal '(unless (and (regexp "stringcondition1") (regexp "stringcond2")) (or (regexp "string1") (regexp "string2"))))

    (expect (org-ql--pre-process-query '(or (ts-active :on "2019-01-01")
                                            (ts-a :on "2019-01-01")
                                            (ts-inactive :on "2019-01-01")
                                            (ts-i :on "2019-01-01")))
            :to-equal '(or (ts :type active :on "2019-01-01")
                           (ts :type active :on "2019-01-01")
                           (ts :type inactive :on "2019-01-01")
                           (ts :type inactive :on "2019-01-01"))))

  (describe "Query optimizing"

    ;; TODO: Other predicates.

    (describe "(level)"
      (it "with a number"
        (expect (org-ql--query-preamble '(level 2))
                :to-equal `(t ,(rx bol (repeat 2 "*") " "))))
      (it "with two numbers"
        (expect (org-ql--query-preamble '(level 2 4))
                :to-equal `(t ,(rx bol (repeat 2 4 "*") " "))))
      (it "<"
        (expect (org-ql--query-preamble '(level < 3))
                :to-equal `(t ,(rx bol (repeat 1 2 "*") " "))))
      (it "<="
        (expect (org-ql--query-preamble '(level <= 2))
                :to-equal `(t ,(rx bol (repeat 1 2 "*") " "))))
      (it ">"
        (expect (org-ql--query-preamble '(level > 2))
                :to-equal `(t ,(rx bol (>= 3 "*") " "))))
      (it ">="
        (expect (org-ql--query-preamble '(level >= 2))
                :to-equal `(t ,(rx bol (>= 2 "*") " "))))))

  (describe "Query results"

    ;; TODO: Other predicates.

    (describe "(category)"

      (org-ql-it "without arguments"
        (expect (length (org-ql test-buffer
                          (category)))
                :to-equal num-headings))

      (org-ql-it "with a category"
        (org-ql-expect ((category "ambition"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(children)"
      (org-ql-it "without arguments"
        (org-ql-expect ((children))
          '("Test data" "Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Recurring" "Ideas" "Code" "Misc")))
      (org-ql-it "with sub-query"
        (org-ql-expect ((children (todo "CHECK")))
          '("Recurring")))
      (org-ql-it "with grandchildren query"
        ;; It's really cool how this works.  It's so simple.
        (org-ql-expect ((children (children "moon")))
          '("Test data" "Take over the universe"))))

    (describe "(descendants)"
      (org-ql-it "without arguments"
        (org-ql-expect ((descendants))
          '("Test data" "Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Recurring" "Ideas" "Code" "Misc")))
      (org-ql-it "with sub-query"
        (org-ql-expect ((descendants (todo "CHECK")))
          '("Test data" "Recurring")))
      (org-ql-it "with granddescendants query"
        (org-ql-expect ((descendants (descendants "moon")))
          '("Test data" "Take over the universe" "Take over the moon" "Code"))))

    (describe "(clocked)"

      (org-ql-it "without arguments"
        (org-ql-expect ((clocked))
          '("Learn universal sign language")))

      (org-ql-it "with a number"
        (org-ql-then
          (org-ql-expect ((clocked 10))
            '("Learn universal sign language"))))

      (org-ql-it ":from a timestamp"
        (org-ql-expect ((clocked :from "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :from "2017-07-06"))
          nil))

      (org-ql-it ":to a timestamp"
        (org-ql-expect ((clocked :to "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :to "2017-07-04"))
          nil))

      (org-ql-it ":on a date"
        (org-ql-expect ((clocked :on "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :on "2018-12-02"))
          nil))

      (org-ql-it "within a range (:from and :to)"
        (org-ql-expect ((clocked :from "2017-07-04" :to "2018-12-11"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :from "2017-07-06" :to "2018-12-11"))
          nil)
        (org-ql-expect ((clocked :from "2017-07-01" :to "2017-07-04"))
          nil)))

    (describe "(closed)"

      (org-ql-it "without arguments"
        (org-ql-expect ((closed))
          '("Learn universal sign language")))

      (org-ql-it "with a number"
        (org-ql-then
          (org-ql-expect ((closed 10))
            '("Learn universal sign language"))))

      (org-ql-it ":on"
        (org-ql-expect ((closed :on "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((closed :on "2019-06-09"))
          nil))

      (org-ql-it ":from"
        (org-ql-expect ((closed :from "2017-07-04"))
          '("Learn universal sign language"))
        (org-ql-expect ((closed :from "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((closed :from "2017-07-06"))
          nil))

      (org-ql-it ":to"
        (org-ql-expect ((closed :to "2017-07-04"))
          nil)
        (org-ql-expect ((closed :to "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((closed :to "2017-07-06"))
          '("Learn universal sign language"))))

    (describe "(deadline)"

      (org-ql-it "without arguments"
        (org-ql-expect ((deadline))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs")))

      (org-ql-it "auto"
        (org-ql-then
          (org-ql-expect ((deadline auto))
            '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))))

      (org-ql-it "with a number"
        (org-ql-then
          (org-ql-expect ((deadline 2))
            '("Take over the world" "/r/emacs"))))

      (org-ql-it ":on"
        (org-ql-expect ((deadline :on "2017-07-05"))
          '("/r/emacs"))

        (org-ql-expect ((deadline :on "2019-06-09"))
          nil))

      (org-ql-it ":from"
        (org-ql-expect ((deadline :from "2017-07-04"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (org-ql-expect ((deadline :from "2017-07-05"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (org-ql-expect ((deadline :from "2017-07-06"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease"))
        (org-ql-expect ((deadline :from "2018-07-06"))
          nil))

      (org-ql-it ":to"
        (org-ql-expect ((deadline :to "2017-07-04"))
          nil)
        (org-ql-expect ((deadline :to "2017-07-05"))
          '("/r/emacs"))
        (org-ql-expect ((deadline :to "2018-07-06"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))))

    (org-ql-it "(done)"
      (org-ql-expect ((done))
        '("Learn universal sign language")))

    (org-ql-it "(habit)"
      (org-ql-expect ((habit))
        '("Practice leaping tall buildings in a single bound")))

    (describe "(planning)"

      (org-ql-it "without arguments"
        (org-ql-expect ((planning))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

      (org-ql-it "with a number"
        (org-ql-then
          (org-ql-expect ((planning 2))
            '("Take over the world" "Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

      (org-ql-it ":on"
        (org-ql-expect ((planning :on "2017-07-05"))
          '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :on "2019-06-09"))
          nil))

      (org-ql-it ":from"
        (org-ql-expect ((planning :from "2017-07-04"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :from "2017-07-05"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :from "2017-07-06"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease")))

      (org-ql-it ":to"
        (org-ql-expect ((planning :to "2017-07-04"))
          '("Skype with president of Antarctica"))
        (org-ql-expect ((planning :to "2017-07-05"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :to "2018-07-06"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

    (describe "(property)"

      ;; MAYBE: Add support for (property) without arguments.
      ;; (org-ql-it "without arguments"
      ;;   (org-ql-expect ((property))))

      (org-ql-it "with a property"
        (org-ql-expect ((property "agenda-group"))
          '("Take over the universe" "Spaceship lease" "Recurring" "Write a symphony")))

      (org-ql-it "with a property and a value"
        (org-ql-expect ((property "agenda-group" "plans"))
          '("Take over the universe" "Write a symphony"))))

    (describe "(regexp)"

      (org-ql-it "with 1 argument"
        (org-ql-expect ((regexp "Take over")
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))

      (org-ql-it "with 2 arguments"
        (org-ql-expect ((regexp "Take over" "pizza")
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Order a pizza" "Get haircut")))

      (org-ql-it "with a plain string"
        (org-ql-expect ("Take over"
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))

      (org-ql-it "with two plain strings"
        (org-ql-expect ((or "Take over" "pizza")
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Order a pizza" "Get haircut"))))

    (describe "(scheduled)"

      (org-ql-it "without arguments"
        (org-ql-expect ((scheduled))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

      (org-ql-it "with a number"
        (org-ql-then
          ;; Using -1 is the easiest way to exclude some results but not all for testing this.
          (org-ql-expect ((scheduled -1))
            '("Skype with president of Antarctica"))))

      (org-ql-it ":on"
        (org-ql-expect ((scheduled :on "2017-07-05"))
          '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :on "2019-06-09"))
          nil))

      (org-ql-it ":from"
        (org-ql-expect ((scheduled :from "2017-07-04"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :from "2017-07-05"))
          '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :from "2017-07-06"))
          nil))

      (org-ql-it ":to"
        (org-ql-expect ((scheduled :to "2017-07-04"))
          '("Skype with president of Antarctica"))
        (org-ql-expect ((scheduled :to "2017-07-05"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :to "2018-07-06"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

    (describe "(todo)"

      (org-ql-it "without arguments"
        (org-ql-expect ((todo)
                        :sort todo)
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with 1 argument"
        ;; FIXME: Figure out why this takes >10x longer than the other (todo)
        ;; tests, according to Buttercup.  Might just be GC, though.
        (org-ql-expect ((todo "WAITING")
                        :sort todo)
          '("Visit the moon")))

      (org-ql-it "with 2 arguments"
        (org-ql-expect ((todo "WAITING" "SOMEDAY")
                        :sort todo)
          '("Visit the moon" "Rewrite Emacs in Common Lisp" "Write a symphony"))))

    (describe "(tags)"

      (org-ql-it "without arguments"
        (org-ql-expect ((tags))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony"))
        (org-ql-expect ((not (tags)))
          '("Test data" "Recurring" "Sunrise/sunset" "Ideas" "Code" "Agenda examining" "Agenda censoring" "Auto grouping" "Auto categories" "Date" "Effort" "Misc" "let-plist" "Profiling")))

      (org-ql-it "with a tag"
        (org-ql-expect ((tags "Emacs"))
          '("/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags "Emacs")))
          '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony" "Code" "Agenda examining" "Agenda censoring" "Auto grouping" "Auto categories" "Date" "Effort" "Misc" "let-plist" "Profiling")))

      (org-ql-it "with 2 tags"
        (org-ql-expect ((tags "Emacs" "space"))
          '("Visit Mars" "Visit the moon" "/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags "Emacs" "space")))
          '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Take over the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony" "Code" "Agenda examining" "Agenda censoring" "Auto grouping" "Auto categories" "Date" "Effort" "Misc" "let-plist" "Profiling"))))

    (describe "(ts)"

      (describe "active"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts :type active))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          ;; TODO: Figure out why these take longer than the other (ts) tests.
          (org-ql-expect ((ts :from "2017-07-08" :type active))
            '("Take over the universe" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease"))
          (org-ql-expect ((ts :from "2019-06-08" :type active))
            nil))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2019-06-10" :type active))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04" :type active))
            '("Skype with president of Antarctica")))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05" :type active))
            '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2019-06-09" :type active))
            nil)))

      (describe "inactive"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts :type inactive))
            '("Test data" "Visit the moon" "Learn universal sign language" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          (org-ql-expect ((ts :from "2017-07-06" :type inactive))
            '("Visit the moon" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2019-06-08" :type inactive))
            nil))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2019-06-10" :type inactive))
            '("Test data" "Visit the moon" "Learn universal sign language" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04" :type inactive))
            'nil))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05" :type inactive))
            '("Test data" "Learn universal sign language"))
          (org-ql-expect ((ts :on "2019-06-09" :type inactive))
            nil)))

      (describe "both"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts))
            '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :type both))
            '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          ;; TODO: Figure out why these take longer than the other (ts) tests.
          (org-ql-expect ((ts :from "2017-07-05"))
            '("Test data" "Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2017-07-05" :type both))
            '("Test data" "Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2019-06-08"))
            nil)
          (org-ql-expect ((ts :from "2019-06-08" :type both))
            nil))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2017-07-06"))
            '("Test data" "Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-06" :type both))
            '("Test data" "Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04"))
            '("Skype with president of Antarctica"))
          (org-ql-expect ((ts :to "2017-07-04" :type both))
            '("Skype with president of Antarctica")))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05"))
            '("Test data" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2017-07-05" :type both))
            '("Test data" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2019-06-09"))
            nil)
          (org-ql-expect ((ts :on "2019-06-09" :type both))
            nil))))

    (describe "Compound queries"

      (org-ql-it "Tags and to-do"
        (org-ql-expect ((and (todo "SOMEDAY")
                             (tags "Emacs")))
          '("Rewrite Emacs in Common Lisp"))))))

;; Local Variables:
;; truncate-lines: t
;; End:

;;; org-ql.el ends here
