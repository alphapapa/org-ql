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

;;;; Variables

;;;; Functions

(defun org-ql-test-insert-result ()
  "FIXME: docstring"
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (correct-sexp-p (pcase (car sexp)
                              ('org-ql 'org-ql)
                              ('org-ql--query-preamble 'query-preamble)
                              ('org-ql--pre-process-query t)
                              (_ nil)))
            (result (pcase (car sexp)
                      ('org-ql (org-ql-test--format-result--ql sexp))
                      ('org-ql--query-preamble (org-ql-test--format-result--query-preamble sexp))
                      ('org-ql--pre-process-query (format "'%S" (eval sexp)))
                      (_ nil))))
      (insert " :to-equal " result)
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
            (correct-sexp-p (eq (car sexp) 'org-ql)))
      (progn
        (setf (car sexp) 'org-ql-agenda)
        (eval sexp))
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

;;;; Tests

(describe "org-ql"

  (before-all

    (defun org-ql-test-org-get-heading ()
      ;; For Org 9.0.5.
      (substring-no-properties (org-get-heading t t)))
    (defun org-ql-test-org-get-heading ()
      ;; For Org 9.1.9.
      (substring-no-properties (org-get-heading t t t t)))

    (setq test-buffer (find-file-noselect (concat default-directory "tests/data.org"))
          num-headings (with-current-buffer test-buffer
                         (org-with-wide-buffer
                          (goto-char (point-min))
                          (cl-loop while (re-search-forward org-heading-regexp nil t)
                                   sum 1)))))

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
            :to-equal '(unless (and (regexp "stringcondition1") (regexp "stringcond2")) (or (regexp "string1") (regexp "string2")))))

  (describe "Query compiling"
    ;; Okay, so it's not really "compiling," but it sounds fancy.  :)

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
                          (category)
                          :action (org-ql-test-org-get-heading)))
                :to-equal num-headings))
      (org-ql-it "with a category"
        (expect (org-ql test-buffer
                  (category "ambition")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(clocked)"
      (org-ql-it "without arguments"
        (expect (org-ql test-buffer
                  (clocked)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language")))
      (org-ql-it ":from a timestamp"
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ":to a timestamp"
        (expect (org-ql test-buffer
                  (clocked :to "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :to "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ":on a date"
        (expect (org-ql test-buffer
                  (clocked :on "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :on "2018-12-02")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it "within a range (:from and :to)"
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-04" :to "2018-12-11")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-06" :to "2018-12-11")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil)
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-01" :to "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil)))

    (describe "(closed)"
      (org-ql-it "without arguments"
        (expect (org-ql test-buffer
                  (closed)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language")))
      (org-ql-it "="
        (expect (org-ql test-buffer
                  (closed = "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed = "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it "<"
        ;; TODO: Figure out why these tests take about 8 times longer than the other comparators in the (closed) tests.
        (expect (org-ql test-buffer
                  (closed < "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed < "2017-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ">"
        (expect (org-ql test-buffer
                  (closed > "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed > "2019-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ">="
        (expect (org-ql test-buffer
                  (closed >= "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed >= "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed >= "2017-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it "<="
        (expect (org-ql test-buffer
                  (closed <= "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil)
        (expect (org-ql test-buffer
                  (closed <= "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed <= "2017-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))))

    (describe "(deadline)"
      (org-ql-it "without arguments"
        (expect (org-ql test-buffer
                  (deadline)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs")))
      (org-ql-it "="
        (expect (org-ql test-buffer
                  (deadline = "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline = "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it "<"
        (expect (org-ql test-buffer
                  (deadline < "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline < "2017-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ">"
        ;; TODO: Figure out why these tests take much longer than e.g. the (deadline <) tests.
        (expect (org-ql test-buffer
                  (deadline > "2017-07-04 00:00")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline > "2019-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ">="
        (expect (org-ql test-buffer
                  (deadline >= "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline >= "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline >= "2017-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease"))
        (expect (org-ql test-buffer
                  (deadline >= "2018-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it "<="
        (expect (org-ql test-buffer
                  (deadline <= "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil)
        (expect (org-ql test-buffer
                  (deadline <= "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline <= "2018-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))))

    (org-ql-it "(done)"
      (expect (org-ql test-buffer
                (done)
                :action (org-ql-test-org-get-heading))
              :to-equal '("Learn universal sign language")))

    (describe "(regexp)"
      (org-ql-it "with 1 argument"
        (expect (org-ql test-buffer
                  (regexp "Take over")
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))
      (org-ql-it "with 2 arguments"
        (expect (org-ql test-buffer
                  (regexp "Take over" "pizza")
                  :sort todo
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Order a pizza" "Get haircut")))
      (org-ql-it "with a plain string"
        (expect (org-ql test-buffer
                  "Take over"
                  :sort todo
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))
      (org-ql-it "with two plain strings"
        (expect (org-ql test-buffer
                  (or "Take over" "pizza")
                  :sort todo
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Order a pizza" "Get haircut"))))

    (describe "(todo)"
      (org-ql-it "without arguments"
        ;; FIXME: This returns an item that is done, which is incorrect.
        (expect (org-ql test-buffer
                  (todo)
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony")))
      (org-ql-it "with 1 argument"
        ;; FIXME: Figure out why this takes >10x longer than the other (todo) tests, according to Buttercup.
        (expect (org-ql test-buffer
                  (todo "WAITING")
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Visit the moon")))
      (org-ql-it "with 2 arguments"
        (expect (org-ql test-buffer
                  (todo "WAITING" "SOMEDAY")
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Visit the moon" "Rewrite Emacs in Common Lisp" "Write a symphony"))))

    (describe "(ts)"
      (org-ql-it "without arguments"
        (expect (org-ql test-buffer
                  (ts)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))
      (org-ql-it ":from a timestamp"
        ;; TODO: Figure out why these take longer than the other (ts) tests.
        (expect (org-ql test-buffer
                  (ts :from "2017-01-01")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :from "2019-06-08")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (org-ql-it ":to a timestamp"
        (expect (org-ql test-buffer
                  (ts :to "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :to "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Skype with president of Antarctica")))
      (org-ql-it ":on a timestamp"
        (expect (org-ql test-buffer
                  (ts :on "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :on "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil)))))

;;; org-ql.el ends here
