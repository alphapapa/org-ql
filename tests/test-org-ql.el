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
            (correct-sexp-p (eq (car sexp) 'org-ql))
            (value (eval sexp))
            (prefix (if (and value (listp value))
                        "'"
                      "")))
      (insert " :to-equal "
              prefix
              (format "%S" value))
    (user-error "Point must be after an `org-ql' form")))

(defun org-ql-test-show-result ()
  "Show `org-ql-agenda' for `org-ql' form."
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (correct-sexp-p (eq (car sexp) 'org-ql)))
      (progn
        (setf (car sexp) 'org-ql-agenda)
        (eval sexp))
    (user-error "Point must be after an `org-ql' form")))

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

  (describe "Predicates"

    (describe "(category)"
      (it "without arguments"
        (expect (length (org-ql test-buffer
                          (category)
                          :action (org-ql-test-org-get-heading)))
                :to-equal num-headings))
      (it "with a category"
        (expect (org-ql test-buffer
                  (category "ambition")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(clocked)"
      (it "without arguments"
        (expect (org-ql test-buffer
                  (clocked)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language")))
      (it ":from a timestamp"
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :from "2017-07-06")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ":to a timestamp"
        (expect (org-ql test-buffer
                  (clocked :to "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :to "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ":on a date"
        (expect (org-ql test-buffer
                  (clocked :on "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (clocked :on "2018-12-02")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it "within a range (:from and :to)"
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
      (it "without arguments"
        (expect (org-ql test-buffer
                  (closed)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language")))
      (it "="
        (expect (org-ql test-buffer
                  (closed = "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed = "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it "<"
        ;; TODO: Figure out why these tests take about 8 times longer than the other comparators in the (closed) tests.
        (expect (org-ql test-buffer
                  (closed < "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed < "2017-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ">"
        (expect (org-ql test-buffer
                  (closed > "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Learn universal sign language"))
        (expect (org-ql test-buffer
                  (closed > "2019-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ">="
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
      (it "<="
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
      (it "without arguments"
        (expect (org-ql test-buffer
                  (deadline)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs")))
      (it "="
        (expect (org-ql test-buffer
                  (deadline = "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline = "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it "<"
        (expect (org-ql test-buffer
                  (deadline < "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline < "2017-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ">"
        ;; TODO: Figure out why these tests take much longer than e.g. the (deadline <) tests.
        (expect (org-ql test-buffer
                  (deadline > "2017-07-04 00:00")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (expect (org-ql test-buffer
                  (deadline > "2019-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ">="
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
      (it "<="
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

    (it "(done)"
      (expect (org-ql test-buffer
                (done)
                :action (org-ql-test-org-get-heading))
              :to-equal '("Learn universal sign language")))

    (describe "(regexp)"
      (it "with 1 argument"
        (expect (org-ql test-buffer
                  (regexp "Take over")
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))
      (it "with 2 arguments"
        (expect (org-ql test-buffer
                  (regexp "Take over" "pizza")
                  :sort todo
                  :action (org-ql-test-org-get-heading)) :to-equal '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Order a pizza" "Get haircut"))))

    (describe "(ts)"
      (it "without arguments"
        (expect (org-ql test-buffer
                  (ts)
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))
      (it ":from a timestamp"
        ;; TODO: Figure out why these take longer than the other (ts) tests.
        (expect (org-ql test-buffer
                  (ts :from "2017-01-01")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :from "2019-06-08")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      (it ":to a timestamp"
        (expect (org-ql test-buffer
                  (ts :to "2019-06-10")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :to "2017-07-04")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Skype with president of Antarctica")))
      (it ":on a timestamp"
        (expect (org-ql test-buffer
                  (ts :on "2017-07-05")
                  :action (org-ql-test-org-get-heading))
                :to-equal '("Test data" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (expect (org-ql test-buffer
                  (ts :on "2019-06-09")
                  :action (org-ql-test-org-get-heading))
                :to-equal nil))
      )

    ;; TODO: Other predicates.
    ))

;;; org-ql.el ends here
