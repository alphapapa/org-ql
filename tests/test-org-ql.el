;;; org-ql.el --- Tests for org-ql                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Package-Requires: ((buttercup) (with-simulated-input))

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
(require 'with-simulated-input)

(require 'org-ql)

;;;; Variables

(defvar org-ql-test-buffer nil
  "Buffer used for running test queries.")

(defvar org-ql-test-num-headings nil
  "Total number of headings in main test data buffer.
Set at runtime by test suite.")

;;;; Functions

(defun org-ql-test-insert-result ()
  "FIXME: docstring"
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (correct-sexp-p (pcase (car sexp)
                              ('org-ql 'org-ql)
                              ('org-ql-expect t)
                              ('org-ql--query-preamble 'query-preamble)
                              ('org-ql--normalize-query t)
                              (_ nil)))
            (result (pcase sexp
                      (`(org-ql-expect ,args)
                       (org-ql-test--format-result--ql `(org-ql org-ql-test-buffer
                                                          ,@args
                                                          :action (org-ql-test-org-get-heading))))
                      (`(org-ql-expect ,args ,_ :buffer ,buffer)
                       (org-ql-test--format-result--ql `(org-ql ,buffer
                                                          ,@args
                                                          :action (org-ql-test-org-get-heading))))
                      (`(org-ql . _) (org-ql-test--format-result--ql sexp))
                      (`(org-ql--query-preamble  . _) (org-ql-test--format-result--query-preamble sexp))
                      (`(org-ql--normalize-query . _) (format "'%S" (eval sexp)))
                      (_ nil))))
      (progn
        (backward-char 1)
        (insert "\n'" result))
    (user-error "Point must be after an `org-ql' form")))

(defun org-ql-test--format-result--ql (sexp)
  (let* ((value (eval sexp))
         ;; FIXME: Not sure why this is currently unused.  Commenting out for now.
         ;; (prefix (if (and value (listp value))
         ;;             "'"
         ;;           ""))
         )
    (format "%S" value)))

(defun org-ql-test--format-result--query-preamble (sexp)
  (-let* (((query preamble) (eval sexp))
          (preamble (when preamble
                      ;; NOTE: `xr' is used here, but this function is only called by
                      ;; interactive helper commands in this file, not when running tests.
                      (xr preamble 'brief))))
    (format "`(%S ,(rx %S))" query preamble)))

(defun org-ql-test-show-result ()
  "Show `org-ql-agenda' for `org-ql' form."
  (interactive)
  (if-let* ((sexp (elisp--preceding-sexp))
            (sexp (pcase sexp
                    (`(org-ql . ,_) (setf (car sexp) 'org-ql-agenda))
                    (`(org-ql-expect ,ql-args . ,_) (setf sexp `(org-ql-search org-ql-test-buffer
                                                                  ',@ql-args)))
                    (_ nil))))

      (eval sexp)
    (user-error "Point must be after an `org-ql' form")))

(defun org-ql-test-data-buffer (filename)
  "Return buffer visiting FILENAME.
FILENAME should be a file in the \"tests\" directory."
  (->> (locate-dominating-file default-directory ".git")
       (expand-file-name "tests")
       (expand-file-name filename)
       find-file-noselect))

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

(cl-defmacro org-ql-expect (ql-args results &key (buffer 'org-ql-test-buffer))
  "Expand to `expect' test form that expects QL-ARGS to equal RESULTS.
RESULTS should be a list of strings as returned by
`org-ql-test-org-get-heading'."
  (declare (indent defun))
  `(expect (org-ql ,buffer
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

    (setq org-ql-test-buffer (org-ql-test-data-buffer "data.org")
          ;; For manual testing:
          ;; org-ql-test-buffer (find-file-noselect "data.org")
          org-ql-test-num-headings (with-current-buffer org-ql-test-buffer
                                     (org-with-wide-buffer
                                      (goto-char (point-min))
                                      (cl-loop while (re-search-forward org-heading-regexp nil t)
                                               sum 1)))))

  (describe "Caching"

    (it "Clears value cache after buffer changes"
      ;; See <https://github.com/alphapapa/org-ql/issues/59>.
      (with-temp-buffer
        (org-mode)
        (insert "* Heading 1
* Heading 2")
        ;; FIXME: `--value-at' does not actually move point, so we do it here.
        (goto-char (point-min))
        (expect (org-ql--value-at (point-min) #'org-get-heading)
                :to-equal "Heading 1")
        (erase-buffer)
        ;; FIXME: See above.
        (insert "* Heading 2")
        (goto-char (point-min))
        (org-ql--value-at (point-min) #'point)
        (expect (org-ql--value-at (point-min) #'org-get-heading)
                :to-equal "Heading 2")))

    (it "Returns nil when cache misses and function returns nil"
      ;; See <https://github.com/alphapapa/org-ql/pull/78>.
      (with-temp-buffer
        (org-mode)
        (insert "* Heading 1")
        ;; FIXME: `--value-at' does not actually move point, so we do it here.
        (goto-char (point-min))
        (expect (org-ql--value-at (point-min) #'org-get-local-tags)
                :to-be nil))))

  (describe "Query functions/macros"

    (it "org-ql"
      (expect (length (org-ql org-ql-test-buffer
                        (category)
                        :sort deadline))
              :to-equal org-ql-test-num-headings))
    (it "org-ql-select"
      (expect (length (org-ql-select org-ql-test-buffer
                        '(category)
                        :sort 'deadline))
              :to-equal org-ql-test-num-headings))
    (it "org-ql-query"
      (expect (length (org-ql-query :select 'element
                                    :from org-ql-test-buffer
                                    :where '(category)
                                    :order-by 'date))
              :to-equal org-ql-test-num-headings)))

  (describe "Query pre-processing"

    (describe "(level)"
      (it "with one level"
        (expect (org-ql--normalize-query '(level "1"))
                :to-equal '(level 1)))
      (it "with two levels"
        (expect (org-ql--normalize-query '(level "1" "2"))
                :to-equal '(level 1 2)))
      (it "with a comparator and a level"
        (expect (org-ql--normalize-query '(level ">" "1"))
                :to-equal '(level > 1))))

    (describe "(link)"
      (it "with one argument"
        (expect (org-ql--normalize-query '(link "DESC-OR-TARGET"))
                :to-equal '(link "DESC-OR-TARGET")))
      (it "with one argument and :regexp-p"
        (expect (org-ql--normalize-query '(link "DESC-OR-TARGET" :regexp-p t))
                :to-equal '(link "DESC-OR-TARGET" :regexp-p t)))
      (it "with keyword arguments"
        (expect (org-ql--normalize-query '(link :description "DESCRIPTION" :target "TARGET"
                                                :regexp-p t))
                :to-equal '(link :description "DESCRIPTION" :target "TARGET"
                                 :regexp-p t))))

    (expect (org-ql--normalize-query '(and "string1" "string2"))
            :to-equal '(and (regexp "string1") (regexp "string2")))
    (expect (org-ql--normalize-query '(or "string1" "string2"))
            :to-equal '(or (regexp "string1") (regexp "string2")))
    (expect (org-ql--normalize-query '(and (todo "TODO")
                                           (or "string1" "string2")))
            :to-equal '(and (todo "TODO") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--normalize-query '(when (todo "TODO")
                                        (or "string1" "string2")))
            :to-equal '(when (todo "TODO") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--normalize-query '(when "string-cond1"
                                        (or "string1" "string2")))
            :to-equal '(when (regexp "string-cond1") (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--normalize-query '(when (and "string-cond1" "string-cond2")
                                        (or "string1" "string2")))
            :to-equal '(when (and (regexp "string-cond1") (regexp "string-cond2")) (or (regexp "string1") (regexp "string2"))))
    (expect (org-ql--normalize-query '(unless (and "stringcondition1" "stringcond2")
                                        (or "string1" "string2")))
            :to-equal '(unless (and (regexp "stringcondition1") (regexp "stringcond2")) (or (regexp "string1") (regexp "string2"))))

    (expect (org-ql--normalize-query '(or (ts-active :on "2019-01-01")
                                          (ts-a :on "2019-01-01")
                                          (ts-inactive :on "2019-01-01")
                                          (ts-i :on "2019-01-01")))
            :to-equal '(or (ts :type active :on "2019-01-01")
                           (ts :type active :on "2019-01-01")
                           (ts :type inactive :on "2019-01-01")
                           (ts :type inactive :on "2019-01-01"))))

  (describe "Query preambles"

    ;; TODO: Other predicates.

    (describe "(clocked)"
      (it "without arguments"
        (expect (org-ql--query-preamble '(clocked))
                :to-equal (list :query t
                                :preamble org-ql-clock-regexp
                                :preamble-case-fold nil)))
      (it "with a number of days"
        (expect (org-ql--query-preamble '(clocked 1))
                :to-equal (list :query t
                                :preamble org-ql-clock-regexp
                                :preamble-case-fold nil)))
      ;; TODO: Other arguments for (clocked).
      )

    (describe "(level)"
      (it "with a number"
        (expect (org-ql--query-preamble '(level 2))
                :to-equal (list :query t
                                :preamble (rx bol (repeat 2 "*") " ")
                                :preamble-case-fold t)))
      (it "with two numbers"
        (expect (org-ql--query-preamble '(level 2 4))
                :to-equal (list :query t
                                :preamble (rx bol (repeat 2 4 "*") " ")
                                :preamble-case-fold t)))
      (it "<"
        (expect (org-ql--query-preamble '(level < 3))
                :to-equal (list :query t
                                :preamble (rx bol (repeat 1 2 "*") " ")
                                :preamble-case-fold t)))
      (it "<="
        (expect (org-ql--query-preamble '(level <= 2))
                :to-equal (list :query t
                                :preamble (rx bol (repeat 1 2 "*") " ")
                                :preamble-case-fold t)))
      (it ">"
        (expect (org-ql--query-preamble '(level > 2))
                :to-equal (list :query t
                                :preamble (rx bol (>= 3 "*") " ")
                                :preamble-case-fold t)))
      (it ">="
        (expect (org-ql--query-preamble '(level >= 2))
                :to-equal (list :query t
                                :preamble (rx bol (>= 2 "*") " ")
                                :preamble-case-fold t)))))

  (describe "Plain query parsing"

    ;; TODO: Other predicates.

    (it "Negated terms"
      (expect (org-ql--plain-query "todo: !todo:CHECK,SOMEDAY")
              :to-equal '(and (todo) (not (todo "CHECK" "SOMEDAY"))))
      (expect (org-ql--plain-query "!todo:CHECK,SOMEDAY todo:")
              :to-equal '(and (not (todo "CHECK" "SOMEDAY")) (todo)))
      (expect (org-ql--plain-query "tags:universe !moon")
              :to-equal '(and (tags "universe") (not (regexp "moon"))))
      (expect (org-ql--plain-query "!moon tags:universe")
              :to-equal '(and (not (regexp "moon")) (tags "universe")))
      (expect (org-ql--plain-query "mars !ts:on=today")
              :to-equal '(and (regexp "mars") (not (ts :on "today"))))
      (expect (org-ql--plain-query "!\"quoted phrase\"")
              :to-equal '(not (regexp "quoted phrase"))))
    (it "Regexp predicates"
      (expect (org-ql--plain-query "scheduled")
              ;; No colon after keyword, so not a predicate query.
              :to-equal '(regexp "scheduled"))
      (expect (org-ql--plain-query "\"quoted phrase\"")
              :to-equal '(regexp "quoted phrase"))
      (expect (org-ql--plain-query "regexp:word")
              :to-equal '(regexp "word"))
      (expect (org-ql--plain-query "regexp:\"quoted phrase\"")
              :to-equal '(regexp "quoted phrase")))
    (it "Timestamp-based predicates"
      (expect (org-ql--plain-query "scheduled:on=2017-07-07")
              :to-equal '(scheduled :on "2017-07-07"))
      (expect (org-ql--plain-query "deadline:from=2017-07-07,to=2017-07-09")
              :to-equal '(deadline :from "2017-07-07" :to "2017-07-09"))
      (expect (org-ql--plain-query "planning:from=2017-07-07")
              :to-equal '(planning :from "2017-07-07"))
      (expect (org-ql--plain-query "closed:from=2017-07-07")
              :to-equal '(closed :from "2017-07-07"))
      (expect (org-ql--plain-query "ts-active:to=2017-07-07")
              :to-equal '(ts-active :to "2017-07-07"))
      (expect (org-ql--plain-query "ts-inactive:to=2017-07-07")
              :to-equal '(ts-inactive :to "2017-07-07"))
      (expect (org-ql--plain-query "ts-a:to=2017-07-07")
              :to-equal '(ts-a :to "2017-07-07"))
      (expect (org-ql--plain-query "ts-i:on=2017-07-07")
              :to-equal '(ts-i :on "2017-07-07"))
      (expect (org-ql--plain-query "ts:")
              :to-equal '(ts))
      (expect (org-ql--plain-query "clocked:")
              :to-equal '(clocked)))
    (it "To-do predicates"
      (expect (org-ql--plain-query "todo:")
              :to-equal '(todo))
      (expect (org-ql--plain-query "todo:TODO")
              :to-equal '(todo "TODO"))
      (expect (org-ql--plain-query "todo:TODO,SOMEDAY")
              :to-equal '(todo "TODO" "SOMEDAY")))
    (it "Compound queries"
      (expect (org-ql--plain-query "todo:SOMEDAY ts-a:from=2020-01-01,to=2021-01-01")
              :to-equal '(and (todo "SOMEDAY") (ts-a :from "2020-01-01" :to "2021-01-01")))
      (expect (org-ql--plain-query "regexp:\"quoted phrase\" todo:SOMEDAY")
              :to-equal '(and (regexp "quoted phrase") (todo "SOMEDAY")))))

  (describe "Convert sexp queries to non-sexp queries"

    ;; FIXME: Test (src) after converting it is implemented.
    ;; (src :lang "elisp" :regexps ("defun"))

    ;; MAYBE: Other predicates?  Or should these cover the other
    ;; cases, because the others use the same format?

    (it "(heading)"
      (expect (org-ql--query-sexp-to-string '(heading "quoted phrase" "word"))
              :to-equal "heading:word,\"quoted phrase\""))
    (it "(priority)"
      (expect (org-ql--query-sexp-to-string '(priority >= B))
              :to-equal "priority:A,B")
      (expect (org-ql--query-sexp-to-string '(priority > B))
              :to-equal "priority:A")
      (expect (org-ql--query-sexp-to-string '(priority < B))
              :to-equal "priority:C")
      (expect (org-ql--query-sexp-to-string '(priority < A))
              :to-equal "priority:B,C")
      (expect (org-ql--query-sexp-to-string '(priority <= B))
              :to-equal "priority:B,C")
      (expect (org-ql--query-sexp-to-string '(priority = A))
              :to-equal "priority:A"))
    (it "(todo)"
      (expect (org-ql--query-sexp-to-string '(todo))
              :to-equal "todo:")
      (expect (org-ql--query-sexp-to-string '(todo "TODO"))
              :to-equal "todo:TODO")
      (expect (org-ql--query-sexp-to-string '(todo "TODO" "NEXT"))
              :to-equal "todo:NEXT,TODO"))
    (it "(ts)"
      (expect (org-ql--query-sexp-to-string '(ts :from -1 :to 1))
              :to-equal "ts:from=-1,to=1")
      (expect (org-ql--query-sexp-to-string '(ts :on today))
              :to-equal "ts:on=today")
      (expect (org-ql--query-sexp-to-string '(ts-active :from "2017-01-01" :to "2018-01-01"))
              :to-equal "ts-active:from=2017-01-01,to=2018-01-01"))
    (it "(and ...)"
      (expect (org-ql--query-sexp-to-string '(and (tags "book" "books") (priority "A")))
              :to-equal "tags:books,book priority:A")
      (expect (org-ql--query-sexp-to-string '(and (tags "space") (not (regexp "moon"))))
              :to-equal "tags:space !regexp:moon"))
    (it "(or ...)"
      (expect (org-ql--query-sexp-to-string '(or (tags "book" "books") (priority "A")))
              :to-equal nil)))

  (describe "Query results"

    ;; TODO: Other predicates.

    (describe "(ancestors)"
      (org-ql-it "without sub-query"
        (org-ql-expect ((ancestors))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with sub-query"
        (org-ql-expect ((ancestors (heading "universe")))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(parent)"
      (org-ql-it "without sub-query"
        (org-ql-expect ((parent))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with sub-query"
        (org-ql-expect ((parent (and (todo) (priority "A"))))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Take over the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(category)"

      (org-ql-it "without arguments"
        (expect (length (org-ql org-ql-test-buffer
                          (category)))
                :to-equal org-ql-test-num-headings))

      (org-ql-it "with a category"
        (org-ql-expect ((category "ambition"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))))

    (describe "(children)"
      (org-ql-it "without arguments"
        (org-ql-expect ((children))
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Recurring" "Ideas")))
      (org-ql-it "with sub-query"
        (org-ql-expect ((children (todo "CHECK")))
          '("Recurring")))
      (org-ql-it "with grandchildren query"
        ;; It's really cool how this works.  It's so simple.
        (org-ql-expect ((children (children "moon")))
          '("Take over the universe"))))

    (describe "(descendants)"
      (org-ql-it "without arguments"
        (org-ql-expect ((descendants))
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Recurring" "Ideas")))
      (org-ql-it "with sub-query"
        (org-ql-expect ((descendants (todo "CHECK")))
          '("Recurring")))
      (org-ql-it "with granddescendants query"
        (org-ql-expect ((descendants (descendants "moon")))
          '("Take over the universe")))
      (org-ql-it "with query that should not match parent"
        ;; This test would fail if the `descendants' predicate did not properly exclude
        ;; the parent heading by narrowing the buffer to begin at the first child.
        (org-ql-expect ((and (descendants (todo "WAITING"))
                             (not (descendants (todo "TODO" "NEXT")))))
          '("Take over the moon"))))

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

      (org-ql-it ":from today"
        (org-ql-then
          (org-ql-expect ((clocked :from today))
            '("Learn universal sign language"))))

      (org-ql-it ":to a timestamp"
        (org-ql-expect ((clocked :to "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :to "2017-07-04"))
          nil))

      (org-ql-it ":to today"
        (org-ql-then
          (org-ql-expect ((clocked :to today))
            '("Learn universal sign language"))))

      (org-ql-it ":on a date"
        (org-ql-expect ((clocked :on "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-expect ((clocked :on "2018-12-02"))
          nil))

      (org-ql-it ":on today"
        (org-ql-then
          (org-ql-expect ((clocked :on today))
            '("Learn universal sign language"))))

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
        (org-ql-then
          (org-ql-expect ((closed :on today))
            '("Learn universal sign language")))
        (org-ql-expect ((closed :on "2019-06-09"))
          nil))

      (org-ql-it ":from"
        (org-ql-expect ((closed :from "2017-07-04"))
          '("Learn universal sign language"))
        (org-ql-expect ((closed :from "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-then
          (org-ql-expect ((closed :from today))
            '("Learn universal sign language")))
        (org-ql-expect ((closed :from "2017-07-06"))
          nil))

      (org-ql-it ":to"
        (org-ql-expect ((closed :to "2017-07-04"))
          nil)
        (org-ql-expect ((closed :to "2017-07-05"))
          '("Learn universal sign language"))
        (org-ql-then
          (org-ql-expect ((closed :to today))
            '("Learn universal sign language")))
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
        (org-ql-then
          (org-ql-expect ((deadline :on today))
            '("/r/emacs")))

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
          nil)
        (org-ql-then
          (org-ql-expect ((deadline :from today))
            '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))))

      (org-ql-it ":to"
        (org-ql-expect ((deadline :to "2017-07-04"))
          nil)
        (org-ql-expect ((deadline :to "2017-07-05"))
          '("/r/emacs"))
        (org-ql-expect ((deadline :to "2018-07-06"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "/r/emacs"))
        (org-ql-then
          (org-ql-expect ((deadline :to today))
            '("/r/emacs")))))

    (org-ql-it "(done)"
      (org-ql-expect ((done))
        '("Learn universal sign language")))

    (org-ql-it "(habit)"
      (org-ql-expect ((habit))
        '("Practice leaping tall buildings in a single bound")))

    (describe "(heading)"
      (org-ql-it "with one argument"
        (org-ql-expect ((heading "world"))
          ;; NOTE: This--correctly--does not match the "Skype with president of
          ;; Antarctica" heading, which has the tag ":world:" in its heading line.
          '("Take over the world")))
      (org-ql-it "with two arguments"
        (org-ql-expect ((heading "Take over" "world"))
          '("Take over the world"))))

    (describe "(link)"
      (org-ql-it "without arguments"
        (org-ql-expect ((link))
          '("/r/emacs")))
      (org-ql-it "with description-or-target"
        (org-ql-expect ((link "emacs"))
          '("/r/emacs")))
      (org-ql-it "with :description"
        (org-ql-expect ((link :description "emacs"))
          '("/r/emacs")))
      (org-ql-it "with :target"
        (org-ql-expect ((link :target "reddit.com"))
          '("/r/emacs")))
      (org-ql-it "with :description and :target"
        (org-ql-expect ((link :description "emacs" :target "reddit.com"))
          '("/r/emacs"))))

    (describe "(outline-path)"
      (org-ql-it "with one argument"
        (org-ql-expect ((outline-path "symphony"))
          '("Write a symphony")))
      (org-ql-it "with two arguments"
        (org-ql-expect ((outline-path "idea" "symphony"))
          '("Write a symphony"))))

    (describe "(outline-path-segment)"
      (org-ql-it "with one argument"
        (org-ql-expect ((outline-path-segment "symphony"))
          '("Write a symphony")))
      (org-ql-it "with a contiguous segment"
        (org-ql-expect ((outline-path-segment "idea" "symphony"))
          '("Write a symphony")))
      (org-ql-it "with a non-contiguous segment"
        (org-ql-expect ((outline-path-segment "data" "symphony"))
          nil)))

    (describe "(path)"
      (org-ql-it "without arguments"
        (org-ql-expect ((path))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))
      (org-ql-it "with one argument"
        (org-ql-expect ((path "data"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))
      (org-ql-it "with two matching arguments"
        (org-ql-expect ((path "data" "tests"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))
      (org-ql-it "with two arguments, one matching"
        (org-ql-expect ((path "data" "nope"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony"))))

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
          nil)
        (org-ql-then
          (org-ql-expect ((planning :on today))
            '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

      (org-ql-it ":from"
        (org-ql-expect ((planning :from "2017-07-04"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :from "2017-07-05"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :from "2017-07-06"))
          '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease"))
        (org-ql-then
          (org-ql-expect ((planning :from today))
            '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

      (org-ql-it ":to"
        (org-ql-expect ((planning :to "2017-07-04"))
          '("Skype with president of Antarctica"))
        (org-ql-expect ((planning :to "2017-07-05"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((planning :to "2018-07-06"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-then
          (org-ql-expect ((planning :to today))
            '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))))

    (describe "(priority)"

      (org-ql-it "without arguments"
        (org-ql-expect ((priority))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Take over the moon" "Renew membership in supervillain club" "Learn universal sign language" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor")))
      (org-ql-it "with a priority"
        (org-ql-expect ((priority "A"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Spaceship lease")))
      (org-ql-it "= a priority"
        (org-ql-expect ((priority = "A"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Spaceship lease")))
      (org-ql-it "< a priority"
        (org-ql-expect ((priority < "B"))
          '("Take over the moon" "Get haircut")))
      (org-ql-it "<= a priority"
        (org-ql-expect ((priority <= "B"))
          '("Take over Mars" "Take over the moon" "Renew membership in supervillain club" "Learn universal sign language" "Get haircut" "Internet" "Fix flux capacitor")))
      (org-ql-it "> a priority"
        (org-ql-expect ((priority > "B"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Spaceship lease")))
      (org-ql-it ">= a priority"
        (org-ql-expect ((priority >= "B"))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Renew membership in supervillain club" "Learn universal sign language" "Internet" "Spaceship lease" "Fix flux capacitor"))))
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
        (org-ql-expect ((regexp "Take over" "universe")
                        :sort todo)
          '("Take over the universe")))

      (org-ql-it "with a plain string"
        (org-ql-expect ("Take over"
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))

      (org-ql-it "with two plain strings in an OR"
        (org-ql-expect ((or "Take over" "universe")
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut")))

      (org-ql-it "case-folding predicate with non-case-folding preamble"
        ;; e.g. the (todo) predicate disables case-folding in its preamble, but that
        ;; should not prevent case-folding in this and other predicates (issue #114).
        (org-ql-expect ((and (todo "TODO") (regexp "take over"))
                        :sort todo)
          '("Take over the universe" "Take over the world" "Take over Mars" "Take over the moon" "Get haircut"))))

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
          nil)
        (org-ql-then
          (org-ql-expect ((scheduled :on today))
            '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

      (org-ql-it ":from"
        (org-ql-expect ((scheduled :from "2017-07-04"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :from "2017-07-05"))
          '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :from "2017-07-06"))
          nil)
        (org-ql-then
          (org-ql-expect ((scheduled :from today))
            '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

      (org-ql-it ":to"
        (org-ql-expect ((scheduled :to "2017-07-04"))
          '("Skype with president of Antarctica"))
        (org-ql-expect ((scheduled :to "2017-07-05"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((scheduled :to "2018-07-06"))
          '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
        (org-ql-then
          (org-ql-expect ((scheduled :to today))
            '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "Shop for groceries" "Rewrite Emacs in Common Lisp")))))

    ;; ;; TODO: Test (src) predicate.  That will require modifying test data, which will be a
    ;; ;; significant hassle.  Manual testing shows that the predicate appears to work properly.
    ;;
    (describe "(todo)"

      (org-ql-it "without arguments"
        (org-ql-expect ((todo)
                        :sort todo)
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with 1 argument"
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
          '("Recurring" "Sunrise/sunset" "Ideas")))

      (org-ql-it "with a tag"
        (org-ql-expect ((tags "Emacs"))
          '("/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags "Emacs")))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony")))

      (org-ql-it "with 2 tags"
        (org-ql-expect ((tags "Emacs" "space"))
          '("Visit Mars" "Visit the moon" "/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags "Emacs" "space")))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Take over the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony")))

      (org-ql-it "with file tags"
        (org-ql-expect ((tags "food"))
          '("Fruit" "Blueberry" "Strawberry" "Vegetable" "Broccoli" "Potato")
          :buffer (org-ql-test-data-buffer "data2.org"))
        (org-ql-expect ((tags "fruit"))
          '("Fruit" "Blueberry" "Strawberry")
          :buffer (org-ql-test-data-buffer "data2.org"))))

    (describe "(tags-inherited)"

      (org-ql-it "without arguments"
        (org-ql-expect ((tags-inherited))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))
        (org-ql-expect ((not (inherited-tags)))
          '("Take over the universe" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with a tag"
        (org-ql-expect ((tags-inherited "Emacs"))
          nil)
        (org-ql-expect ((itags "ambition"))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language"))
        (org-ql-expect ((not (tags-i "ambition")))
          '("Take over the universe" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with 2 tags"
        (org-ql-expect ((itags "personal" "world"))
          '("Skype with president of Antarctica"))
        (org-ql-expect ((not (tags-inherited "personal" "world")))
          ;; Note that this correctly includes the task "Practice leaping...", which has the LOCAL tag "personal".
          '("Take over the universe" "Take over the world" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with file tags"
        (org-ql-expect ((tags-inherited "food"))
          '("Fruit" "Blueberry" "Strawberry" "Vegetable" "Broccoli" "Potato")
          :buffer (org-ql-test-data-buffer "data2.org"))
        (org-ql-expect ((tags-inherited "fruit"))
          '("Blueberry" "Strawberry")
          :buffer (org-ql-test-data-buffer "data2.org"))))

    (describe "(tags-local)"

      (org-ql-it "without arguments"
        (org-ql-expect ((tags-local))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony"))
        (org-ql-expect ((not (local-tags)))
          '("Take over Mars" "Take over the moon" "Renew membership in supervillain club" "Learn universal sign language" "Recurring" "Sunrise/sunset" "Ideas")))

      (org-ql-it "with a tag"
        (org-ql-expect ((tags-local "world"))
          '("Take over the world" "Skype with president of Antarctica"))
        (org-ql-expect ((ltags "ambition"))
          '("Take over the universe"))
        (org-ql-expect ((not (tags-l "ambition")))
          '("Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with 2 tags"
        (org-ql-expect ((ltags "personal" "world"))
          '("Take over the world" "Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Get haircut"))
        (org-ql-expect ((not (tags-local "personal" "world")))
          '("Take over the universe" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "/r/emacs" "Shop for groceries" "Sunrise/sunset" "Ideas" "Rewrite Emacs in Common Lisp" "Write a symphony")))

      (org-ql-it "with file tags"
        (org-ql-expect ((tags-local "food"))
          nil
          :buffer (org-ql-test-data-buffer "data2.org"))
        (org-ql-expect ((tags-local "fruit"))
          '("Fruit")
          :buffer (org-ql-test-data-buffer "data2.org"))))

    (describe "(tags-all), (tags&)"

      (org-ql-it "with 2 tags"
        (org-ql-expect ((tags-all "universe" "personal"))
          '("Practice leaping tall buildings in a single bound"))
        (org-ql-expect ((tags& "ambition" "space"))
          '("Visit Mars" "Visit the moon")))

      (org-ql-it "with file tags"
        (org-ql-expect ((tags-all "food" "fruit"))
          '("Fruit" "Blueberry" "Strawberry")
          :buffer (org-ql-test-data-buffer "data2.org"))))

    (describe "(tags-regexp), (tags*)"

      (org-ql-it "without arguments"
        (org-ql-expect ((tags-regexp))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp" "Write a symphony"))
        (org-ql-expect ((not (tags*)))
          '("Recurring" "Sunrise/sunset" "Ideas")))

      (org-ql-it "with a tag regexp"
        (org-ql-expect ((tags-regexp "Emac"))
          '("/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags* "Emac")))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Visit Mars" "Take over the moon" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony")))

      (org-ql-it "with 2 tag regexps"
        (org-ql-expect ((tags-regexp "Emac" "spac"))
          '("Visit Mars" "Visit the moon" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Rewrite Emacs in Common Lisp"))
        (org-ql-expect ((not (tags* "Emac" "spac")))
          '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Take over Mars" "Take over the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Recurring" "Shop for groceries" "Sunrise/sunset" "Ideas" "Write a symphony")))

      (org-ql-it "with regexp matching file tags"
        (org-ql-expect ((tags-regexp "foo"))
          '("Fruit" "Blueberry" "Strawberry" "Vegetable" "Broccoli" "Potato")
          :buffer (org-ql-test-data-buffer "data2.org"))
        (org-ql-expect ((tags* "frui"))
          '("Fruit" "Blueberry" "Strawberry")
          :buffer (org-ql-test-data-buffer "data2.org"))))

    (describe "(ts)"

      (describe "active"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts :type active))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          (org-ql-expect ((ts :from "2017-07-08" :type active))
            '("Take over the universe" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease"))
          (org-ql-expect ((ts :from "2019-06-08" :type active))
            nil)
          (org-ql-then
            (org-ql-expect ((ts :from today))
              '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":from a number of days"
          (org-ql-then
            (org-ql-expect ((ts :from 5))
              '("Take over the universe" "Visit Mars" "Visit the moon" "Renew membership in supervillain club" "Internet" "Spaceship lease" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2019-06-10" :type active))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04" :type active))
            '("Skype with president of Antarctica"))
          (org-ql-then
            (org-ql-expect ((ts :to today))
              '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":to a number of days"
          (org-ql-then
            (org-ql-expect ((ts :to -1))
              '("Skype with president of Antarctica"))))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05" :type active))
            '("Practice leaping tall buildings in a single bound" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2019-06-09" :type active))
            nil)
          (org-ql-then
            (org-ql-expect ((ts :on today))
              '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":on a number of days"
          (org-ql-then
            (org-ql-expect ((ts-active :on 2))
              '("Take over the world")))))

      (describe "inactive"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts :type inactive))
            '("Visit the moon" "Learn universal sign language" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          (org-ql-expect ((ts :from "2017-07-06" :type inactive))
            '("Visit the moon" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2019-06-08" :type inactive))
            nil)
          (org-ql-then
            (org-ql-expect ((ts-inactive :from today))
              '("Visit the moon" "Learn universal sign language" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":from a number of days"
          (org-ql-then
            (org-ql-expect ((ts-i :from 5))
              '("Visit the moon" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2019-06-10" :type inactive))
            '("Visit the moon" "Learn universal sign language" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04" :type inactive))
            'nil)
          (org-ql-then
            (org-ql-expect ((ts-inactive :to today))
              '("Learn universal sign language"))))

        (org-ql-it ":to a number of days"
          (org-ql-then
            (org-ql-expect ((ts-i :to 5))
              '("Learn universal sign language"))))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05" :type inactive))
            '("Learn universal sign language"))
          (org-ql-expect ((ts :on "2019-06-09" :type inactive))
            nil)
          (org-ql-then
            (org-ql-expect ((ts-inactive :on today))
              '("Learn universal sign language"))))

        (org-ql-it ":on a number of days"
          (org-ql-then
            (org-ql-expect ((ts-inactive :on 19))
              '("Visit the moon" "Rewrite Emacs in Common Lisp")))))

      (describe "both"

        (org-ql-it "without arguments"
          (org-ql-expect ((ts))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :type both))
            '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp")))

        (org-ql-it ":from a timestamp"
          (org-ql-expect ((ts :from "2017-07-05"))
            '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2017-07-05" :type both))
            '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :from "2019-06-08"))
            nil)
          (org-ql-expect ((ts :from "2019-06-08" :type both))
            nil)
          (org-ql-then
            (org-ql-expect ((ts :from today))
              '("Take over the universe" "Take over the world" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":from a number of days"
          (org-ql-then
            (org-ql-expect ((ts :from -5))
              '("Take over the universe" "Take over the world" "Skype with president of Antarctica" "Visit Mars" "Visit the moon" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Internet" "Spaceship lease" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":to a timestamp"
          (org-ql-expect ((ts :to "2017-07-06"))
            '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-06" :type both))
            '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :to "2017-07-04"))
            '("Skype with president of Antarctica"))
          (org-ql-expect ((ts :to "2017-07-04" :type both))
            '("Skype with president of Antarctica"))
          (org-ql-then
            (org-ql-expect ((ts :to today))
              '("Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":to a number of days"
          (org-ql-then
            (org-ql-expect ((ts :to 5))
              '("Take over the world" "Skype with president of Antarctica" "Practice leaping tall buildings in a single bound" "Renew membership in supervillain club" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":on a timestamp"
          (org-ql-expect ((ts :on "2017-07-05"))
            '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2017-07-05" :type both))
            '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))
          (org-ql-expect ((ts :on "2019-06-09"))
            nil)
          (org-ql-expect ((ts :on "2019-06-09" :type both))
            nil)
          (org-ql-then
            (org-ql-expect ((ts :on today))
              '("Practice leaping tall buildings in a single bound" "Learn universal sign language" "Order a pizza" "Get haircut" "Fix flux capacitor" "/r/emacs" "Shop for groceries" "Rewrite Emacs in Common Lisp"))))

        (org-ql-it ":on a number of days"
          (org-ql-then
            (org-ql-expect ((ts :on 5))
              '("Renew membership in supervillain club"))))))

    (describe "Compound queries"

      (org-ql-it "Tags and to-do"
        (org-ql-expect ((and (todo "SOMEDAY")
                             (tags "Emacs")))
          '("Rewrite Emacs in Common Lisp")))))

  (describe "Org link safety"

    ;; NOTE: These tests probably do not guarantee safety.  Still, these tests are probably
    ;; better than nothing.  In fact, this is pretty cool: by testing for the specific error
    ;; signal and the arguments to the error, I caught a mistake I made while writing these tests
    ;; (leaving a closing bracket off one of the links), which caused that test to fail but for a
    ;; different reason.  Were it not for testing the specific error AND its arguments, that test
    ;; case would have apparently passed, but it would have hidden the mistake in the test.

    ;; NOTE: Rather than calling `message' in the "evil" lambdas (which, if it succeeds
    ;; in being evil, merely writes a string to the test output, which is EASILY missed),
    ;; we call `error', which, in combination with testing for specific error types and
    ;; arguments, correctly causes tests to fail if the unsafe condition is not caught.

    ;; Also, while writing these tests, the version of org-super-agenda in the test sandbox does
    ;; not have the fix applied yet, and the test for that apparently, correctly does not pass yet.

    (require 'org-ql-search)
    (require 'org-ql-view)

    ;; The :auto-map test requires there to be results (because the org-super-agenda :auto-map group's
    ;; key-form, where the safety check is, only gets evaluated when there are results).  Using
    ;; `with-temp-buffer' in the `open-link' function causes the temp buffer to be killed before the results
    ;; have a chance to be gathered.  So we make a test buffer and run the test in that, with a test heading.

    (let ((test-buffer (get-buffer-create "*test-org-ql*")))
      (cl-flet ((open-link
                 (link) (with-current-buffer test-buffer
                          (erase-buffer)
                          (org-mode)
                          (insert "* TODO Test heading \n\n")
                          (insert link)
                          (backward-char 1)
                          (call-interactively #'org-open-at-point))))

        (describe "buffers-files parameter"
          :var ((quoted-lambda-link "[[org-ql-search:todo:?buffers-files%3D%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29]]")
                (unquoted-lambda-link "[[org-ql-search:todo:?buffers-files%3D%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29]]")
                (quoted-lambda-in-list-link "[[org-ql-search:todo:?buffers-files%3D%28%28quote%20%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29%29%29]]")
                (unquoted-lambda-in-list-link "[[org-ql-search:todo:?buffers-files%3D%28%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29%29]]"))
          (it "Errors for a quoted lambda"
            (expect (open-link quoted-lambda-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe buffers-files parameter detected: (lambda nil (error UNSAFE))")))
          (it "Errors for an unquoted lambda"
            (expect (open-link unquoted-lambda-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe buffers-files parameter detected: (lambda nil (error UNSAFE))")))
          (it "Errors for a quoted lambda in a list"
            (if (version< (org-version) "9.3")
                (expect (open-link quoted-lambda-in-list-link)
                        :to-throw 'error '("CAUTION: Link not opened because unsafe buffers-files parameter detected: ((quote (lambda nil (error UNSAFE))))"))
              (expect (open-link quoted-lambda-in-list-link)
                      :to-throw 'error '("CAUTION: Link not opened because unsafe buffers-files parameter detected: ('(lambda nil (error UNSAFE)))"))))
          (it "Errors for an unquoted lambda in a list"
            (expect (open-link unquoted-lambda-in-list-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe buffers-files parameter detected: ((lambda nil (error UNSAFE)))"))))

        (describe "super-groups parameter"
          :var ((quoted-lambda-link "[[org-ql-search:todo:?super-groups%3D%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29]]")
                (unquoted-lambda-link "[[org-ql-search:todo:?super-groups%3D%28lambda%20nil%20%28error%20%22UNSAFE%22%29%29]]")
                (quoted-expression-link "[[org-ql-search:todo:?super-groups%3D%28error%20%22UNSAFE%22%29]]")
                (unquoted-expression-link "[[org-ql-search:todo:?super-groups%3D%22UNSAFE%22]]")
                (pred-selector-link "[[org-ql-search:todo:?super-groups%3D%28%28%3Apred%20%28lambda%20%28_%29%20%28error%20%22UNSAFE%22%29%29%29%29]]")
                (auto-map-selector-link "[[org-ql-search:todo:?super-groups%3D%28%28%3Aauto-map%20%28lambda%20%28_%29%20%28error%20%22UNSAFE%22%29%29%29%29]]"))
          (it "Errors for a quoted lambda"
            (expect (open-link quoted-lambda-link)
                    :to-throw 'wrong-type-argument '(listp lambda)))
          (it "Errors for an unquoted lambda"
            (expect (open-link unquoted-lambda-link)
                    :to-throw 'wrong-type-argument '(listp lambda)))
          (it "Errors for a quoted expression"
            (expect (open-link quoted-expression-link)
                    :to-throw 'wrong-type-argument '(listp error)))
          (it "Errors for an unquoted expression"
            (expect (open-link unquoted-expression-link)
                    :to-throw 'error '("cl-etypecase failed: UNSAFE, (symbol list)")))

          ;; NOTE: These two tests depend on `org-super-agenda' to signal these errors.  It's probably better
          ;; to catch these in `org-super-agenda' rather than in `org-ql', because if other potentially unsafe
          ;; selectors were added to org-super-agenda, org-ql would have to play catch-up, adding more tests.
          ;; Catching them in org-super-agenda means that it can add more checks itself in the future.
          (it "Errors for a :pred group"
            (expect (open-link pred-selector-link)
                    :to-throw 'error '("Unsafe groups disallowed (:pred): (lambda (_) (error UNSAFE))")))
          (it "Errors for an :auto-map group"
            (expect (open-link auto-map-selector-link)
                    :to-throw 'error '("Unsafe groups disallowed (:auto-map): ((lambda (_) (error UNSAFE)))"))))

        (describe "title parameter"
          :var ((quoted-lambda-link "[[org-ql-search:todo:?title%3D%28lambda%20%28_%20_%29%20%28error%20%22UNSAFE%22%29%29]]")
                (unquoted-lambda-link "[[org-ql-search:todo:?title%3D%28lambda%20%28_%20_%29%20%28error%20%22UNSAFE%22%29%29]]")
                (expression-link "[[org-ql-search:todo:?title%3D%28error%20%22UNSAFE%22%29]]"))
          (it "Errors for a quoted lambda"
            (expect (open-link quoted-lambda-link)
                    :to-throw 'wrong-type-argument '(characterp lambda)))
          (it "Errors for an unquoted lambda"
            (expect (open-link unquoted-lambda-link)
                    :to-throw 'wrong-type-argument '(characterp lambda)))
          (it "Errors for an expression"
            (expect (open-link expression-link)
                    :to-throw 'wrong-type-argument '(characterp error))))

        (describe "sort parameter"
          :var ((quoted-lambda-link "[[org-ql-search:todo:?sort%3D%28lambda%20%28_%20_%29%20%28error%20%22UNSAFE%22%29%29]]")
                (unquoted-lambda-link "[[org-ql-search:todo:?sort%3D%28lambda%20%28_%20_%29%20%28error%20%22UNSAFE%22%29%29]]")
                (quoted-lambda-in-list-link "[[org-ql-search:todo:?sort%3D%28%28quote%20%28lambda%20%28_%20_%29%20%28error%20%22UNSAFE%22%29%29%29%29]]")
                (unquoted-lambda-in-list-link "[[org-ql-search:todo:?sort=((lambda%20nil%20(error%20\"UNSAFE\")))]]"))
          (it "Errors for a quoted lambda"
            (expect (open-link quoted-lambda-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe sort parameter detected: (lambda (_ _) (error UNSAFE))")))
          (it "Errors for an unquoted lambda"
            (expect (open-link unquoted-lambda-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe sort parameter detected: (lambda (_ _) (error UNSAFE))")))
          (it "Errors for a quoted lambda in a list"
            (if (version< (org-version) "9.3")
                (expect (open-link quoted-lambda-in-list-link)
                        :to-throw 'error '("CAUTION: Link not opened because unsafe sort parameter detected: ((quote (lambda (_ _) (error UNSAFE))))"))
              (expect (open-link quoted-lambda-in-list-link)
                      :to-throw 'error '("CAUTION: Link not opened because unsafe sort parameter detected: ('(lambda (_ _) (error UNSAFE)))"))))
          (it "Errors for an unquoted lambda in a list"
            (expect (open-link unquoted-lambda-in-list-link)
                    :to-throw 'error '("CAUTION: Link not opened because unsafe sort parameter detected: ((lambda nil (error UNSAFE)))")))))))

  (describe "View saving/loading"
    :var* ((temp-dir (make-temp-file "test-org-ql-" 'dir))
           (temp-filenames (cl-loop for file in '("test1.org" "test2.org")
                                    collect (expand-file-name file temp-dir)))
           (file-contents (with-temp-buffer
                            (insert "#+TITLE: Test data\n\n"
                                    "* TODO Heading 1\n"
                                    "Heading 1 text.\n\n"
                                    "* Heading 2\n"
                                    "Heading 2 text.\n")
                            (buffer-string))))

    ;; This section will test saving and loading search views by a few different
    ;; means.  In each one, the values of these buffer-local variables will be
    ;; stored before saving a view and compared after loading it:

    ;; - org-ql-view-buffers-files
    ;; - org-ql-view-query
    ;; - org-ql-view-sort
    ;; - org-ql-view-super-groups
    ;; - org-ql-view-title

    ;; `org-ql-view-buffers-files' needs to be tested with these kinds of values:

    ;; - A buffer
    ;; - A string (filename)
    ;; - List of buffers
    ;; - List of strings (filenames)
    ;; - Combination

    ;; `org-ql-view-query' needs to be tested with these kinds of values:

    ;; - String query
    ;; - Sexp query

    ;; `org-ql-view-sort' needs to be tested with these kinds of values:

    ;; - Symbol
    ;; - List of symbols

    ;; `org-ql-view-super-groups' can probably be tested with nearly
    ;; any list value (because this set of tests is not intended to
    ;; test link safety).

    ;; TODO: `org-ql-view-title' needs to be tested with a string value.

    (before-all
      (dolist (filename temp-filenames)
        (with-temp-file filename
          (insert file-contents))))

    (after-all
      (delete-directory temp-dir 'recursive))

    (describe "Bookmarks"
      :var ((title "ORG-QL-TEST")
            bookmark-alist
            view-buffer)

      (before-each
        (setf view-buffer (get-buffer-create "*TEST*"))
        (dolist (filename temp-filenames)
          ;; Kill any existing buffers visiting these files.
          (when-let ((buffer (find-file-noselect filename 'nowarn)))
            (kill-buffer buffer))))

      (cl-flet ((var-after-bookmark-set-and-jump
                 (var buffers-files query &key sort super-groups)
                 (org-ql-search buffers-files query
                   :super-groups super-groups
                   :sort sort :title title :buffer view-buffer)
                 (set-buffer view-buffer)
                 (bookmark-set title)
                 (kill-buffer)
                 (bookmark-jump title)
                 (buffer-local-value var (get-buffer (concat "*Org QL View: " title "*")))))

        (describe "Grouping"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (super-groups '((:auto-priority))))
          (it "is restored"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-super-groups temp-filenames query
                                                     :super-groups super-groups)
                    :to-equal super-groups)))

        (describe "Queries"
          :var ((string-query "todo:TODO regexp:heading")
                (sexp-query '(and (todo "TODO") (regexp "heading"))))
          (it "Sexps match"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-query temp-filenames sexp-query) :to-equal sexp-query))
          (it "Strings match"
            ;; NOTE: This test includes the string query being replaced with its sexp form after the query is run.
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-query temp-filenames string-query)
                    :to-equal sexp-query)))

        (describe "Sorting"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (sorter 'todo)
                (multiple-sorters '(todo priority)))
          (it "One sorter is restored"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-sort temp-filenames query :sort sorter)
                    :to-equal sorter))
          (it "Multiple sorters are restored"
            ;; NOTE: This test includes the string query being replaced with its sexp form after the query is run.
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-sort temp-filenames query :sort multiple-sorters)
                    :to-equal multiple-sorters)))

        (describe "Buffers/Files"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (one-filename (car temp-filenames)))
          (it "One filename matches"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-buffers-files one-filename query)
                    :to-equal one-filename))
          (it "A list of filenames matches"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-buffers-files temp-filenames query)
                    :to-equal temp-filenames))
          ;; NOTE: These actually bookmark the filenames backing the buffers.
          (it "One buffer matches"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-buffers-files
                                                     (find-file-noselect (car temp-filenames)) query)
                    :to-equal one-filename))
          (it "A list of buffers matches"
            (expect (var-after-bookmark-set-and-jump 'org-ql-view-buffers-files
                                                     (mapcar #'find-file-noselect temp-filenames) query)
                    :to-equal temp-filenames)))))

    (describe "Dynamic blocks"
      (describe "warn about sexp queries"

        (cl-flet ((test-dblock
                   (&optional input) (with-current-buffer (get-buffer-create "*TEST DBLOCK*")
                                       (erase-buffer)
                                       (org-mode)
                                       (insert "* TODO Heading 1\n\n"
                                               "#+BEGIN: org-ql :query (or (todo) (regexp \"Heading\")) :columns (todo)\n"
                                               "#+END:")
                                       (goto-char (point-min))
                                       (forward-line 2)
                                       (with-simulated-input input
                                         (org-dblock-update))
                                       (kill-buffer))))

          (it "when org-ql-ask-unsafe-queries is non-nil"
            ;; TODO: Should the query be converted to string form if possible and only warn if not?
            (let ((org-ql-ask-unsafe-queries t))
              (expect (test-dblock "no RET")
                      :to-throw 'user-error '("Query aborted by user"))))

          (it "unless org-ql-ask-unsafe-queries is nil"
            ;; TODO: Should the query be converted to string form if possible and only warn if not?
            (let ((org-ql-ask-unsafe-queries nil))
              (expect (test-dblock)
                      :not :to-throw))))))

    (describe "Links"
      ;; Not sure if this binding works.
      :var ((title "ORG-QL-TEST")
            (view-buffer-name "*TEST VIEW BUFFER*")
            link-buffer view-buffer)

      (before-each
        (kill-buffer (get-buffer view-buffer-name))
        (setf link-buffer (get-buffer-create "*TEST LINK BUFFER*")
              view-buffer (get-buffer-create view-buffer-name))
        (with-current-buffer link-buffer
          (erase-buffer)
          (insert "* TODO Test heading\n\n")
          (org-mode)))

      (cl-flet* ((open-link-in
                  (link buffer input)
                  ;; Org REDUCED THE NUMBER OF ARGUMENTS TO `org-open-link-from-string'!  That BREAKS BACKWARD
                  ;; COMPATIBILITY!  So I have to make my own function so these tests can work across Org versions!
                  (with-current-buffer buffer
                    (erase-buffer)
                    (org-mode)
                    (insert "* TODO Test heading\n\n")
                    (insert link)
                    (backward-char 1)
                    (with-simulated-input input
                      (org-open-at-point))))

                 (var-after-link-save-open
                  (var buffers-files query &key sort super-groups
                       (buffer link-buffer) (store-input "RET") open-input)
                  (org-ql-search buffers-files query
                    :super-groups super-groups
                    :sort sort :title title :buffer view-buffer)
                  (with-current-buffer view-buffer
                    (cl-assert (member '("org-ql-search" :follow org-ql-view--link-open :store org-ql-view--link-store)
                                       org-link-parameters)
                               t)
                    (with-simulated-input store-input
                      ;; Avoid writing "Stored: ..." to test output.
                      (let ((inhibit-message t))
                        (call-interactively #'org-store-link nil)))
                    (kill-buffer))
                  (cl-assert (and org-stored-links (caar org-stored-links)) t)
                  (open-link-in (caar org-stored-links) buffer open-input)
                  (with-current-buffer (get-buffer (concat "*Org QL View: " title "*"))
                    (prog1 (buffer-local-value var (current-buffer))
                      (kill-buffer)))))

        (describe "Queries"
          :var ((string-query "todo:TODO regexp:heading")
                (string-query-in-sexp-form '(and (todo "TODO") (regexp "heading")))
                ;; NOTE: The sexp query must be one that `org-ql--query-sexp-to-string'
                ;; can't convert to a string.
                (sexp-query '(or (todo "TODO") (regexp "heading"))))

          (describe "in sexp form"

            (describe "prompt when `org-ql-ask-unsafe-queries' is non-nil"
              :var ((org-ql-ask-unsafe-queries t))

              (it "and signal an error when rejected by user"
                (expect (var-after-link-save-open 'org-ql-view-query temp-filenames sexp-query
                                                  :open-input "no RET")
                        :to-throw 'user-error '("Query aborted by user")))
              (it "and run when approved by user"
                (expect (var-after-link-save-open 'org-ql-view-query temp-filenames sexp-query
                                                  :open-input "yes RET")
                        :to-equal sexp-query)))

            (it "don't prompt when `org-ql-ask-unsafe-queries' is nil"
              (let ((org-ql-ask-unsafe-queries nil))
                (expect (var-after-link-save-open 'org-ql-view-query temp-filenames sexp-query)
                        :to-equal sexp-query)))

            (it "match after restoring"
              (let ((org-ql-ask-unsafe-queries nil)) ; Disable safety check for this test.
                (expect (var-after-link-save-open 'org-ql-view-query temp-filenames sexp-query)
                        :to-equal sexp-query))))

          (describe "in raw sexp form"
            ;; These tests guard against manually constructed queries, which could
            ;; theoretically be different than the ones resulting from storing links (i.e. they
            ;; might have encoding).  These tests probably aren't necessary, but it's good to
            ;; be extra careful, because an evil sexp could be harmful if eval'ed.
            :var ((raw-link "[[org-ql-search:(todo \"TODO\")]]"))

            (describe "prompt when `org-ql-ask-unsafe-queries' is non-nil"
              :var ((org-ql-ask-unsafe-queries t))

              (it "and signal an error when rejected by user"
                (expect (open-link-in raw-link link-buffer "no RET")
                        :to-throw 'user-error '("Query aborted by user")))
              (it "and run when approved by user"
                (expect (open-link-in raw-link link-buffer "yes RET")
                        :not :to-throw)))

            (it "don't prompt when `org-ql-ask-unsafe-queries' is nil"
              (let ((org-ql-ask-unsafe-queries nil))
                (expect (open-link-in raw-link link-buffer "no RET")
                        :not :to-throw)))

            (it "match after restoring"
              (let ((org-ql-ask-unsafe-queries nil) ; Disable safety check for this test.
                    (org-ql-view-buffer view-buffer))
                (expect (open-link-in raw-link link-buffer "no RET")
                        :not :to-throw)
                (expect (buffer-local-value 'org-ql-view-query view-buffer)
                        :to-equal '(todo "TODO"))))

            (it "could be evil when not prompted about"
              ;; This test confirms what *could* happen if these checks weren't in place.
              (let ((org-ql-ask-unsafe-queries nil))
                (expect (open-link-in "[[org-ql-search:(error \"EVIL!\")]]" link-buffer nil)
                        :to-throw 'error '("EVIL!")))))

          (describe "in string form"
            (it "match after restoring"
              ;; NOTE: This test includes the string query being replaced with its sexp form after the query is run.
              (expect (var-after-link-save-open 'org-ql-view-query temp-filenames string-query)
                      :to-equal string-query-in-sexp-form))))

        (describe "Grouping"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (super-groups '((:auto-priority))))
          (it "is restored"
            (expect (var-after-link-save-open 'org-ql-view-super-groups temp-filenames query
                                              :super-groups super-groups)
                    :to-equal super-groups)))

        (describe "Sorting"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (sorter 'todo)
                (multiple-sorters '(todo priority)))
          (it "One sorter is restored"
            (expect (var-after-link-save-open 'org-ql-view-sort temp-filenames query :sort sorter)
                    :to-equal sorter))
          (it "Multiple sorters are restored"
            ;; NOTE: This test includes the string query being replaced with its sexp form after the query is run.
            (expect (var-after-link-save-open 'org-ql-view-sort temp-filenames query :sort multiple-sorters)
                    :to-equal multiple-sorters)))

        (describe "Buffers/Files"
          :var ((query '(and (todo "TODO") (regexp "heading")))
                (one-filename (car temp-filenames)))
          (it "Can search a file by filename"
            (expect (var-after-link-save-open 'org-ql-view-buffers-files one-filename query
                                              :store-input "M-n M-n RET")
                    :to-equal one-filename))
          (it "Can search multiple files by filename"
            (expect (var-after-link-save-open 'org-ql-view-buffers-files temp-filenames query
                                              :store-input "M-n M-n RET")
                    :to-equal temp-filenames))
          (it "Can search buffer containing the link"
            ;; This is sort-of a special case because of how the test link-opening function works.
            (expect (var-after-link-save-open 'org-ql-view-buffers-files one-filename query
                                              :buffer link-buffer)
                    :to-equal link-buffer)))))

    ;; MAYBE: Also test `org-ql-views', although I already know it works now.
    ;; (describe "org-ql-views")
    ))

;; Local Variables:
;; truncate-lines: t
;; End:

;;; org-ql.el ends here
