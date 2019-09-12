;;; org-ql.el --- Org Query Language, search command, and agenda-like view  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql
;; Version: 0.3-pre
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.0") (s "1.12.0") (ts "0.2"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; `org-ql' is a lispy query language for Org files.  It allows you to
;; find Org entries matching certain criteria and return a list of
;; them or perform actions on them.  Commands are also provided which
;; display a buffer with matching results, similar to an Org Agenda
;; buffer.

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

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-habit)
(require 'seq)
(require 'subr-x)

(require 'dash)
(require 'ts)

;;;; Compatibility

(if (version< org-version "9.2")
    (progn
      (defalias 'org-timestamp-to-time #'org-timestamp--to-internal-time)
      (defun org-ql--get-tags (&optional pos local)
        (org-get-tags-at pos local)))
  (defun org-ql--get-tags (&rest _ignore)
    "Call `org-get-tags', ignoring arguments."
    (org-get-tags)))

;;;; Constants

;; Note the use of the `rx' `blank' keyword, which matches "horizontal" whitespace.

(defconst org-ql-tsr-regexp-inactive
  (concat org-ts-regexp-inactive "\\(--?-?"
	  org-ts-regexp-inactive "\\)?")
  ;; MAYBE: Propose this for org.el.
  "Regular expression matching an inactive timestamp or timestamp range.")

(defconst org-ql-clock-regexp
  (rx bol (0+ blank) "CLOCK:" (group (1+ not-newline)))
  "Regular expression matching Org \"CLOCK:\" lines.
Like `org-clock-line-re', but matches the timestamp range in a
match group.")

(defconst org-ql-planning-regexp
  (rx bol (0+ blank) (or "CLOSED" "DEADLINE" "SCHEDULED") ":" (1+ blank) (group (1+ not-newline)))
  "Regular expression matching Org \"planning\" lines.
That is, \"CLOSED:\", \"DEADLINE:\", or \"SCHEDULED:\".")

;;;; Variables

(defvar org-ql--today nil)

(defvar org-ql-use-preamble t
  ;; MAYBE: Naming things is hard.  There must be a better term than "preamble."
  "Use query preambles to speed up searches.
May be disabled for debugging, benchmarks, etc.")

(defvar org-ql-cache (make-hash-table :weakness 'key)
  ;; IIUC, setting weakness to `key' means that, when a buffer is closed,
  ;; its entries will be removed from this table at the next GC.
  "Query cache, keyed by buffer.
Each value is a list of the buffer's modified tick and another
hash table, keyed by arguments passed to
`org-ql--select-cached'.")

(defvar org-ql-tags-cache (make-hash-table :weakness 'key)
  "Per-buffer tags cache.
Keyed by buffer.  Each value is a cons of the buffer's modified
tick, and another hash table keyed on buffer position, whose
values are a list of two lists, inherited tags and local tags, as
strings.")

(defvar org-ql-predicates
  (list (list :name 'org-back-to-heading :fn (symbol-function 'org-back-to-heading)))
  "Plist of predicates, their corresponding functions, and their docstrings.
This list should not contain any duplicates.")

;;;; Customization

(defgroup org-ql nil
  "Customization for `org-ql'."
  :group 'org
  :link '(url-link "https://github.com/alphapapa/org-ql"))

;;;; Macros

(cl-defmacro org-ql--defpred (name args docstring &rest body)
  "Define an `org-ql' selector predicate named `org-ql--predicate-NAME'.
ARGS is a `cl-defun'-style argument list.  DOCSTRING is the
function's docstring.  BODY is the body of the predicate.

Predicates will be called with point on the beginning of an Org
heading and should return non-nil if the heading's entry is a
match."
  (declare (debug (symbolp listp stringp def-body))
           (indent defun))
  (let ((fn-name (intern (concat "org-ql--predicate-" (symbol-name name))))
        (pred-name (intern (symbol-name name))))
    `(progn
       (push (list :name ',pred-name :fn ',fn-name :docstring ,docstring :args ',args) org-ql-predicates)
       (cl-defun ,fn-name ,args ,docstring ,@body))))

;;;###autoload
(cl-defmacro org-ql (buffers-or-files query &key sort narrow action)
  "Expands into a call to `org-ql-select' with the same arguments.
For convenience, arguments should be unquoted."
  (declare (indent defun))
  `(org-ql-select ,buffers-or-files
     ',query
     :action ',action
     :narrow ,narrow
     :sort ',sort))

;;;; Functions

(define-hash-table-test 'org-ql-hash-test #'equal (lambda (args)
                                                    (sxhash-equal (prin1-to-string args))))

;;;###autoload
(cl-defun org-ql-select (buffers-or-files query &key action narrow sort)
  "Return items matching QUERY in BUFFERS-OR-FILES.

BUFFERS-OR-FILES is a one or a list of files and/or buffers.

QUERY is an `org-ql' query sexp (quoted, since this is a
function).

ACTION is a function which is called on each matching entry with
point at the beginning of its heading.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to calling
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-agenda--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

If NARROW is non-nil, buffers are not widened (the default is to
widen and search the entire buffer).

SORT is either nil, in which case items are not sorted; or one or
a list of defined `org-ql' sorting methods (`date', `deadline',
`scheduled', `todo', `priority', or `random'); or a user-defined
comparator function that accepts two items as arguments and
returns nil or non-nil."
  (declare (indent defun))
  (-let* ((buffers (->> (cl-typecase buffers-or-files
                          (null (list (current-buffer)))
                          (list buffers-or-files)
                          (otherwise (list buffers-or-files)))
                        (--map (cl-etypecase it
                                 (buffer it)
                                 (string (or (find-buffer-visiting it)
                                             (when (file-readable-p it)
                                               ;; It feels unintuitive that `find-file-noselect' returns
                                               ;; a buffer if the filename doesn't exist.
                                               (find-file-noselect it))
                                             (user-error "Can't open file: %s" it)))))
                        ;; Ignore special/hidden buffers.
                        (--remove (string-prefix-p " " (buffer-name it)))))
          (query (org-ql--pre-process-query query))
          ((query preamble-re) (org-ql--query-preamble query))
          (predicate (org-ql--query-predicate query))
          (action (pcase action
                    ;; NOTE: These two lambdas are backquoted to prevent "unused lexical
                    ;; variable" warnings from byte-compilation, because they don't use
                    ;; all of the variables from their enclosing scope.
                    ('element-with-markers (byte-compile
                                            `(lambda (&rest _ignore)
                                               (org-ql--add-markers
                                                (org-element-headline-parser (line-end-position))))))
                    ((or 'nil 'element) (byte-compile
                                         `(lambda (&rest _ignore)
                                            (org-element-headline-parser (line-end-position)))))
                    ((pred functionp) action)
                    ((and (pred listp) (guard (or (special-form-p (car action))
                                                  (macrop (car action))
                                                  (functionp (car action)))))
                     (byte-compile
                      `(lambda (&rest _ignore)
                         ,action)))
                    (_ (user-error "Invalid action form: %s" action))))
          (org-ql--today (ts-now))
          (items (->> buffers
                      (--map (with-current-buffer it
                               (unless (derived-mode-p 'org-mode)
                                 (user-error "Not an Org buffer: %s" (buffer-name)))
                               (org-ql--select-cached :query query :preamble-re preamble-re
                                                      :predicate predicate :action action :narrow narrow)))
                      (-flatten-n 1))))
    ;; Sort items
    (pcase sort
      (`nil items)
      ((or 'date 'deadline 'scheduled 'todo 'priority 'random
           (guard (cl-loop for elem in sort
                           always (memq elem '(date deadline scheduled todo priority random)))))
       ;; Default sorting functions
       (org-ql--sort-by items (-list sort)))
      ;; Sort by user-given comparator.
      ((pred functionp) (sort items sort))
      (_ (user-error "SORT must be either nil, one or a list of the defined sorting methods (see documentation), or a comparison function of two arguments")))))

;;;###autoload
(cl-defun org-ql-query (&key (select 'element-with-markers) from where narrow order-by)
  "Like `org-ql-select', but arguments are named more like a SQL query.

SELECT corresponds to the `org-ql-select' argument ACTION.  It is
the function called on matching headings, the results of which
are returned by this function.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-agenda--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

FROM corresponds to the `org-ql-select' argument BUFFERS-OR-FILES.
It may be one or a list of file paths and/or buffers.

WHERE corresponds to the `org-ql-select' argument QUERY.  It
should be an `org-ql' query sexp.

ORDER-BY corresponds to the `org-ql-select' argument SORT, which
see.

NARROW corresponds to the `org-ql-select' argument NARROW."
  (declare (indent 0))
  (org-ql-select from where
    :action select
    :narrow narrow
    :sort order-by))

(defun org-ql--pre-process-query (query)
  "Return QUERY having been pre-processed.
Replaces bare strings with (regexp) selectors, and appropriate
`ts'-related selectors."
  ;; This is unsophisticated, but it works.
  ;; TODO: Maybe query pre-processing should be done in one place,
  ;; rather than here and in --query-predicate.
  ;; NOTE: Don't be scared by the `pcase' patterns!  They make this
  ;; all very easy once you grok the backquoting and unquoting.
  (cl-labels ((rec (element)
                   (pcase element
                     (`(or . ,clauses) `(or ,@(mapcar #'rec clauses)))
                     (`(and . ,clauses) `(and ,@(mapcar #'rec clauses)))
                     (`(not . ,clauses) `(not ,@(mapcar #'rec clauses)))
                     (`(when ,condition . ,clauses) `(when ,(rec condition)
                                                       ,@(mapcar #'rec clauses)))
                     (`(unless ,condition . ,clauses) `(unless ,(rec condition)
                                                         ,@(mapcar #'rec clauses)))
                     ;; TODO: Combine (regexp) when appropriate (i.e. inside an OR, not an AND).
                     ((pred stringp) `(regexp ,element))
                     ;; Quote children queries so the user doesn't have to.
                     (`(children ,query) `(children ',query))
                     (`(children) '(children (lambda () t)))
                     (`(descendants ,query) `(descendants ',query))
                     (`(descendants) '(descendants (lambda () t)))
                     ;; Timestamp-based predicates.  I think this is the way that makes the most sense:
                     ;; set the limit to N days in the future, adjusted to 23:59:59 (since Org doesn't
                     ;; support timestamps down to the second, anyway, there should be no need to adjust
                     ;; it forward to 00:00:00 of the next day).  That way, e.g. if it's Monday at 3 PM,
                     ;; and N is 1, rather than showing items up to 3 PM Tuesday, it will show items any
                     ;; time on Tuesday.  If this isn't desired, the user can pass a specific timestamp.
                     (`(,(and pred (or 'clocked 'closed))
                        ,(and num-days (pred numberp)))
                      ;; (clocked) and (closed) implicitly look into the past.
                      (let ((from (->> (ts-now)
                                       (ts-adjust 'day (* -1 num-days))
                                       (ts-apply :hour 0 :minute 0 :second 0))))
                        `(,pred :from ,from)))
                     (`(deadline auto)
                      ;; Use `org-deadline-warning-days' as the :to arg.
                      (let ((to (->> (ts-now)
                                     (ts-adjust 'day org-deadline-warning-days)
                                     (ts-apply :hour 23 :minute 59 :second 59))))
                        `(deadline-warning :to ,to)))
                     (`(,(and pred (or 'deadline 'scheduled 'planning))
                        ,(and num-days (pred numberp)))
                      (let ((to (->> (ts-now)
                                     (ts-adjust 'day num-days)
                                     (ts-apply :hour 23 :minute 59 :second 59))))
                        `(,pred :to ,to)))

                     ;; Priorities
                     (`(priority) ;; Match any defined priority by comparing to C.
                      ;; Note that we quote the comparator again for consistency.
                      `(priority '>= "C"))
                     (`(priority ,_letter) element)
                     (`(priority ,comparator ,letter)
                      ;; Quote comparator.
                      `(priority ',comparator ,letter))

                     ;; Tags.
                     (`(,(or 'tags-all 'tags&) . ,tags) `(and ,@(--map `(tags ,it) tags)))
                     ;; MAYBE: -all versions for inherited and local.
                     ;; Inherited and local predicate aliases.
                     (`(,(or 'tags-i 'itags 'inherited-tags) . ,tags) `(tags-inherited ,@tags))
                     (`(,(or 'tags-l 'ltags 'local-tags) . ,tags) `(tags-local ,@tags))

                     ;; Timestamps
                     (`(,(or 'ts-active 'ts-a) . ,rest) `(ts :type active ,@rest))
                     (`(,(or 'ts-inactive 'ts-i) . ,rest) `(ts :type inactive ,@rest))
                     ;; Any other form: passed through unchanged.
                     (_ element))))
    (rec query)))

(defmacro org-ql--from-to-on ()
  "For internal use.
Expands into a form that processes arguments to timestamp-related
predicates."
  ;; Several attempts to use `cl-macrolet' and `cl-symbol-macrolet' failed, so I
  ;; resorted to this top-level macro.  It will do for now.
  `(progn
     (when on
       (setq from on
             to on))
     (when from
       (setq from (pcase from
                    ((pred stringp) (ts-parse-fill 'begin from))
                    ((pred numberp) (->> (ts-now)
                                         (ts-adjust 'day from)
                                         (ts-apply :hour 0 :minute 0 :second 0)))
                    ((pred ts-p) from)
                    ('today (->> (ts-now)
                                 (ts-apply :hour 0 :minute 0 :second 0))))))
     (when to
       (setq to (pcase to
                  ((pred stringp) (ts-parse-fill 'end to))
                  ((pred numberp) (->> (ts-now)
                                       (ts-adjust 'day to)
                                       (ts-apply :hour 23 :minute 59 :second 59)))
                  ((pred ts-p) to)
                  ('today (->> (ts-now)
                               (ts-apply :hour 23 :minute 59 :second 59))))))))

(defun org-ql--query-predicate (query)
  "Return predicate function for QUERY."
  (byte-compile
   `(lambda ()
      (cl-macrolet ((clocked (&key from to on)
                             (org-ql--from-to-on)
                             `(org-ql--predicate-clocked :from ,from :to ,to))
                    (closed (&key from to on)
                            (org-ql--from-to-on)
                            `(org-ql--predicate-closed :from ,from :to ,to))
                    (deadline (&key from to on)
                              (org-ql--from-to-on)
                              `(org-ql--predicate-deadline :from ,from :to ,to))
                    (planning (&key from to on)
                              (org-ql--from-to-on)
                              `(org-ql--predicate-planning :from ,from :to ,to))
                    (scheduled (&key from to on)
                               (org-ql--from-to-on)
                               `(org-ql--predicate-scheduled :from ,from :to ,to))
                    (ts (&key from to on (type 'both))
                        (org-ql--from-to-on)
                        `(org-ql--predicate-ts :from ,from :to ,to
                                               :regexp ,(pcase type
                                                          ('both org-tsr-regexp-both)
                                                          ('active org-tsr-regexp)
                                                          ('inactive org-ql-tsr-regexp-inactive)))))
        ,query))))

(defun org-ql--query-preamble (query)
  "Return (QUERY PREAMBLE) for QUERY.
When QUERY has a clause with a corresponding preamble, and it's
appropriate to use one (i.e. the clause is not in an `or'),
replace the clause with a preamble."
  (pcase org-ql-use-preamble
    ('nil (list query nil))
    (_ (let (org-ql-preamble)
         (cl-labels ((rec (element)
                          (or (when org-ql-preamble
                                ;; Only one preamble is allowed
                                element)
                              (pcase element
                                (`(or _) element)
                                (`(clocked . ,_)
                                 (setq org-ql-preamble org-ql-clock-regexp)
                                 element)
                                (`(closed . ,_)
                                 (setq org-ql-preamble org-closed-time-regexp)
                                 ;; Return element, because the predicate still needs testing.
                                 element)
                                (`(deadline . ,_)
                                 (setq org-ql-preamble org-deadline-time-regexp)
                                 ;; Return element, because the predicate still needs testing.
                                 element)
                                (`(regexp . ,regexps)
                                 ;; Search for first regexp, then confirm with predicate.
                                 (setq org-ql-preamble (car regexps))
                                 element)
                                (`(todo . ,(and todo-keywords (guard todo-keywords)))
                                 ;; FIXME: With case-folding, a query like (todo "WAITING") can find a
                                 ;; non-todo heading named "Waiting".  For correctness, we could test the
                                 ;; predicate anyway, but that would negate some of the speed, and in
                                 ;; most cases it probably won't matter, so I'm leaving it this way for
                                 ;; now.  Maybe we should use a special variable to control case-folding.
                                 (setq org-ql-preamble
                                       (rx-to-string `(seq bol (1+ "*") (1+ space) (or ,@todo-keywords) (or " " eol))
                                                     t))
                                 ;; Return nil, don't test the predicate.
                                 nil)
                                (`(habit)
                                 ;; TODO: Move regexp to const.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (0+ space) ":STYLE:" (1+ space)
                                                                           "habit" (0+ space) eol)))
                                 nil)

                                ;; Heading text.
                                (`(heading ,regexp)
                                 ;; Only one regexp: match with preamble only.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ blank) (0+ nonl)
                                                                           ,regexp)
                                                                     'no-group))
                                 nil)
                                (`(heading . ,regexps)
                                 ;; Multiple regexps: use preamble to match against first
                                 ;; regexp, then let the predicate match the rest.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ blank) (0+ nonl)
                                                                           ,(car regexps))
                                                                     'no-group))
                                 element)

                                ;; Heading levels.
                                (`(level ,comparator-or-num ,num)
                                 (let ((repeat (pcase comparator-or-num
                                                 ('< `(repeat 1 ,(1- num) "*"))
                                                 ('<= `(repeat 1 ,num "*"))
                                                 ('> `(>= ,(1+ num) "*"))
                                                 ('>= `(>= ,num "*"))
                                                 ((pred integerp) `(repeat ,comparator-or-num ,num "*")))))
                                   (setq org-ql-preamble (rx-to-string `(seq bol ,repeat " ") t))
                                   ;; Return nil, because we don't need to test the predicate.
                                   nil))
                                (`(level ,num)
                                 (setq org-ql-preamble (rx-to-string `(seq bol (repeat ,num "*") " ") t))
                                 nil)

                                ;; Planning lines.
                                (`(planning . ,_)
                                 (setq org-ql-preamble org-ql-planning-regexp)
                                 ;; Return element, because the predicate still needs testing.
                                 element)

                                ;; Priorities.
                                ;; NOTE: This only accepts A, B, or C.  I haven't seen
                                ;; other priorities in the wild, so this will do for now.
                                (`(priority)
                                 ;; Any priority.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ blank) "[#" (in "ABC") "]") t))
                                 nil)
                                (`(priority ,letter)
                                 ;; Specific priority without comparator.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ blank)
                                                                           (optional (1+ upper) (1+ blank))
                                                                           "[#" ,letter "]") t))
                                 nil)
                                (`(priority ,comparator ,letter)
                                 (let* ((priority-letters '("A" "B" "C"))
                                        (index (-elem-index letter priority-letters))
                                        ;; NOTE: Higher priority == lower number.
                                        ;; NOTE: Because we need to support both preamble-based queries and
                                        ;; regular predicate ones, we work around an idiosyncrasy of query
                                        ;; pre-processing by accepting both quoted and double-quoted comparator
                                        ;; function symbols.  Not the most elegant solution, but it works.
                                        (priorities (s-join "" (pcase comparator
                                                                 ((or '= ''=) (list letter))
                                                                 ((or '> ''>) (cl-subseq priority-letters 0 index))
                                                                 ((or '>= ''>=) (cl-subseq priority-letters 0 (1+ index)))
                                                                 ((or '< ''<) (cl-subseq priority-letters (1+ index)))
                                                                 ((or '<= ''<=) (cl-subseq priority-letters index))))))
                                   (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ blank) (optional (1+ upper) (1+ blank))
                                                                             "[#" (in ,priorities) "]") t))
                                   nil))

                                ;; Properties.
                                (`(property ,property ,value)
                                 ;; We do NOT return nil, because the predicate still needs to be tested,
                                 ;; because the regexp could match a string not inside a property drawer.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (0+ space) ":" ,property ":"
                                                                           (1+ space) ,value (0+ space) eol)))
                                 element)
                                (`(property ,property)
                                 ;; We do NOT return nil, because the predicate still needs to be tested,
                                 ;; because the regexp could match a string not inside a property drawer.
                                 ;; NOTE: The preamble only matches if there appears to be a value.
                                 ;; A line like ":ID: " without any other text does not match.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (0+ space) ":" ,property ":" (1+ space)
                                                                           (minimal-match (1+ not-newline)) eol)))
                                 element)
                                ;; MAYBE: Support (property) without args.
                                ;; (`(property)
                                ;;  ;; We do NOT return nil, because the predicate still needs to be tested,
                                ;;  ;; because the regexp could match a string not inside a property drawer.
                                ;;  ;; NOTE: The preamble only matches if there appears to be a value.
                                ;;  ;; A line like ":ID: " without any other text does not match.
                                ;;  (setq org-ql-preamble (rx-to-string `(seq bol (0+ space) ":" (1+ (not (or space ":"))) ":"
                                ;;                                            (1+ space) (minimal-match (1+ not-newline)) eol)))
                                ;;  element)
                                (`(scheduled . ,_)
                                 (setq org-ql-preamble org-scheduled-time-regexp)
                                 ;; Return element, because the predicate still needs testing.
                                 element)
                                (`((or 'tags-local 'local-tags 'tags-l 'ltags) . ,tags)
                                 ;; When searching for local, non-inherited tags, we can
                                 ;; search directly to headings containing one of the tags.
                                 (setq org-ql-preamble (rx-to-string `(seq bol (1+ "*") (1+ space) (1+ not-newline)
                                                                           ":" (or ,@tags) ":")
                                                                     t))
                                 ;; Return nil, because we don't need to test the predicate.
                                 nil)
                                (`(ts . ,rest)
                                 (setq org-ql-preamble (pcase (plist-get rest :type)
                                                         ((or 'nil 'both) org-tsr-regexp-both)
                                                         ('active org-tsr-regexp)
                                                         ('inactive org-ql-tsr-regexp-inactive)))
                                 ;; Predicate needs testing only when args are present.
                                 (-let (((&keys :from :to :on) rest))
                                   (when (or from to on)
                                     element)))
                                (`(and . ,rest)
                                 (let ((clauses (mapcar #'rec rest)))
                                   `(and ,@(-non-nil clauses))))
                                (_ element)))))
           (setq query (pcase (mapcar #'rec (list query))
                         ((or `(nil)
                              `((nil))
                              `((and))
                              `((or)))
                          t)
                         (query (-flatten-n 1 query))))
           (list query org-ql-preamble))))))

(defun org-ql--select-cached (&rest args)
  "Return results for ARGS and current buffer using cache."
  ;; MAYBE: Timeout cached queries.  Probably not necessarily since they will be removed when a
  ;; buffer is closed, or when a query is run after modifying a buffer.
  (-let* (((&plist :query :preamble-re :action :narrow) args)
          (query-cache-key
           ;; The key must include the preamble, because some queries are replaced by
           ;; the preamble, leaving a nil query, which would make the key ambiguous.
           (list :query query :preamble-re preamble-re :action action
                 (if narrow
                     ;; Use bounds of narrowed portion of buffer.
                     (cons (point-min) (point-max))
                   nil))))
    (if-let* ((buffer-cache (gethash (current-buffer) org-ql-cache))
              (query-cache (cadr buffer-cache))
              (modified-tick (car buffer-cache))
              (buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
              (cached-result (gethash query-cache-key query-cache)))
        (pcase cached-result
          ('org-ql-nil nil)
          (_ cached-result))
      (let ((new-result (apply #'org-ql--select args)))
        (cond ((or (not query-cache)
                   (not buffer-unmodified-p))
               (puthash (current-buffer)
                        (list (buffer-modified-tick)
                              (let ((table (make-hash-table :test 'org-ql-hash-test)))
                                (puthash query-cache-key (or new-result 'org-ql-nil) table)
                                table))
                        org-ql-cache))
              (t (puthash query-cache-key (or new-result 'org-ql-nil) query-cache)))
        new-result))))

(cl-defun org-ql--select (&key preamble-re predicate action narrow &allow-other-keys)
  "Return results of mapping function ACTION across entries in current buffer matching function PREDICATE.
If NARROW is non-nil, buffer will not be widened."
  ;; Since the mappings are stored in the variable `org-ql-predicates', macros like `flet'
  ;; can't be used, so we do it manually (this is same as the equivalent `flet' expansion).
  ;; Mappings are stored in the variable because it allows predicates to be defined with a
  ;; macro, which allows documentation to be easily generated for them.

  ;; MAYBE: Lift the `flet'-equivalent out of this function so it isn't done for each buffer.
  (let (orig-fns)
    (--each org-ql-predicates
      ;; Save original function mappings.
      (let ((name (plist-get it :name)))
        (push (list :name name :fn (symbol-function name)) orig-fns)))
    (unwind-protect
        (progn
          (--each org-ql-predicates
            ;; Set predicate functions.
            (fset (plist-get it :name) (plist-get it :fn)))
          ;; Run query.
          (save-excursion
            (save-restriction
              (unless narrow
                (widen))
              (goto-char (point-min))
              (when (org-before-first-heading-p)
                (outline-next-heading))
              (if (not (org-at-heading-p))
                  (progn
                    ;; No headings in buffer: return nil.
                    (unless (string-prefix-p " " (buffer-name))
                      ;; Not a special, hidden buffer: show message, because if a user accidentally
                      ;; searches a buffer without headings, he might be confused.
                      (message "org-ql: No headings in buffer: %s" (current-buffer)))
                    nil)
                ;; Find matching entries.
                (cond (preamble-re (cl-loop while (re-search-forward preamble-re nil t)
                                            do (outline-back-to-heading 'invisible-ok)
                                            when (funcall predicate)
                                            collect (funcall action)
                                            do (outline-next-heading)))
                      (t (cl-loop when (funcall predicate)
                                  collect (funcall action)
                                  while (outline-next-heading))))))))
      (--each orig-fns
        ;; Restore original function mappings.
        (fset (plist-get it :name) (plist-get it :fn))))))

(defun org-ql--tags-at (position)
  "Return tags for POSITION in current buffer.
Returns cons (INHERITED-TAGS . LOCAL-TAGS)."
  ;; I'd like to use `-if-let*', but it doesn't leave non-nil variables
  ;; bound in the else clause, so destructured variables that are non-nil,
  ;; like found caches, are not available in the else clause.
  (if-let* ((buffer-cache (gethash (current-buffer) org-ql-tags-cache))
            (modified-tick (car buffer-cache))
            (tags-cache (cdr buffer-cache))
            (buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
            (cached-result (gethash position tags-cache)))
      ;; Found in cache: return them.
      (pcase cached-result
        ('org-ql-nil nil)
        (_ cached-result))
    ;; Not found in cache: get tags and cache them.
    (let* ((local-tags (or (org-ql--get-tags position 'local)
                           'org-ql-nil))
           (inherited-tags (or (when org-use-tag-inheritance
                                 (save-excursion
                                   (when (org-up-heading-safe)
                                     (-let* (((inherited local) (org-ql--tags-at (point)))
                                             (tags (when (or inherited local)
                                                     (cond ((and (listp inherited)
                                                                 (listp local))
                                                            (->> (append inherited local)
                                                                 -non-nil -uniq))
                                                           ((listp inherited) inherited)
                                                           ((listp local) local)))))
                                       (cl-typecase org-use-tag-inheritance
                                         (list (setf tags (-intersection tags org-use-tag-inheritance)))
                                         (string (setf tags (--select (string-match org-use-tag-inheritance it)
                                                                      tags))))
                                       (pcase org-tags-exclude-from-inheritance
                                         ('nil tags)
                                         (_ (-difference tags org-tags-exclude-from-inheritance)))))))
                               'org-ql-nil))
           (all-tags (list inherited-tags local-tags)))
      ;; Check caches again, because they may have been set now.
      ;; TODO: Is there a clever way we could avoid doing this, or is it inherently necessary?
      (setf buffer-cache (gethash (current-buffer) org-ql-tags-cache)
            modified-tick (car buffer-cache)
            tags-cache (cdr buffer-cache)
            buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
      (unless (and buffer-cache buffer-unmodified-p)
        ;; Buffer-local tags cache empty or invalid: make new one.
        (setf tags-cache (make-hash-table))
        (puthash (current-buffer)
                 (cons (buffer-modified-tick) tags-cache)
                 org-ql-tags-cache))
      (puthash position all-tags tags-cache))))

;;;;; Helpers

(defun org-ql--add-markers (element)
  "Return ELEMENT with Org marker text properties added.
ELEMENT should be an Org element like that returned by
`org-element-headline-parser'.  This function should be called
from within ELEMENT's buffer."
  ;; NOTE: `org-agenda-new-marker' works, until it doesn't, because...I don't know.  It sometimes
  ;; raises errors or returns markers that don't point into a buffer.  `copy-marker' always works,
  ;; of course, but maybe it will leave "dangling" markers, which could affect performance over
  ;; time?  I don't know, but for now, it seems that we have to use `copy-marker'.
  (let* ((marker (copy-marker (org-element-property :begin element)))
         (properties (--> (cadr element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (cadr element) properties)
    element))

(defun org-ql--sanity-check-form (form)
  "Signal error if any forms in FORM do not have preconditions met.
Or, when possible, fix the problem."
  (cl-flet ((check (symbol)
                   (cl-case symbol
                     ('done (unless org-done-keywords
                              ;; NOTE: This check needs to be done from within the Org buffer being checked.
                              (error "Variable `org-done-keywords' is nil.  Are you running this from an Org buffer?"))))))
    (cl-loop for elem in form
	     if (consp elem)
	     do (progn
		  (check (car elem))
		  (org-ql--sanity-check-form (cdr elem)))
	     else do (check elem))))

;;;;; Predicates

(org-ql--defpred children (query)
  "Return non-nil if current entry has children matching QUERY."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (when (org-goto-first-child)
        ;; Lisp makes this easy and elegant: all we do is modify the query,
        ;; nesting it inside an (and), and it doesn't descend into grandchildren.
        (let* ((level (org-current-level))
               (query (cl-typecase query
                        (byte-code-function `(and (level ,level)
                                                  (funcall ,query)))
                        (t `(and (level ,level)
                                 ,query)))))
          (catch 'found
            (org-ql-select (current-buffer)
              query
              :narrow t
              :action (lambda ()
                        (throw 'found t)))))))))

(org-ql--defpred descendants (query)
  "Return non-nil if current entry has descendants matching QUERY."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (when (org-goto-first-child)
        (narrow-to-region (point) (point-max))
        (catch 'found
          (org-ql-select (current-buffer)
            query
            :narrow t
            :action (lambda ()
                      (throw 'found t))))))))

(org-ql--defpred category (&rest categories)
  "Return non-nil if current heading is in one or more of CATEGORIES (a list of strings)."
  (when-let ((category (org-get-category (point))))
    (cl-typecase categories
      (null t)
      (otherwise (member category categories)))))

(org-ql--defpred path (&rest regexps)
  "Return non-nil if current heading's buffer's filename path matches any of REGEXPS (regexp strings).
Without arguments, return non-nil if buffer is file-backed."
  (when (buffer-file-name)
    (cl-typecase regexps
      (null t)
      (list (cl-loop for regexp in regexps
                     thereis (string-match regexp (buffer-file-name)))))))

(org-ql--defpred todo (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS (a list of strings)."
  (when-let ((state (org-get-todo-state)))
    (cl-typecase keywords
      (null (not (member state org-done-keywords)))
      (list (member state keywords))
      (symbol (member state (symbol-value keywords)))
      (otherwise (user-error "Invalid todo keywords: %s" keywords)))))

(org-ql--defpred done ()
  "Return non-nil if entry's TODO keyword is in `org-done-keywords'."
  ;; NOTE: This was a defsubst before being defined with the macro.  Might be good to make it a defsubst again.
  (or (apply #'org-ql--predicate-todo org-done-keywords)))

(org-ql--defpred tags (&rest tags)
  "Return non-nil if current heading has one or more of TAGS (a list of strings).
Tests both inherited and local tags."
  (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
    (-let* (((inherited local) (org-ql--tags-at (point))))
      (cl-typecase tags
        (null (or (tags-p inherited)
                  (tags-p local)))
        (otherwise (or (when (tags-p inherited)
                         (seq-intersection tags inherited))
                       (when (tags-p local)
                         (seq-intersection tags local))))))))

(org-ql--defpred tags-inherited (&rest tags)
  "Return non-nil if current heading's inherited tags include one or more of TAGS (a list of strings).
If TAGS is nil, return non-nil if heading has any inherited tags."
  (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
    (-let* (((inherited _) (org-ql--tags-at (point))))
      (cl-typecase tags
        (null (tags-p inherited))
        (otherwise (when (tags-p inherited)
                     (seq-intersection tags inherited)))))))

(org-ql--defpred tags-local (&rest tags)
  "Return non-nil if current heading's local tags include one or more of TAGS (a list of strings).
If TAGS is nil, return non-nil if heading has any local tags."
  (cl-macrolet ((tags-p (tags)
                        `(and ,tags
                              (not (eq 'org-ql-nil ,tags)))))
    (-let* (((_ local) (org-ql--tags-at (point))))
      (cl-typecase tags
        (null (tags-p local))
        (otherwise (when (tags-p local)
                     (seq-intersection tags local)))))))

(org-ql--defpred level (level-or-comparator &optional level)
  "Return non-nil if current heading's outline level matches arguments.
The following forms are accepted:

  (level NUMBER): Matches if heading level is NUMBER.
  (level NUMBER NUMBER): Matches if heading level is equal to or between NUMBERs.
  (level COMPARATOR NUMBER): Matches if heading level compares to NUMBER with COMPARATOR.

COMPARATOR may be `<', `<=', `>', or `>='."
  ;; NOTE: It might be necessary to take into account `org-odd-levels'; see docstring for
  ;; `org-outline-level'.
  (when-let ((outline-level (org-outline-level)))
    (pcase level-or-comparator
      ((pred numberp) (pcase level
                        ('nil ;; Equality
                         (= outline-level level-or-comparator))
                        ((pred numberp) ;; Between two levels
                         (>= level-or-comparator outline-level level))))
      ((pred symbolp) ;; Compare with function
       (funcall level-or-comparator outline-level level)))))

(org-ql--defpred priority (&optional comparator-or-priority priority)
  "Return non-nil if current heading has a certain priority.
COMPARATOR-OR-PRIORITY should be either a comparator function,
like `<=', or a priority string, like \"A\" (in which case (`='
will be the comparator).  If COMPARATOR-OR-PRIORITY is a
comparator, PRIORITY should be a priority string.  If both
arguments are nil, return non-nil if heading has any defined
priority."
  (let* (comparator)
    (cond ((null priority)
           ;; No comparator given: compare only given priority with =
           (setq priority comparator-or-priority
                 comparator '=))
          (t
           ;; Both comparator and priority given
           (setq comparator comparator-or-priority)))
    (setq comparator (cl-case comparator
                       ;; Invert comparator because higher priority means lower number
                       (< '>)
                       (> '<)
                       (<= '>=)
                       (>= '<=)
                       (= '=)
                       (otherwise (user-error "Invalid comparator: %s" comparator))))
    (setq priority (* 1000 (- org-lowest-priority (string-to-char priority))))
    (when-let ((item-priority (save-excursion
                                (save-match-data
                                  ;; TODO: Is the save-match-data above necessary?
                                  (when (and (looking-at org-heading-regexp)
                                             (save-match-data
                                               (string-match org-priority-regexp (match-string 0))))
                                    ;; TODO: Items with no priority
                                    ;; should not be the same as B
                                    ;; priority.  That's not very
                                    ;; useful IMO.  Better to do it
                                    ;; like in org-super-agenda.
                                    (org-get-priority (match-string 0)))))))
      (funcall comparator priority item-priority))))

(org-ql--defpred habit ()
  "Return non-nil if entry is a habit."
  (org-is-habit-p))

(org-ql--defpred regexp (&rest regexps)
  "Return non-nil if current entry matches all of REGEXPS (regexp strings)."
  (let ((end (or (save-excursion
                   (outline-next-heading))
                 (point-max))))
    (save-excursion
      (goto-char (line-beginning-position))
      (cl-loop for regexp in regexps
               always (save-excursion
                        (re-search-forward regexp end t))))))

(org-ql--defpred heading (&rest regexps)
  "Return non-nil if current entry's heading matches all REGEXPS (regexp strings)."
  ;; TODO: In Org 9.2+, `org-get-heading' takes 2 more arguments.
  (let ((heading (org-get-heading 'no-tags 'no-todo)))
    (--all? (string-match it heading) regexps)))

(org-ql--defpred property (property &optional value)
  "Return non-nil if current entry has PROPERTY (a string), and optionally VALUE (a string)."
  (pcase property
    ('nil (user-error "Property matcher requires a PROPERTY argument"))
    (_ (pcase value
         ('nil
          ;; Check that PROPERTY exists
          (org-entry-get (point) property))
         (_
          ;; Check that PROPERTY has VALUE
          (string-equal value (org-entry-get (point) property 'selective)))))))

;;;;;; Timestamps

;; TODO: Remove the _on vars from these arg lists.  I think they're not
;; necessary, or shouldn't be, since --pre-process-query should handle them.

;; NOTE: These docstrings apply to the functions defined by `org-ql--defpref',
;; not necessarily to the way users are expected to call them in queries.  The
;; queries are pre-processed by `org-ql--pre-process-query' to handle
;; arguments which are constant during a query's execution.

;; TODO: Update the macro to define a user-facing docstring so I don't
;; have to manually update the documentation.

(org-ql--defpred clocked (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable"
  ;; warnings, because we pre-process that argument in a macro before
  ;; this function is called.
  "Return non-nil if current entry was clocked in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."
  (org-ql--predicate-ts :from from :to to :regexp org-ql-clock-regexp :match-group 1))

(org-ql--defpred closed (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable"
  ;; warnings, because we pre-process that argument in a macro before
  ;; this function is called.
  "Return non-nil if current entry was closed in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."
  (org-ql--predicate-ts :from from :to to :regexp org-closed-time-regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql--defpred deadline (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable"
  ;; warnings, because we pre-process that argument in a macro before
  ;; this function is called.
  "Return non-nil if current entry has deadline in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."
  (org-ql--predicate-ts :from from :to to :regexp org-deadline-time-regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql--defpred deadline-warning (&key from to)
  "Internal selector used to handle `org-deadline-warning-days' and deadlines with warning periods.
Should be called on a planning line, because it does not search
past the end of the current line."
  (save-excursion
    (forward-line 1)
    (when (re-search-forward org-deadline-time-regexp (line-end-position) t)
      (-let* ((context (org-element-context))
              ;; Since we need to handle warning periods, we parse the Org timestamp
              ;; as an org-element rather than as a string.  Unfortunately, sometimes
              ;; `org-element-context' returns a timestamp nested inside a `planning'
              ;; element, other times just the timestamp, so we have to handle both.
              (deadline-ts-element (pcase context
                                     (`(planning ,tss) (plist-get tss :deadline))
                                     (`(timestamp . ,_) context)))
              ((_timestamp (&keys :warning-value :warning-unit)) deadline-ts-element)
              (ts (ts-parse-org-element deadline-ts-element))
              (ts (pcase warning-unit
                    ('nil ts)
                    ((and unit (or 'year 'month 'day))
                     (->> ts (ts-adjust unit (* -1 warning-value))))
                    ('week (->> ts (ts-adjust 'day (* -7 warning-value)))))))
        (cond ((and from to) (ts-in from to ts))
              (from (ts<= from ts))
              (to (ts<= ts to)))))))

(org-ql--defpred planning (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable"
  ;; warnings, because we pre-process that argument in a macro before
  ;; this function is called.
  "Return non-nil if current entry has planning timestamp in given period (i.e. its deadline, scheduled, or closed timestamp).
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."
  (org-ql--predicate-ts :from from :to to :regexp org-ql-planning-regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql--defpred scheduled (&key from to _on)
  ;; The underscore before `on' prevents "unused lexical variable"
  ;; warnings, because we pre-process that argument in a macro before
  ;; this function is called.
  "Return non-nil if current entry is scheduled in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value."
  (org-ql--predicate-ts :from from :to to :regexp org-scheduled-time-regexp :match-group 1
                        :limit (line-end-position 2)))

(org-ql--defpred ts (&key from to _on regexp (match-group 0) (limit (org-entry-end-position)))
  ;; The underscore before `on' prevents "unused lexical variable" warnings,
  ;; because we pre-process that argument in a macro before this function is
  ;; called.  The `regexp' argument is also provided by the macro and is not
  ;; to be given by the user, so it is omitted from the docstring.
  "Return non-nil if current entry has a timestamp in given period.
If no arguments are specified, return non-nil if entry has any
timestamp.

If FROM, return non-nil if entry has a timestamp on or after
FROM.

If TO, return non-nil if entry has a timestamp on or before TO.

If ON, return non-nil if entry has a timestamp on date ON.

FROM, TO, and ON should be either `ts' structs, or strings
parseable by `parse-time-string' which may omit the time value.

TYPE may be `active' to match active timestamps, `inactive' to
match inactive ones, or `both' / nil to match both types.

LIMIT bounds the search for the timestamp REGEXP.  It defaults to
the end of the entry, i.e. the position returned by
`org-entry-end-position', but for certain searches it should be
bound to a different positiion, e.g. for planning lines, the end
of the line after the heading."
  ;; TODO: DRY this with the clocked predicate.
  ;; NOTE: FROM and TO are actually expected to be `ts' structs.  The docstring is written
  ;; for end users, for which the arguments are pre-processed by `org-ql-select'.
  (cl-macrolet ((next-timestamp ()
                                `(when (re-search-forward regexp limit t)
                                   (ts-parse-org (match-string match-group))))
                (test-timestamps (pred-form)
                                 `(cl-loop for next-ts = (next-timestamp)
                                           while next-ts
                                           thereis ,pred-form)))
    (save-excursion
      (cond ((not (or from to)) (re-search-forward regexp limit t))
            ((and from to) (test-timestamps (ts-in from to next-ts)))
            (from (test-timestamps (ts<= from next-ts)))
            (to (test-timestamps (ts<= next-ts to)))))))

;;;;; Sorting

;; TODO: These appear to work properly, but it would be good to have tests for them.
;; MAYBE: Add timestamp sorter.  Could be slow in some cases, without clever caching of timestamps per-entry.

(defun org-ql--sort-by (items predicates)
  "Return ITEMS sorted by PREDICATES.
PREDICATES is a list of one or more sorting methods, including:
`deadline', `scheduled', and `priority'."
  ;; MAYBE: Use macrolet instead of flet.
  (cl-flet* ((sorter (symbol)
                     (pcase symbol
                       ((or 'deadline 'scheduled)
                        (apply-partially #'org-ql--date-type< (intern (concat ":" (symbol-name symbol)))))
                       ;; TODO: Rename `date' to `planning'.  `date' should be something else.
                       ('date #'org-ql--date<)
                       ('priority #'org-ql--priority<)
                       ('random (lambda (&rest _ignore)
                                  (= 0 (random 2))))
                       ;; NOTE: 'todo is handled below
                       ;; TODO: Add more.
                       (_ (user-error "Invalid sorting predicate: %s" symbol))))
             (sort-by-todo-keyword (items)
                                   (let* ((grouped-items (--group-by (when-let (keyword (org-element-property :todo-keyword it))
                                                                       (substring-no-properties keyword))
                                                                     items))
                                          (sorted-groups (cl-sort grouped-items #'<
                                                                  :key (lambda (keyword)
                                                                         (or (cl-position (car keyword) org-todo-keywords-1 :test #'string=)
                                                                             ;; Put at end of list if not found
                                                                             (1+ (length org-todo-keywords-1)))))))
                                     (-flatten-n 1 (-map #'cdr sorted-groups)))))
    (cl-loop for pred in (nreverse predicates)
             do (setq items (if (eq pred 'todo)
                                (sort-by-todo-keyword items)
                              (-sort (sorter pred) items)))
             finally return items)))

;; TODO: Rewrite date sorters using `ts'.

(defun org-ql--date-type< (type a b)
  "Return non-nil if A's date of TYPE is earlier than B's.
A and B are Org headline elements.  TYPE should be a symbol like
`:deadline' or `:scheduled'"
  (org-ql--org-timestamp-element< (org-element-property type a)
                                  (org-element-property type b)))

(defun org-ql--date< (a b)
  "Return non-nil if A's deadline or scheduled property is earlier than B's.
Deadline is considered before scheduled."
  (cl-macrolet ((ts (item)
                    `(or (org-element-property :deadline ,item)
                         (org-element-property :scheduled ,item))))
    (org-ql--org-timestamp-element< (ts a) (ts b))))

(defun org-ql--org-timestamp-element< (a b)
  "Return non-nil if A's date element is earlier than B's.
A and B are Org timestamp elements."
  (cl-macrolet ((ts (ts)
                    `(when ,ts
                       (org-timestamp-format ,ts "%s"))))
    (let* ((a-ts (ts a))
           (b-ts (ts b)))
      (cond ((and a-ts b-ts)
             (string< a-ts b-ts))
            (a-ts t)
            (b-ts nil)))))

(defun org-ql--priority< (a b)
  "Return non-nil if A's priority is higher than B's.
A and B are Org headline elements."
  (cl-macrolet ((priority (item)
                          `(org-element-property :priority ,item)))
    ;; NOTE: Priorities are numbers in Org elements.  This might differ from the priority selector logic.
    (let ((a-priority (priority a))
          (b-priority (priority b)))
      (cond ((and a-priority b-priority)
             (< a-priority b-priority))
            (a-priority t)
            (b-priority nil)))))

;;;; Footer

(provide 'org-ql)

;;; org-ql.el ends here
