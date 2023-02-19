;;; taxy-org-ql-view.el ---                          -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'org-ql-view)

(require 'taxy)
(require 'taxy-magit-section)

;;;; Structs

;;;; Variables

(defvar-local taxy-org-ql-view-args nil
  "Arguments passed to `taxy-org-ql-search'.
Used when updating the view.")

(defvar-local taxy-org-ql-view-queries nil
  "Queries shown in the current buffer.
Used when updating the view.")

;;;; Customization

(defgroup org-ql-view-taxy nil
  "Options for `org-ql-view-taxy'."
  :group 'org-ql-view)

(defcustom org-ql-view-taxy-blank-between-depth 1
  "Insert blank lines between groups up to this depth."
  :type 'integer)

(defcustom org-ql-view-taxy-initial-depth 0
  "Effective initial depth of first-level groups.
Sets at which depth groups and items begin to be indented.  For
example, setting to -1 prevents indentation of the first and
second levels."
  :type 'integer)

;;;;; Faces

(defgroup org-ql-view-faces nil
  "Faces for Org QL View buffers."
  :group 'org-ql-view-taxy)

(defface org-ql-view-header-line
  '((t (:inherit header-line :weight bold)))
  "Header line.")

(defface org-ql-view-query-heading
  '((t (:inherit header-line :height 1.3)))
  "Query headings.")

(defface taxy-org-ql-view-header
  '((t (:inherit header-line :height 1.5 :weight bold :overline t :extend t)))
  "View top-level section names.")

(defface org-ql-view-heading
  `((t (:inherit magit-section-heading :weight bold)))
  "Group headings.
Inherited by level-specific faces.")

(defface org-ql-view-heading-1
  `((t (:inherit org-ql-view-heading
		 :height 1.1 :overline t
		 :background ,(face-background 'header-line))))
  "Level-1 group headings.")

(defface org-ql-view-heading-2
  `((t (:inherit org-ql-view-heading
		 :height 1.0 :overline nil
		 :background ,(face-background 'header-line))))
  "Level-2 group headings.")

;;;;; Columns

(eval-and-compile
  (taxy-magit-section-define-column-definer "org-ql-view"))

(org-ql-view-define-column "Category" (:max-width nil :align 'right)
  (or (org-with-point-at (org-element-property :org-hd-marker item)
	(org-get-category (point)))
      ""))

(org-ql-view-define-column "Keyword" (:max-width nil :align 'right)
  (let ((keyword (or (org-element-property :todo-keyword item) "")))
    (unless (string-empty-p keyword)
      ;; NOTE: We use `substring-no-properties' to avoid propagating
      ;; `wrap-prefix' and `line-prefix' properties that may be
      ;; present on the source buffer's keyword.
      (setf keyword (org-ql-view--add-todo-face (substring-no-properties keyword))))
    keyword))

(org-ql-view-define-column "Heading" (:max-width 60)
  (propertize (org-link-display-format
	       (org-element-property
		:raw-value (org-ql-view--add-faces item)))
	      :org-hd-marker (org-element-property :org-hd-marker item)))

(org-ql-view-define-column "Pri" (:max-width nil)
  (or (-some->> (org-element-property :priority item)
        (char-to-string)
        (format "[#%s]")
        (org-ql-view--add-priority-face))
      ""))

(org-ql-view-define-column "Planning" (:max-width nil)
  (when-let ((planning-element (or (org-element-property :deadline item)
                                   (org-element-property :scheduled item)
                                   (org-element-property :closed item))))
    (org-ql-view--format-relative-date
     (floor (/ (ts-diff (ts-now) (ts-parse-org-element planning-element))
               86400)))))

(org-ql-view-define-column "Tags" (:max-width nil)
  ;; Copied from `org-ql-view--format-element'.
  (when-let ((tags (if org-use-tag-inheritance
                       ;; MAYBE: Use our own variable instead of `org-use-tag-inheritance'.
                       (if-let ((marker (or (org-element-property :org-hd-marker item)
                                            (org-element-property :org-marker item))))
                           (with-current-buffer (marker-buffer marker)
                             (org-with-wide-buffer
                              (goto-char marker)
                              (cl-loop for type in (org-ql--tags-at marker)
                                       unless (or (eq 'org-ql-nil type)
                                                  (not type))
                                       append type)))
                         ;; No marker found
                         ;; TODO: Use `display-warning' with `org-ql' as the type.
                         (warn "No marker found for item: %s" item)
                         (org-element-property :tags item))
                     (org-element-property :tags item))))
    (org-add-props (concat ":" (string-join tags ":") ":")
        nil 'face 'org-tag)))

(unless org-ql-view-columns
  (setq-default org-ql-view-columns
                (get 'org-ql-view-columns 'standard-value)))

;;;; Taxy keys

(eval-and-compile
  (taxy-define-key-definer taxy-org-ql-view-define-key
    taxy-org-ql-view-keys "taxy-org-ql--key"
    "Define a `taxy-org-ql-view' key function by NAME having BODY taking ARGS.
Within BODY, `item' is bound to the `org-element' element being
tested.

Defines a function named `taxy-org-ql--predicate-NAME', and adds
an entry to `taxy-org-ql-view-keys' mapping NAME to the new
function symbol."))

(taxy-org-ql-view-define-key heading (&rest strings)
  "Return STRINGS that ITEM's heading matches."
  (when-let ((matches (cl-loop with heading = (org-element-property :raw-value item)
                               for string in strings
                               when (string-match (regexp-quote string) heading)
                               collect string)))
    (format "Heading: %s" (string-join matches ", "))))

(taxy-org-ql-view-define-key todo (&optional keyword)
  "Return the to-do keyword for ITEM.
If KEYWORD, return whether it matches that."
  (when-let ((element-keyword (org-element-property :todo-keyword item)))
    (cl-flet ((format-keyword
               (keyword) (format "To-do: %s" keyword)))
      (pcase keyword
        ('nil (format-keyword element-keyword))
        (_ (pcase element-keyword
             ((pred (equal keyword))
              (format-keyword element-keyword))))))))

(taxy-org-ql-view-define-key tags (&rest tags)
  "Return the tags for ITEM.
If TAGS, return whether it matches them."
  (cl-flet ((tags-at
             (pos) (apply #'append (delq 'org-ql-nil (org-ql--tags-at pos)))))
    (org-with-point-at (org-element-property :org-hd-marker item)
      (pcase tags
        ('nil (tags-at (point)))
        (_ (when-let (common-tags (seq-intersection tags (tags-at (point))
                                                    #'cl-equalp))
             (format "Tags: %s" (string-join common-tags ", "))))))))

(taxy-org-ql-view-define-key priority (&optional priority)
  "Return ITEM's priority as a string.
If PRIORITY, return it if it matches ITEM's priority."
  (when-let ((priority-number (org-element-property :priority item)))
    (cl-flet ((format-priority
               (num) (format "Priority: %s" num)))
      ;; FIXME: Priority numbers may be wildly larger, right?
      (pcase priority
        ('nil (format-priority (char-to-string priority-number)))
        (_ (pcase (char-to-string priority-number)
             ((and (pred (equal priority)) string)
              (format-priority string))))))))

(taxy-org-ql-view-define-key planning-month ()
  "Return ITEM's planning-date month, or nil.
Returns in format \"%Y-%m (%B)\"."
  (when-let ((planning-element (or (org-element-property :deadline item)
                                   (org-element-property :scheduled item)
                                   (org-element-property :closed item))))
    (ts-format "Planning: %Y-%m (%B)" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning-year ()
  "Return ITEM's planning-date year, or nil.
Returns in format \"%Y\"."
  (when-let ((planning-element (or (org-element-property :deadline item)
                                   (org-element-property :scheduled item)
                                   (org-element-property :closed item))))
    (ts-format "Planning: %Y" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning-date ()
  "Return ITEM's planning date, or nil.
Returns in format \"%Y-%m-%d\"."
  (when-let ((planning-element (or (org-element-property :deadline item)
                                   (org-element-property :scheduled item)
                                   (org-element-property :closed item))))
    (ts-format "Planning: %Y-%m-%d" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning ()
  "Return \"Planned\" if ITEM has a planning date."
  (when (or (org-element-property :deadline item)
            (org-element-property :scheduled item)
            (org-element-property :closed item))
    "Planned"))

(taxy-org-ql-view-define-key agenda
  (&optional (days (pcase org-agenda-span
		     ('week 7)
		     ('day 1)
		     ('month (date-days-in-month (ts-year (ts-now)) (ts-month (ts-now))))
		     ('year 365)
		     ((pred numberp) org-agenda-span))))
  ;; FIXME: This isn't quite how Org Agenda works.
  "Return ITEM's planning date if it's within DAYS of the current date."
  (when-let ((planning-element (or (org-element-property :deadline item)
				   (org-element-property :scheduled item)
				   (org-element-property :closed item))))
    (let ((parsed-ts (ts-parse-org-element planning-element)))
      (when (<= (ts-diff (ts-apply 'hour 0 'minute 0 'second 0 (ts-now)) parsed-ts)
		(* days 86400))
	(ts-format "Agenda: %Y-%m-%d" parsed-ts)))))

(taxy-org-ql-view-define-key category ()
  "Return ITEM's category."
  (org-with-point-at (org-element-property :org-hd-marker item)
    (concat "Category: " (org-get-category))))

(defun taxy-org-ql--latest-timestamp-in (regexp element)
  "Return the latest timestamp matching REGEXP in ELEMENT.
Searches in ELEMENT's buffer."
  (org-with-point-at (org-element-property :org-hd-marker element)
    (let* ((limit (org-entry-end-position))
           (tss (cl-loop for next-ts =
                         (when (re-search-forward regexp limit t)
                           (ts-parse-org (match-string 1)))
                         while next-ts
                         collect next-ts)))
      (when tss
        (car (sort tss #'ts>))))))

(taxy-org-ql-view-define-key ts-year ()
  "Return the year of ITEM's latest timestamp."
  (when-let ((latest-ts (taxy-org-ql--latest-timestamp-in org-element--timestamp-regexp item)))
    (ts-format "%Y" latest-ts)))

(taxy-org-ql-view-define-key ts-month ()
  "Return the month of ITEM's latest timestamp."
  (when-let ((latest-ts (taxy-org-ql--latest-timestamp-in org-element--timestamp-regexp item)))
    (ts-format "%Y-%m (%B)" latest-ts)))

(taxy-org-ql-view-define-key deadline (&rest args)
  "Return whether ITEM has a deadline according to ARGS."
  (when-let ((deadline-element (org-element-property :deadline item)))
    (pcase args
      (`(,(or 'nil 't)) "Deadlined")
      (_ (let ((element-ts (ts-parse-org-element deadline-element)))
           (pcase args
             ((and `(:past)
                   (guard (ts> (ts-now) element-ts)))
              "Overdue")
             ((and `(:today)
                   (guard (equal (ts-day (ts-now)) (ts-day element-ts))))
              "Due today")
             ((and `(:future)
                   (guard (ts< (ts-now) element-ts)))
              ;; FIXME: Not necessarily soon.
              "Due soon")
             ((and `(:before ,target-date)
                   (guard (ts< element-ts (ts-parse target-date))))
              (concat "Due before: " target-date))
             ((and `(:after ,target-date)
                   (guard (ts> element-ts (ts-parse target-date))))
              (concat "Due after: " target-date))
             ((and `(:on ,target-date)
                   (guard (let ((now (ts-now)))
                            (and (equal (ts-doy element-ts)
                                        (ts-doy now))
                                 (equal (ts-year element-ts)
                                        (ts-year now))))))
              (concat "Due on: " target-date))
             ((and `(:from ,target-ts)
                   (guard (ts<= (ts-parse target-ts) element-ts)))
              (concat "Due from: " target-ts))
             ((and `(:to ,target-ts)
                   (guard (ts>= (ts-parse target-ts) element-ts)))
              (concat "Due to: " target-ts))
             ((and `(:from ,from-ts :to ,to-ts)
                   (guard (and (ts<= (ts-parse from-ts) element-ts)
                               (ts>= (ts-parse to-ts) element-ts))))
              (format "Due from: %s to %s" from-ts to-ts))))))))

(taxy-org-ql-view-define-key planned (&rest args)
  "Return whether ITEM is planned according to ARGS.
DEADLINE, SCHEDULED, and CLOSED timestamps are considered, in
that order."
  (when-let ((planned-element (or (org-element-property :deadline item)
                                  (org-element-property :scheduled item)
                                  (org-element-property :closed item))))
    ;; TODO: Every key should support a :name like this.
    (let ((name (cadr (member :name args))))
      (when name
        (let ((pos (cl-position :name args)))
          (setf args (append (cl-subseq args 0 pos)
                             (cl-subseq args (+ 2 pos))))))
      (pcase args
        (`(,(or 'nil 't)) (or name "Planned"))
        (_ (let ((element-ts (ts-parse-org-element planned-element)))
             (pcase args
               ((and `(:past)
                     (guard (ts> (ts-now) element-ts)))
                (or name "Planned: past"))
               ((and `(:today)
                     (guard (equal (ts-day (ts-now)) (ts-day element-ts))))
                (or name "Planned: today"))
               ((and `(:future)
                     (guard (ts< (ts-now) element-ts)))
                ;; FIXME: Not necessarily soon.
                (or name "Planned: future"))
               ((and `(:before ,target-date)
                     (guard (ts< element-ts (ts-parse target-date))))
                (or name (concat "Planned before: " target-date)))
               ((and `(:after ,target-date)
                     (guard (ts> element-ts (ts-parse target-date))))
                (or name (concat "Planned after: " target-date)))
               ((and `(:on ,target-date)
                     (guard (let ((now (ts-now)))
                              (and (equal (ts-doy element-ts)
                                          (ts-doy now))
                                   (equal (ts-year element-ts)
                                          (ts-year now))))))
                (or name (concat "Planned on: " target-date)))
               ((and `(:from ,target-ts)
                     (guard (ts<= (ts-parse target-ts) element-ts)))
                (or name (concat "Planned from: " target-ts)))
               ((and `(:to ,target-ts)
                     (guard (ts>= (ts-parse target-ts) element-ts)))
                (or name (concat "Planned to: " target-ts)))
               ((and `(:from ,from-ts :to ,to-ts)
                     (guard (and (ts<= (ts-parse from-ts) element-ts)
                                 (ts>= (ts-parse to-ts) element-ts))))
                (or name (format "Planned from: %s to %s" from-ts to-ts))))))))))

(taxy-org-ql-view-define-key file (&key full-path)
  "Return the name of ITEM's containing file."
  (let ((filename (org-with-point-at (org-element-property :org-hd-marker item)
                    (if full-path
                        (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))))))
    (concat "File: " filename)))

;;;; Mode

(defvar taxy-org-ql-view-mode-map
  (let* ((org-agenda-mode-map-copy (copy-keymap org-agenda-mode-map))
	 map)
    (cl-loop for key in (where-is-internal #'org-agenda-goto org-agenda-mode-map-copy)
	     do (define-key org-agenda-mode-map-copy key nil))
    (setf map (make-composed-keymap magit-section-mode-map org-agenda-mode-map-copy))
    (define-key map "g" #'taxy-org-ql-view-refresh)
    (define-key map "r" #'taxy-org-ql-view-refresh)
    (define-key map "q" #'bury-buffer)
    (define-key map "v" #'org-ql-view-dispatch)
    (define-key map (kbd "C-x C-s") #'org-ql-view-save)
    ;; HACK: Undefine Org's extra "<tab>" binding from
    ;; org-agenda-mode-map, which interferes with the "TAB" binding
    ;; from magit-section-mode-map.  (This shouldn't be necessary
    ;; since we're already looping through the bindings earlier, but
    ;; for some reason, it is.)
    (define-key map (kbd "<tab>") nil)
    map))

(define-derived-mode taxy-org-ql-view-mode magit-section-mode "Org QL View"
  "TODO: Docstring."
  ;; For compatibility with Org Agenda commands.
  (setq-local org-agenda-type 'search))

;;;; Functions

;; FIXME: Each taxy's items are formatted relative to its own items,
;; so column widths don't account for the width of items in other
;; taxys.  This should be fixable, but it will require some thoughtful
;; refactoring, which will probably require a new version of
;; taxy-magit-section.

(cl-defun taxy-org-ql-view
    (&rest rest &key name buffer queries from group sort append)
  "Show Org QL QUERIES in BUFFER with `taxy-org-ql-view'.
BUFFER may be a buffer, a name of a buffer, or a name of a buffer
to make.

QUERIES is a list of plists with the following keys:

  :name  An optional name for the query.
  :from   One or a list of buffers/files to search.
  :query  The `org-ql' query expression.
  :sort   One or a list of sorting predicates.
  :group  A group definition.

GROUP and SORT, if specified, apply to all QUERIES unless a
query specifies its own.

If APPEND, add QUERIES to BUFFER; otherwise, replace BUFFER's
contents."
  (declare (indent defun))
  ;; Silence byte-compiler since we use `symbol-value' for these.
  (ignore from group sort append)
  (cl-labels ((add-props
               ;; NOTE: This mutates.  Maybe good, maybe not.
               (plist) (dolist (prop '(:name :from :sort :group) plist)
                         (unless (plist-member plist prop)
                           (setf plist (plist-put plist prop (plist-get rest prop)))))))
    (let* ((buffer
            (cl-typecase buffer
              (buffer buffer)
              (string (or (get-buffer buffer)
                          (get-buffer-create (format "*Taxy Org QL View: %s*" buffer)))))))
      (with-current-buffer buffer
        (unless append
          (kill-all-local-variables)
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (unless (eq 'taxy-org-ql-view-mode major-mode)
          (taxy-org-ql-view-mode))
        (cl-pushnew rest taxy-org-ql-view-args :test #'equal)
        (let ((inhibit-read-only t))
          (when name
            (save-excursion
              (goto-char (point-max))
              (insert (propertize (concat name "\n") 'face 'taxy-org-ql-view-header) "\n")))
          (dolist (query queries)
            (setf query (add-props query))
            (apply #'taxy-org-ql-view--add-search query)))
        (pop-to-buffer (current-buffer))))))

(defun taxy-org-ql-view-refresh ()
  "Refresh buffer."
  (interactive)
  (cl-assert (eq 'taxy-org-ql-view-mode major-mode))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dolist (args (reverse taxy-org-ql-view-args))
    (apply #'taxy-org-ql-view :buffer (current-buffer) :append t
           args)))

(cl-defun taxy-org-ql-view--add-search
    (&key name from query group sort)
  "Show Org QL QUERY on BUFFERS-OR-FILES with `taxy-org-ql-view'."
  (declare (indent 1))
  (let (format-table column-sizes)
    (cl-labels ((format-item (item)
			     ;; For compatibility with Org Agenda, we
			     ;; add the marker property to the whole
			     ;; string (though it only seems to check
			     ;; at BOL).
			     (let* ((string (gethash item format-table))
				    (marker (or (get-text-property 0 :org-hd-marker string)
						(when-let ((pos (next-single-property-change 0 :org-hd-marker string)))
						  (get-text-property pos :org-hd-marker string)))))
			       ;; I don't understand why Org sometimes
			       ;; uses one property and sometimes the
			       ;; other.
			       (propertize string
					   'org-hd-marker marker
					   'org-marker marker)))
		(heading-face
		 (depth) (pcase depth
			   (-1 'org-ql-view-query-heading)
			   ;; NOTE: Faces count from 1 (like
			   ;; `outline-` faces), but depth from 0 (or
			   ;; -1 for query headings).
			   (0 'org-ql-view-heading-1)
			   (1 'org-ql-view-heading-2)
			   (_ 'org-ql-view-heading)))
                (make-fn (&rest args)
                         (apply #'make-taxy-magit-section
                                :make #'make-fn
                                :take (taxy-make-take-function group taxy-org-ql-view-keys)
                                :format-fn #'format-item
                                :heading-face-fn #'heading-face
                                :level-indent org-ql-view-level-indent
                                :item-indent org-ql-view-item-indent
                                args)))
      (let* ((title (or name
                        (org-ql-view--header-line-format
		         :buffers-files from
		         :query query
		         ;; FIXME: View titles.
		         )))
             (items (org-ql-select from query
                      :action 'element-with-markers
                      :sort sort))
             (taxy (thread-last (make-fn
                                 :name title)
                                (taxy-fill items)))
             (taxy-magit-section-insert-indent-items nil)
             format-cons)
        ;; FIXME: Adding a search overwrites the `header-line-format'.
        (setf format-cons (taxy-magit-section-format-items
                           org-ql-view-columns org-ql-view-column-formatters taxy)
              format-table (car format-cons)
              column-sizes (cdr format-cons)
              header-line-format (taxy-magit-section-format-header
				  column-sizes org-ql-view-column-formatters))
	(add-face-text-property 0 (length header-line-format) 'org-ql-view-header-line
				nil header-line-format)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (taxy-magit-section-insert taxy :items 'first
              :initial-depth -1)))))))

;;;; Footer

(provide 'taxy-org-ql-view)

;;; taxy-org-ql-view.el ends here
