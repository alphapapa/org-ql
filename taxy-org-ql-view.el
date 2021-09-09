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

(require 'map)
(require 'seq)

(require 'org-ql-view)

(require 'taxy)
(require 'taxy-magit-section)

;;;; Structs

(cl-defstruct (taxy-org-ql-view-section
               (:include taxy-magit-section
                         (make #'make-taxy-org-ql-view-section))))

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

(defcustom org-ql-view-taxy-level-indent 1
  "Indentation per level of depth."
  :type 'integer)

(defcustom org-ql-view-taxy-item-indent 1
  "Indentation of items relative to their level's indentation."
  :type 'integer)

;;;; Macros

;;;;; Columns

(defvar org-ql-view-column-format-fns nil
  "FIXME: Docstring.")

(defvar org-ql-view-columns
  '("Keyword" "Pri" "Heading" "Planning" "Tags")
  "FIXME: Docstring.")

(defmacro org-ql-view-define-column (name plist &rest body)
  "Define a column formatting function with NAME.
NAME should be a string.  BODY should return a string or nil.  In
the BODY, `element' is bound to the Org element, and `depth' is
bound to the buffer's depth in the group tree.

PLIST may be a plist setting the following options:

  `:face' is a face applied to the string.

  `:max-width' defines a customization option for the column's
  maximum width with the specified value as its default: an
  integer limits the width, while nil does not."
  (declare (indent defun))
  (cl-check-type name string)
  (pcase-let* ((fn-name (intern (concat "org-ql-view-column-format-" (downcase name))))
               ((map :face :max-width) plist)
               (max-width-variable (intern (concat "org-ql-view-column-" name "-max-width")))
               (max-width-docstring (format "Maximum width of the %s column." name)))
    `(progn
       ,(when (plist-member plist :max-width)
          `(defcustom ,max-width-variable
             ,max-width
             ,max-width-docstring
             :type '(choice (integer :tag "Maximum width")
                            (const :tag "Unlimited width" nil))))
       (defun ,fn-name (element depth)
         (if-let ((string (progn ,@body)))
             (progn
               ,(when max-width
                  `(when ,max-width-variable
                     (setf string (truncate-string-to-width string ,max-width-variable nil nil "…"))))
               ,(when face
                  ;; Faces are not defined until load time, while this checks type at expansion
                  ;; time, so we can only test that the argument is a symbol, not a face.
                  (cl-check-type face symbol ":face must be a face symbol")
                  `(setf string (propertize string 'face ',face)))
               string)
           ""))
       (setf (map-elt org-ql-view-column-format-fns ,name) #',fn-name))))

(org-ql-view-define-column "Keyword" (:max-width nil)
  (let ((indentation (make-string (+ (* depth org-ql-view-taxy-level-indent)
				     org-ql-view-taxy-item-indent)
				  ? ))
	(keyword (or (org-element-property :todo-keyword element) "")))
    (unless (string-empty-p keyword)
      ;; NOTE: We use `substring-no-properties' to avoid propagating
      ;; `wrap-prefix' and `line-prefix' properties that may be
      ;; present on the source buffer's keyword.
      (setf keyword (org-ql-view--add-todo-face (substring-no-properties keyword))))
    (concat indentation keyword)))

(org-ql-view-define-column "Heading" (:max-width 60)
  (ignore depth)
  (org-link-display-format
   (org-element-property
    :raw-value (org-ql-view--add-faces element))))

(org-ql-view-define-column "Planning" (:max-width nil)
  (ignore depth)
  (when-let ((planning-element (or (org-element-property :deadline element)
				   (org-element-property :scheduled element)
				   (org-element-property :closed element))))
    (org-ql-view--format-relative-date
     (floor (/ (ts-diff (ts-now) (ts-parse-org-element planning-element))
	       86400)))))

(org-ql-view-define-column "Pri" (:max-width nil)
  (ignore depth)
  (or (-some->> (org-element-property :priority element)
        (char-to-string)
        (format "[#%s]")
        (org-ql-view--add-priority-face))
      ""))

(org-ql-view-define-column "Tags" (:max-width nil)
  (ignore depth)
  ;; Copied from `org-ql-view--format-element'.
  (when-let ((tags (if org-use-tag-inheritance
		       ;; MAYBE: Use our own variable instead of `org-use-tag-inheritance'.
		       (if-let ((marker (or (org-element-property :org-hd-marker element)
					    (org-element-property :org-marker element))))
			   (with-current-buffer (marker-buffer marker)
			     (org-with-wide-buffer
			      (goto-char marker)
			      (cl-loop for type in (org-ql--tags-at marker)
				       unless (or (eq 'org-ql-nil type)
                                                  (not type))
				       append type)))
                         ;; No marker found
                         ;; TODO: Use `display-warning' with `org-ql' as the type.
                         (warn "No marker found for item: %s" element)
                         (org-element-property :tags element))
		     (org-element-property :tags element))))
    (org-add-props (concat ":" (string-join tags ":") ":")
	nil 'face 'org-tag)))

;;;; Defining taxy keys with macro

(defvar taxy-org-ql-view-keys nil)

(defmacro taxy-org-ql-view-define-key (name args &rest body)
  "Define a `taxy-org-ql-view' key function by NAME having BODY taking ARGS.
Within BODY, `element' is bound to the `org-element' element
being tested.

Defines a function named `taxy-org-ql--predicate-NAME', and adds
an entry to `taxy-org-ql-view-keys' mapping NAME to the new
function symbol."
  (declare (indent defun)
	   (debug (&define symbolp listp &rest def-form)))
  (let* ((fn-symbol (intern (format "taxy-org-ql--predicate-%s" name)))
	 (fn `(lambda (element ,@args)
		,@body)))
    `(progn
       (fset ',fn-symbol ,fn)
       (setf (map-elt taxy-org-ql-view-keys ',name) ',fn-symbol))))

(taxy-org-ql-view-define-key heading (&rest strings)
  "Return STRINGS that ELEMENT's heading matches."
  (when-let ((matches (cl-loop with heading = (org-element-property :raw-value element)
			       for string in strings
			       when (string-match (regexp-quote string) heading)
			       collect string)))
    (format "Heading: %s" (string-join matches ", "))))

(taxy-org-ql-view-define-key todo (&optional keyword)
  "Return the to-do keyword for ELEMENT.
If KEYWORD, return whether it matches that."
  (when-let ((element-keyword (org-element-property :todo-keyword element)))
    (cl-flet ((format-keyword
	       (keyword) (format "To-do: %s" keyword)))
      (pcase keyword
	('nil (format-keyword element-keyword))
	(_ (pcase element-keyword
	     ((pred (equal keyword))
	      (format-keyword element-keyword))))))))

(taxy-org-ql-view-define-key tags (&rest tags)
  "Return the tags for ELEMENT.
If TAGS, return whether it matches them."
  (cl-flet ((tags-at
	     (pos) (apply #'append (delq 'org-ql-nil (org-ql--tags-at pos)))))
    (org-with-point-at (org-element-property :org-hd-marker element)
      (pcase tags
	('nil (tags-at (point)))
	(_ (when-let (common-tags (seq-intersection tags (tags-at (point))
						    #'cl-equalp))
	     (format "Tags: %s" (string-join common-tags ", "))))))))

(taxy-org-ql-view-define-key priority (&optional priority)
  "Return ELEMENT's priority as a string.
If PRIORITY, return it if it matches ELEMENT's priority."
  (when-let ((priority-number (org-element-property :priority element)))
    (cl-flet ((format-priority
	       (num) (format "Priority: %s" num)))
      ;; FIXME: Priority numbers may be wildly larger, right?
      (pcase priority
	('nil (format-priority (char-to-string priority-number)))
	(_ (pcase (char-to-string priority-number)
	     ((and (pred (equal priority)) string)
	      (format-priority string))))))))

(taxy-org-ql-view-define-key planning-month ()
  "Return ELEMENT's planning-date month, or nil.
Returns in format \"%Y-%m (%B)\"."
  (when-let ((planning-element (or (org-element-property :deadline element)
				   (org-element-property :scheduled element)
				   (org-element-property :closed element))))
    (ts-format "Planning: %Y-%m (%B)" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning-year ()
  "Return ELEMENT's planning-date year, or nil.
Returns in format \"%Y\"."
  (when-let ((planning-element (or (org-element-property :deadline element)
				   (org-element-property :scheduled element)
				   (org-element-property :closed element))))
    (ts-format "Planning: %Y" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning-date ()
  "Return ELEMENT's planning date, or nil.
Returns in format \"%Y-%m-%d\"."
  (when-let ((planning-element (or (org-element-property :deadline element)
				   (org-element-property :scheduled element)
				   (org-element-property :closed element))))
    (ts-format "Planning: %Y-%m-%d" (ts-parse-org-element planning-element))))

(taxy-org-ql-view-define-key planning ()
  "Return \"Planned\" if ELEMENT has a planning date."
  (when (or (org-element-property :deadline element)
	    (org-element-property :scheduled element)
	    (org-element-property :closed element))
    "Planned"))

(taxy-org-ql-view-define-key category ()
  "Return ELEMENT's category."
  (org-with-point-at (org-element-property :org-hd-marker element)
    (org-get-category)))

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
  "Return the year of ELEMENT's latest timestamp."
  (when-let ((latest-ts (taxy-org-ql--latest-timestamp-in org-element--timestamp-regexp element)))
    (ts-format "%Y" latest-ts)))

(taxy-org-ql-view-define-key ts-month ()
  "Return the month of ELEMENT's latest timestamp."
  (when-let ((latest-ts (taxy-org-ql--latest-timestamp-in org-element--timestamp-regexp element)))
    (ts-format "%Y-%m (%B)" latest-ts)))

(taxy-org-ql-view-define-key deadline (&rest args)
  "Return whether ELEMENT has a deadline according to ARGS."
  (when-let ((deadline-element (org-element-property :deadline element)))
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

(defun taxy-org-ql-view-take-fn (keys)
  "Return a `taxy' \"take\" function for KEYS.
Each of KEYS should be a function alias defined in
`taxy-org-ql-view-keys', or a list of such KEY-FNS (recursively,
ad infinitum, approximately)."
  (let ((macrolets (cl-loop for (name . fn) in taxy-org-ql-view-keys
			    collect `(,name ',fn))))
    (cl-labels ((expand-form
		 ;; Is using (cadr (macroexpand-all ...)) really better than `eval'?
		 (form) (cadr
			 (macroexpand-all
			  `(cl-symbol-macrolet (,@macrolets)
			     ,form))))
		(quote-fn
		 (fn) (pcase fn
			((pred symbolp) fn)
			(`(,(and (pred symbolp) fn)
			   . ,(and args (guard (cl-typecase (car args)
						 ((or keyword (and atom (not symbol)))
						  t)))))
			 ;; Key with args: replace with a lambda that
			 ;; calls that key's function with given args.
			 `(lambda (element)
			    (,(expand-form fn) element ,@args)))
			((pred listp) (cons 'list (mapcar #'quote-fn fn))))))
      (setf keys (mapcar #'quote-fn keys))
      (expand-form
       `(lambda (item taxy)
	  (taxy-take-keyed (list ,@keys) item taxy))))))

(defun taxy-org-ql-view-make-taxy (name keys &rest args)
  "Return a dynamic `taxy-org-ql-view-section' taxy named NAME having KEYS.
KEYS is passed to `taxy-org-ql-view-take-fn', which see."
  (declare (indent defun))
  (apply #'make-taxy-org-ql-view-section
	 :name name
	 :take (taxy-org-ql-view-take-fn keys)
	 args))

;;;; Variables

;;;; Customization

;;;; Commands

;;;; Functions

(cl-defun taxy-org-ql-search
    (buffers-or-files query &key taxy-keys sort)
  "Show Org QL QUERY on BUFFERS-OR-FILES with `taxy-org-ql-view'."
  (declare (indent 1))
  (let* ((title (format "Query:%S  In:%S" query buffers-or-files))
	 (buffer-name (format "*Taxy Org QL View: %s*" title)))
    (when (get-buffer buffer-name)
      ;; Reusing an existing magit-section buffer seems to cause a lot
      ;; of GC, so just kill it if it already exists.
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (magit-section-mode)
      (use-local-map (make-composed-keymap (list magit-section-mode-map org-ql-view-map)))
      (taxy-org-ql-view--add-search buffers-or-files
	query :sort sort :taxy-keys taxy-keys)
      (pop-to-buffer (current-buffer)))))

(cl-defun taxy-org-ql-view--add-search
    (buffers-or-files query &key taxy-keys sort)
  "Show Org QL QUERY on BUFFERS-OR-FILES with `taxy-org-ql-view'."
  (declare (indent 1))
  (let (format-table column-sizes)
    (cl-labels ((format-item (item) (gethash item format-table))
		(make-fn (&rest args)
			 (apply #'make-taxy-org-ql-view-section
                                :make #'make-fn
                                :format-fn #'format-item
                                ;; :heading-face-fn #'heading-face
                                :heading-indent org-ql-view-taxy-level-indent
                                :item-indent 0
                                args)))
      (let* ((title (format "Query:%S  In:%S"
			    (org-ql--query-sexp-to-string query) buffers-or-files))
	     (items (org-ql-select buffers-or-files query
		      :action 'element-with-markers
		      :sort sort))
	     (taxy (thread-last (make-fn
				 :name title
				 :take (taxy-org-ql-view-take-fn taxy-keys))
		     (taxy-fill items)))
	     (taxy-magit-section-insert-indent-items nil)
	     format-cons header)
	;; FIXME: Adding a search overwrites the `header-line-format'.
	(setf format-cons (taxy-magit-section-format-items
			   org-ql-view-columns org-ql-view-column-format-fns taxy)
	      format-table (car format-cons)
	      column-sizes (cdr format-cons)
	      ;; NOTE: The first column is handled differently.
	      header (concat (format (format " %%-%ss" (cdar column-sizes)) (caar column-sizes))
			     (cl-loop for (name . size) in (cdr column-sizes)
				      for spec = (format " %%-%ss" size)
				      concat (format spec name)))
	      header-line-format header)
	(let ((inhibit-read-only t))
	  (save-excursion
	    (goto-char (point-max))
	    (taxy-magit-section-insert taxy :items 'first)))))))

;;;; Footer

(provide 'taxy-org-ql-view)

;;; taxy-org-ql-view.el ends here
