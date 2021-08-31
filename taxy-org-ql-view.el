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
                         (format-fn #'org-ql-view--format-element)
                         (indent 2)
                         (make #'make-taxy-org-ql-view-section))))

;;;; Macros

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

(defun taxy-org-ql-view-make-taxy (name keys)
  "Return a dynamic `taxy-org-ql-view-section' taxy named NAME having KEYS.
KEYS is passed to `taxy-org-ql-view-take-fn', which see."
  (declare (indent defun))
  (make-taxy-org-ql-view-section
   :name name
   :take (taxy-org-ql-view-take-fn keys)))

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
  (let* ((title (format "Query:%S  In:%S" query buffers-or-files))
	 (taxy (taxy-org-ql-view-make-taxy title
		 taxy-keys))
	 (items (org-ql-select buffers-or-files query
		  :action 'element-with-markers
		  :sort sort))	 )
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-max))
	(taxy-magit-section-insert
	 (thread-last taxy
	   (taxy-fill items)
	   (taxy-mapc* (lambda (taxy)
			 (setf (taxy-taxys taxy)
			       (cl-sort (taxy-taxys taxy) #'string<
					:key #'taxy-name)))))
	 :items 'last)))))

;;;; Footer

(provide 'taxy-org-ql-view)

;;; taxy-org-ql-view.el ends here
