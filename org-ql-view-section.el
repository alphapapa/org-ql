;;; org-ql-view-section.el --- Magit-section-like views for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

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

;; Inspired by `magit-section'.

;;; Code:

;;;; Requirements

(require 'eieio)

(require 's)

;;;; Macros

(defmacro org-ql-defkeymap (name copy docstring &rest maps)
  "Define a new keymap variable (using `defvar').

NAME is a symbol, which will be the new variable's symbol.  COPY
may be a keymap which will be copied, or nil, in which case the
new keymap will be sparse.  DOCSTRING is the docstring for
`defvar'.

MAPS is a sequence of alternating key-value pairs.  The keys may
be a string, in which case they will be passed as arguments to
`kbd', or a raw key sequence vector.  The values may be lambdas
or function symbols, as would be normally passed to
`define-key'."
  (declare (indent defun))
  (let* ((map (if copy
                  (copy-keymap copy)
                (make-sparse-keymap (prin1-to-string name)))))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    `(defvar ,name ',map ,docstring)))

(defmacro org-ql-defclass (name superclasses slots &rest options-and-doc)
  ;; Copied from `defclass*' in elexandria.el.
  ;; MAYBE: Change instance-initform to instance-init.  This would no longer set
  ;; the slot to the value of the expression, but would just evaluate it.  The
  ;; expression could set the slot value with `setq' if necessary.
  "Like `defclass', but supports instance initforms.
Each slot may have an `:instance-initform', which is evaluated in
the context of the object's slots when each instance is
initialized, similar to Python's __init__ method."
  ;; TODO: Add option to set all slots' initforms (e.g. to set them all to nil).
  (declare (indent defun))
  (let* ((slot-inits (-non-nil (--map (let ((name (car it))
                                            (initer (plist-get (cdr it) :instance-initform)))
                                        (when initer
                                          (list 'setq name initer)))
                                      slots)))
         (slot-names (mapcar #'car slots))
         ;; FIXME: `around-fn-name' is unused.
         ;; (around-fn-name (intern (concat (symbol-name name) "-initialize")))
         (docstring (format "Inititalize instance of %s." name)))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options-and-doc)
       (when (> (length ',slot-inits) 0)
         (cl-defmethod initialize-instance :after ((this ,name) &rest _)
           ,docstring
           (with-slots ,slot-names this
             ,@slot-inits))))))

(cl-defmacro org-ql-defstruct (&rest args)
  ;; Copied from `ts-defstruct'.
  "Like `cl-defstruct', but with additional slot options.

Additional slot options and values:

`:accessor-init': a sexp that initializes the slot in the
accessor if the slot is nil.  The symbol `struct' will be bound
to the current struct.  The accessor is defined after the struct
is fully defined, so it may refer to the struct
definition (e.g. by using the `cl-struct' `pcase' macro).

`:aliases': A list of symbols which will be aliased to the slot
accessor, prepended with the struct name (e.g. a struct `ts' with
slot `year' and alias `y' would create an alias `ts-y')."
  (declare (indent defun))
  ;; FIXME: Compiler warnings about accessors defined multiple times.  Not sure if we can fix this
  ;; except by ignoring warnings.
  (let* ((struct-name (car args))
         (struct-slots (cdr args))
         (cl-defstruct-expansion (macroexpand `(cl-defstruct ,struct-name ,@struct-slots)))
         accessor-forms alias-forms)
    (cl-loop for slot in struct-slots
             for pos from 1
             when (listp slot)
             do (-let* (((slot-name _slot-default . slot-options) slot)
                        ((&keys :accessor-init :aliases) slot-options)
                        (accessor-name (intern (concat (symbol-name struct-name) "-" (symbol-name slot-name))))
                        (accessor-docstring (format "Access slot \"%s\" of `%s' struct STRUCT."
                                                    slot-name struct-name))
                        (struct-pred (intern (concat (symbol-name struct-name) "-p")))
                        ;; Accessor form copied from macro expansion of `cl-defstruct'.
                        (accessor-form `(cl-defsubst ,accessor-name (struct)
                                          ,accessor-docstring
                                          ;; FIXME: side-effect-free is probably not true here, but what about error-free?
                                          ;;  (declare (side-effect-free error-free))
                                          (or (,struct-pred struct)
                                              (signal 'wrong-type-argument
                                                      (list ',struct-name struct)))
                                          ,(when accessor-init
                                             `(unless (aref struct ,pos)
                                                (aset struct ,pos ,accessor-init)))
                                          ;; NOTE: It's essential that this `aref' form be last
                                          ;; so the gv-setter works in the compiler macro.
                                          (aref struct ,pos))))
                  (push accessor-form accessor-forms)
                  ;; Remove accessor forms from `cl-defstruct' expansion.  This may be distasteful,
                  ;; but it would seem more distasteful to copy all of `cl-defstruct' and potentially
                  ;; have the implementations diverge in the future when Emacs changes (e.g. the new
                  ;; record type).
                  (cl-loop for form in-ref cl-defstruct-expansion
                           do (pcase form
                                (`(cl-defsubst ,(and accessor (guard (eq accessor accessor-name)))
                                      . ,_)
                                 accessor ; Silence "unused lexical variable" warning.
                                 (setf form nil))))
                  ;; Alias definitions.
                  (cl-loop for alias in aliases
                           for alias-name = (intern (concat (symbol-name struct-name) "-" (symbol-name alias)))
                           do (push `(defalias ',alias-name ',accessor-name) alias-forms))
                  ;; TODO: Setter
                  ;; ,(when (plist-get slot-options :reset)
                  ;;    `(gv-define-setter ,accessor-name (ts value)
                  ;;       `(progn
                  ;;          (aset ,ts ,,pos ,value)
                  ;;          (setf (ts-unix ts) ni))))
                  ))
    `(progn
       ,cl-defstruct-expansion
       ,@accessor-forms
       ,@alias-forms)))

;;;; Variables

(defvar org-ql-view-section-depth -1
  "Depth of current section.
Used in `org-ql-view-string'.")

(org-ql-defkeymap org-ql-view-map nil
  "Map for `org-ql-view' maps."
  [tab] org-ql-view-section-toggle)

;;;; Classes

(org-ql-defclass org-ql-view-section ()
  ;; TODO: Decide/clarify whether beg/end should/can be markers.
  ;; NOTE: Not using initforms for now.
  ((beg :initarg :beg
        :documentation "Position or marker at which section begins in buffer.")
   (end :initarg :end
        :documentation "Position or marker at which section ends in buffer.")
   (header :initarg :header
           :documention "Optional header string.")
   (contents-beg :initarg :contents-beg
                 :documentation "Position or marker at which section's contents begins in buffer (i.e. where the header ends).")
   (items :initarg :items
          :documentation "List of items the section contains.")
   (keymap :initarg :keymap
           :instance-initform org-ql-view-map
           :documentation "Keymap used inside section.")
   (collapsed :initarg :collapsed
              :initform nil
              :documentation "Whether the section is collapsed.")))

;;;; Structs

(org-ql-defstruct org-ql-item
  buffer marker beg category level todo priority heading tags deadline-element scheduled-element properties
  (deadline-ts nil :accessor-init (when-let* ((element (org-ql-item-deadline-element struct)))
                                    (ts-parse-org-element element)))
  (scheduled-ts nil :accessor-init (when-let* ((element (org-ql-item-scheduled-element struct)))
                                     (ts-parse-org-element element))))

;;;; Customization

(defgroup org-ql-view nil
  "Options for `org-ql' views."
  :group 'org-ql)

(defcustom org-ql-view-item-indent "  "
  "String used to indent individual items."
  :type 'string)

(defcustom org-ql-view-item-indent-per-level 4
  "Number of spaces used to indent items at each levep."
  :type 'integer)

;;;; Commands

(defun org-ql-view-section-toggle (section)
  "Toggle SECTION, or section at point."
  (interactive (list (org-ql-view-section-at)))
  (when section
    (with-slots (collapsed) section
      (if collapsed
          (org-ql-section-expand section)
        (org-ql-section-collapse section)))))

(defun org-ql-section-collapse (section)
  "Collapse SECTION, or section at point."
  (interactive (list (org-ql-view-section-at)))
  (when section
    (with-slots (collapsed contents-beg end) section
      (setf collapsed t)
      (let ((ov (make-overlay contents-beg end)))
        ;; I don't know if `evaporate' is necessary, but `magit-section' does it.
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'invisible t)))))

(defun org-ql-section-expand (section)
  "Expand SECTION, or section at point."
  (interactive (list (org-ql-view-section-at)))
  (when section
    (with-slots (collapsed contents-beg end) section
      (setf collapsed nil)
      (remove-overlays contents-beg end 'invisible t))))

;;;; Methods

(cl-defmethod org-ql-view-insert ((section org-ql-view-section) &key group-by)
  "Insert SECTION into current buffer."
  (cl-labels ((group (items fns &key parent-header)
                     (setf items (-group-by (car fns) items))
                     (if (cdr fns)
                         (--map (org-ql-view-section
                                 :header (car it)
                                 :items (group (cdr it) (cdr fns) :parent-header (car it)))
                                items)
                       (cond ((assoc nil items)
                              (append (--map (org-ql-view-section
                                              :header (car it)
                                              :items (cdr it))
                                             (butlast items))
                                      (cdr (assoc nil items))))
                             (t (--map (org-ql-view-section
                                        :header (car it)
                                        :items (cdr it))
                                       items))))))
    (with-slots (beg end header contents-beg items keymap) section
      (let* ((org-ql-view-section-depth (1+ org-ql-view-section-depth))
             (org-ql-view-item-indent (s-repeat (* org-ql-view-item-indent-per-level
                                                   org-ql-view-section-depth)
                                                " ")))
        (when group-by
          (setf items (group items group-by)))
        (setf beg (point))
        (insert org-ql-view-item-indent (org-ql-view-header section))
        (put-text-property beg (point) :org-ql-view-section section)
        (setf contents-beg (point))
        (let* ((org-ql-view-item-indent (s-repeat (* org-ql-view-item-indent-per-level
                                                     (1+ org-ql-view-section-depth))
                                                  " ")))
          (dolist (item items)
            (insert "\n")
            (cl-typecase item
              (org-ql-view-section (org-ql-view-insert item))
              (list (--each item
                      (insert "\n")
                      (org-ql-view-insert it)))
              (otherwise (org-ql-view-insert item))
              )))
        (setf end (point))
        (put-text-property beg (point) 'keymap keymap)))))

;; (cl-defmethod org-ql-view-insert ((sections list))
;;   "Insert SECTIONS into current buffer."
;;   (let* ((org-ql-view-section-depth (1+ org-ql-view-section-depth)))
;;     (dolist (section sections)
;;       (insert "\n")
;;       (org-ql-view-insert section))))
(cl-defmethod org-ql-view-insert ((sections list))
  "Insert SECTIONS into current buffer."
  (error "OOPS"))

(cl-defmethod org-ql-view-header ((section org-ql-view-section))
  "Return header string for SECTION.
Does not include newline."
  (cl-labels ((num-items (items)
                         (cl-loop for item in items
                                  sum (cl-typecase item
                                        (org-ql-view-section (num-items (oref item items)))
                                        (list (cl-loop for section in item
                                                       sum (num-items (oref section items))))
                                        (otherwise 1)))))
    (with-slots (header items) section
      (concat (propertize (format "%s"
                                  (or header "Section"))
                          'face 'magit-section-heading)
              " (" (number-to-string (num-items items)) ")"))))

(cl-defmethod org-ql-view-insert ((item org-ql-item))
  "Insert ITEM into current buffer.
Does not insert newline."
  (pcase-let* (((cl-struct org-ql-item todo priority heading tags) item)
               (todo (when todo
                       (propertize todo 'face (org-get-todo-face todo))))
               (priority (when priority
                           (propertize (concat "[#" priority "]") 'face (org-get-priority-face priority))))
               (tags (when tags
                       (propertize (s-join ":" tags) 'face 'org-tag-group)
                       ))
               (beg (point)))
    (insert org-ql-view-item-indent)
    (when todo
      (insert todo " "))
    (when priority
      (insert priority " "))
    (when heading
      (insert heading " "))
    (when tags
      (insert tags))
    (put-text-property beg (point) :org-ql-view-section item)))

(cl-defmethod org-ql-view-insert ((item string))
  "Insert ITEM into current buffer."
  (insert item))

;;;; Functions

(cl-defun org-ql-view-section-at (&optional (pos (point-at-bol)))
  (get-text-property pos :org-ql-view-section))

(defun org-ql-item-at ()
  ;; FIXME: Put this function elsewhere.
  "Return `org-ql-item' for entry at point."
  (-let* (((_ (&plist :begin :level :raw-value :priority :tags :todo-keyword :deadline :scheduled :CATEGORY))
           (org-element-headline-parser (line-end-position))))
    (make-org-ql-item :beg begin
                      :category CATEGORY
                      :level level
                      :todo todo-keyword
                      :priority (when priority
                                  (char-to-string priority))
                      :heading raw-value
                      :tags tags
                      :deadline-element deadline
                      :scheduled-element scheduled)))

(defun org-ql-view-sort-todo (items)
  "Return ITEMS sorted by to-do keyword, in to-do keyword order.
Uses `org-todo-keywords', which does not include buffer-local
keywords."
  (cl-flet ((keyword (string)
                     ;; Return keyword without parenthesized options.
                     (unless (string= string "|")
                       (if (string-match (rx (group (minimal-match (1+ anything)))
                                             "(" (1+ anything) ")")
                                         string)
                           (match-string 1 string)
                         string))))
    (let ((keywords (cl-loop with non-done-keywords with done-keywords
                             for (_type . keywords) in (reverse org-todo-keywords)
                             for non-done = (->> keywords
                                                 (--take-while (not (string= it "|")))
                                                 (-map #'keyword))
                             for done = (-map #'keyword
                                              (-slice keywords
                                                      (1+ (or (--find-index (string= it "|")
                                                                            keywords)
                                                              -1))))
                             do (progn
                                  (setf non-done-keywords (append non-done non-done-keywords))
                                  (setf done-keywords (append done done-keywords)))
                             finally return (append non-done-keywords done-keywords))))
      (-sort (lambda (a b)
               (cond ((and (org-ql-item-todo a)
                           (org-ql-item-todo b))
                      (< (or (cl-position (org-ql-item-todo a) keywords :test #'string=) 0)
                         (or (cl-position (org-ql-item-todo b) keywords :test #'string=) 0)))
                     ((org-ql-item-todo a) t)
                     ((org-ql-item-todo b) nil)))
             items))))


(defun org-ql-view-sort-planning (items)
  "Return ITEMS sorted by planning date."
  (cl-flet ((planning-ts (item)
                         (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))))
    (sort items (lambda (a b)
                  (let ((a-ts (planning-ts a))
                        (b-ts (planning-ts b)))
                    (cond ((and a-ts b-ts)
                           (ts< a-ts b-ts))
                          (a-ts t)
                          (b-ts nil)))))))

;;;; Footer

(provide 'org-ql-view-section)

;;; org-ql-view-section.el ends here
