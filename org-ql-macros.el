;;; org-ql-macros.el --- Macros used in org-ql       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

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

(require 'cl-lib)
(require 'eieio)

(require 'dash)

;;;; Macros

(defmacro org-ql-defkeymap (name copy docstring &rest maps)
  ;; Copied from `defkeymap' in elexandria.el.
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

;;;; Footer

(provide 'org-ql-macros)

;;; org-ql-macros.el ends here
