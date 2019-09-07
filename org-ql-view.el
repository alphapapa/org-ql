;;; org-ql-view.el --- Views for org-ql results  -*- lexical-binding: t; -*-

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

;; Inspired by `magit-section'.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)

(require 'dash)
(require 's)

(require 'org-ql-macros)

;;;; Variables

(defvar org-ql-view-depth -1
  "Depth of current section.
Used in `org-ql-view-string'.")

(org-ql-defkeymap org-ql-view-map nil
  "Map for `org-ql-view' maps."
  [tab] org-ql-view-section-toggle)

;;;; Faces

(defface org-ql-view-heading
  `((t (:height 1.2 :weight bold)))
  "Face for headings in `org-ql-view' buffers.")

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

;;;; Customization

(defgroup org-ql-view nil
  "Options for `org-ql' views."
  :group 'org-ql)

(defcustom org-ql-view-item-indent "  "
  "String used to indent individual items."
  :type 'string)

(defcustom org-ql-view-item-indent-per-level 3
  "Number of spaces used to indent items at each level."
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
  (cl-labels ((group (items fns)
                     (setf items (-group-by (car fns) items))
                     (if (cdr fns)
                         (--map (org-ql-view-section
                                 :header (car it)
                                 :items (group (cdr it) (cdr fns)))
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
      (let* ((org-ql-view-depth (1+ org-ql-view-depth))
             (org-ql-view-item-indent (s-repeat (* org-ql-view-item-indent-per-level
                                                   org-ql-view-depth)
                                                " ")))
        (when group-by
          (setf items (group items group-by)))
        (setf beg (point))
        (insert org-ql-view-item-indent (org-ql-view-header section))
        (put-text-property beg (point) :org-ql-view-section section)
        (setf contents-beg (point))
        (let* ((org-ql-view-item-indent (s-repeat (* org-ql-view-item-indent-per-level
                                                     (1+ org-ql-view-depth))
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
;;   (let* ((org-ql-view-depth (1+ org-ql-view-depth)))
;;     (dolist (section sections)
;;       (insert "\n")
;;       (org-ql-view-insert section))))
;; (cl-defmethod org-ql-view-insert ((sections list))
;;   "Insert SECTIONS into current buffer."
;;   (error "OOPS"))

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
                                  (or header "None"))
                          'face 'org-ql-view-heading)
              " (" (number-to-string (num-items items)) ")"))))

(cl-defmethod org-ql-view-insert ((item org-ql-item))
  "Insert ITEM into current buffer.
Does not insert newline."
  (pcase-let* (((cl-struct org-ql-item todo priority heading tags) item)
               (beg (point)))
    (insert org-ql-view-item-indent)
    (when todo
      (insert (propertize todo 'face (org-get-todo-face todo)) " "))
    (when priority
      (insert (propertize (concat "[#" priority "]") 'face (org-get-priority-face priority)) " "))
    (when heading
      (insert heading " "))
    (when tags
      (insert (propertize (s-join ":" tags) 'face 'org-tag-group)))
    (put-text-property beg (point) :org-ql-view-section item)))

(cl-defmethod org-ql-view-insert ((item string))
  "Insert ITEM into current buffer."
  (insert item))

;;;; Functions

(cl-defun org-ql-view-section-at (&optional (pos (point-at-bol)))
  "Return section at POS or point."
  (get-text-property pos :org-ql-view-section))

;;;; Footer

(provide 'org-ql-view)

;;; org-ql-view.el ends here
