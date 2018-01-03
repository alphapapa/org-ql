;; -*- lexical-binding: t; -*-


;;; Commentary:

;; This is just a proof-of-concept for how the agenda code might be
;; written in a more functional way, to avoid making multiple passes
;; through each file when building a multi-day agenda.

;; Unfortunately, however, it's more like a proof-of-nope, because
;; it's significantly slower than the existing agenda code.  Profiling
;; shows that =org-element-parse-buffer= is just not a fast function,
;; so most of the time spent is in that function, parsing the whole
;; buffer into a lisp tree.  The existing agenda code is not written
;; in a functional style, and it makes multiple passes through each
;; file when preparing a multi-day agenda view, but it avoids parsing
;; the /entire/ buffer the way =org-element-parse-buffer= does.

;; So I guess this approach won't work, unless someone can come up
;; with a version of =org-element-parse-buffer= that's about 10x
;; faster.

;;;; Principles

;;;;; Work on elements

;; We want to work on elements as returned by =org-element-parse-buffer=.  We only convert to strings at the very end, when we send the list of those to =org-agenda-finalize-entries=.

;;;;; Preserve the call to =org-agenda-finalize-entries=

;; We want to preserve the final call to =org-agenda-finalize-entries= to preserve compatibility with other packages and functions.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'dash)
(require 'cl-lib)

;;;; Macros

(cl-defun org-agenda-ng--test-lambda (&key all any none)
  `(lambda ()
     (and ,@(-non-nil (list (when all
                              `(and ,@(--map (cl-typecase it
                                               (function (list it))
                                               (cons `(apply #',(car it) ',(cdr it))))
                                             all)))
                            (when any
                              `(or ,@(--map (cl-typecase it
                                              (function (list it))
                                              (cons `(apply #',(car it) ',(cdr it))))
                                            any)))
                            (when none
                              `(not (or ,@(--map (cl-typecase it
                                                   (function (list it))
                                                   (cons `(apply #',(car it) ',(cdr it))))
                                                 none)))))))))

;;;; Tests

(defun org-agenda-ng--test-test.org (&rest args)
  ;; Helper function
  (apply #'org-agenda-ng--agenda :files '("~/src/emacs/org-super-agenda/test/test.org")
         args))

(defun org-agenda-ng--test-agenda ()
  (interactive)
  (org-agenda-ng--test-test.org
   :any '((org-agenda-ng--todo-p "TODO")
          (org-agenda-ng--date-p :deadline < "2017-08-05"))))

(defun org-agenda-ng--test-agenda2 ()
  (interactive)
  (org-agenda-ng--test-test.org
   :any `((org-agenda-ng--date-p :date <= ,(org-today))
          (org-agenda-ng--date-p :deadline <= ,(+ org-deadline-warning-days (org-today)))
          (org-agenda-ng--date-p :scheduled <= ,(org-today)))
   :none `((org-agenda-ng--todo-p org-done-keywords))))

(defun org-agenda-ng--test-agenda3 ()
  (interactive)
  (org-agenda-ng--test-test.org
   :any `((org-agenda-ng--date-p :date <= ,(org-today))
          (org-agenda-ng--date-p :deadline <= ,(+ org-deadline-warning-days (org-today)))
          (org-agenda-ng--date-p :scheduled <= ,(org-today)))
   :none `((org-agenda-ng--todo-p org-done-keywords))))

(defun org-agenda-ng--test-agenda-today ()
  (interactive)
  (org-agenda-ng--agenda
   :files "~/src/emacs/org-super-agenda/test/test.org"
   :any `((org-agenda-ng--date-p :date <= ,(org-today))
          (org-agenda-ng--date-p :deadline <= ,(+ org-deadline-warning-days (org-today)))
          (org-agenda-ng--date-p :scheduled <= ,(org-today)))
   :none `((org-agenda-ng--todo-p org-done-keywords))))

(defun org-agenda-ng--test-agenda-today ()
  (interactive)
  (org-agenda-ng--agenda
   :files "~/src/emacs/org-super-agenda/test/test.org"
   :any `((org-agenda-ng--date-p :date <= ,(org-today))
          (org-agenda-ng--date-p :deadline <= ,(+ org-deadline-warning-days (org-today)))
          (org-agenda-ng--date-p :scheduled <= ,(org-today)))
   :none `((org-agenda-ng--todo-p org-done-keywords))))

(defun org-agenda-ng--test-agenda-today ()
  (interactive)
  (org-agenda-ng--agenda
   :files org-agenda-files
   :any `((org-agenda-ng--date-p :date <= ,(org-today))
          (org-agenda-ng--date-p :deadline <= ,(+ org-deadline-warning-days (org-today)))
          (org-agenda-ng--date-p :scheduled <= ,(org-today)))
   :none `((org-agenda-ng--todo-p org-done-keywords))))

(defun org-agenda-ng--test-todo-list ()
  (interactive)
  (org-agenda-ng--test-test.org
   :any '(org-agenda-ng--todo-p)
   :none '((org-agenda-ng--todo-p org-done-keywords))))

;;;; Commands

(cl-defun org-agenda-ng--agenda (&key files all any none pred)
  (unless files
    (setq files (buffer-file-name (current-buffer))))
  (unless (listp files)
    (setq files (list files)))
  (mapc 'find-file-noselect files)
  (let* ((org-use-tag-inheritance t)
         (entries (-flatten
                   (--map (with-current-buffer (find-buffer-visiting it)
                            ;; FIXME: It would be more optimal to
                            ;; gather the entry at each matching
                            ;; position instead of returning a list of
                            ;; positions and then having to go back
                            ;; and get the entries at each position.
                            (let* ((positions (org-agenda-ng--filter-buffer :all all :any any :none none :pred pred))
                                   (elements (--map (org-with-point-at it
                                                      (org-element-headline-parser
                                                       ;; FIXME: This is a hack
                                                       (save-excursion
                                                         (forward-line 3)
                                                         (point))))
                                                    positions)))
                              (mapcar #'org-agenda-ng--format-element elements)))
                          files)))
         (result-string (org-agenda-finalize-entries entries 'agenda))
         (target-buffer (get-buffer-create "test-agenda-ng")))
    (with-current-buffer target-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert result-string)
      (read-only-mode 1)
      (pop-to-buffer (current-buffer)))))

;;;; Functions

;; (cl-defun org-agenda-ng--get-entries (file &key any-preds none-preds)
;;   "Return list of entries from FILE."
;;   (with-current-buffer (find-buffer-visiting file)
;;     (let* ((org-use-tag-inheritance t)
;;            (tree (cddr (org-element-parse-buffer 'headline)))
;;            (entries (--> (org-agenda-ng--filter-tree tree :any any-preds :none none-preds)
;;                          (mapcar #'org-agenda-ng--format-element it))))
;;       entries)))

(cl-defun org-agenda-ng--heading-positions (file)
  "Return list of heading positions in FILE."
  (with-current-buffer (find-buffer-visiting file)
    (save-excursion
      (goto-char (point-min))
      (cl-loop initially do (when (org-before-first-heading-p)
                              (outline-next-heading))
               collect (point)
               while (outline-next-heading)))))

;; (cl-defun org-agenda-ng--filter-buffer (&key any-preds none-preds)
;;   "Return positions of matching headings in current buffer.
;; Headings should return non-nil for any ANY-PREDS and nil for all
;; NONE-PREDS."
;;   (cl-flet ((pred () (and (--any? (cl-typecase it
;;                                     (function (funcall it))
;;                                     (cons (apply (car it) (cdr it))))
;;                                   any-preds)
;;                           ;; Don't return if any filters match
;;                           (or (null none-preds)
;;                               (--none? (cl-typecase it
;;                                          (function (funcall it))
;;                                          (cons (apply (car it) (cdr it))))
;;                                        none-preds)))))
;;     (org-with-wide-buffer
;;      (goto-char (point-min))
;;      (when (org-before-first-heading-p)
;;        (outline-next-heading))
;;      (cl-loop when (pred)
;;               collect (point)
;;               while (outline-next-heading)))))

(cl-defun org-agenda-ng--filter-buffer (&key all any none pred)
  "Return positions of matching headings in current buffer.
Headings should return non-nil for any ANY-PREDS and nil for all
NONE-PREDS."
  ;; NOTE: Build lambda so typecase doesn't run for every item
  (let* ((our-lambda (when (or all any none)
                       (org-agenda-ng--test-lambda :all all :any any :none none)))
         (pred (cond ((and our-lambda pred)
                      (lambda ()
                        (and (funcall our-lambda)
                             (funcall pred))))
                     (our-lambda our-lambda)
                     (pred pred)
                     (t (user-error "No tests given")))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (org-before-first-heading-p)
       (outline-next-heading))
     (cl-loop when (funcall pred)
              collect (point)
              while (outline-next-heading)))))

;;;; Faces/properties

(defun org-agenda-ng--add-markers (element)
  "Return ELEMENT with marker properties added."
  (let* ((marker (org-agenda-new-marker (org-element-property :begin element)))
         (properties (--> (second element)
                          (plist-put it :org-marker marker)
                          (plist-put it :org-hd-marker marker))))
    (setf (second element) properties)
    element))

(defun org-agenda-ng--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with its text-properties set according to its property list.
Its property list should be the second item in the list, as returned by `org-element-parse-buffer'."
  (let* ((properties (second element))
         ;; Remove the :parent property, which so bloats the size of
         ;; the properties list that it makes it essentially
         ;; impossible to debug, because Emacs takes approximately
         ;; forever to show it in the minibuffer or with
         ;; `describe-text-properties'.  Also, remove ":" from key
         ;; symbols.
         (properties (cl-loop for (key val) on properties by #'cddr
                              for key = (intern (cl-subseq (symbol-name key) 1))
                              unless (member key '(parent))
                              append (list key val)))
         (title (--> (org-agenda-ng--add-faces element)
                     (org-element-property :raw-value it)
                     ;; (org-link-display-format it)
                     ))
         (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                 (org-agenda-ng--add-todo-face it)))
         (tag-list (if org-agenda-use-tag-inheritance
                       (if-let ((marker (or (org-element-property :org-hd-marker element)
                                            (org-element-property :org-marker element)
                                            (org-element-property :begin element))))
                           (org-with-point-at marker
                             (org-get-tags-at))
                         ;; No marker found
                         (warn "No marker found for item: %s" title)
                         (org-element-property :tags element))
                     (org-element-property :tags element)))
         (tag-string (-some--> tag-list
                               (s-join ":" it)
                               (s-wrap it ":")
                               (org-add-props it nil 'face 'org-tag)))
         (level (org-element-property :level element))
         (category (org-element-property :category element))
         (priority-string (-some->> (org-element-property :priority element)
                                    (char-to-string)
                                    (format "[#%s]")
                                    (org-agenda-ng--add-priority-face)))
         (string (s-join " " (list todo-keyword priority-string title tag-string))))
    (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
    ;; Add all the necessary properties and faces to the whole string
    (--> string
         (concat "  " it)
         (org-add-props it properties 'tags tag-list))))

(defun org-agenda-ng--add-faces (element)
  (->> element
       (org-agenda-ng--add-scheduled-face)
       (org-agenda-ng--add-deadline-face)))

(defun org-agenda-ng--add-priority-face (string)
  "Return STRING with priority face added."
  (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
    (let ((face (org-get-priority-face (string-to-char (match-string 2)))))
      (org-add-props string nil 'face face 'font-lock-fontified t))))

(defun org-agenda-ng--add-scheduled-face (element)
  "Add faces to ELEMENT's title for its scheduled status."
  (if-let ((scheduled-date (org-element-property :scheduled element)))
      (let* ((today-day-number (org-today))
             (scheduled-day-number (org-time-string-to-absolute
                                    (org-element-timestamp-interpreter scheduled-date 'ignore)))
             (todo-keyword (org-element-property :todo-keyword element))
             (done-p (member todo-keyword org-done-keywords))
             (today-p (= today-day-number scheduled-day-number))
             (face (cond
                    (done-p 'org-agenda-done)
                    (today-p 'org-scheduled-today)
                    (t 'org-scheduled)))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (second element)
                              (plist-put it :title title))))
        (list (car element)
              properties))
    ;; Not scheduled
    element))

(defun org-agenda-ng--add-deadline-face (element)
  "Add faces to ELEMENT's title for its deadline status."
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
             (todo-keyword (org-element-property :todo-keyword element))
             (done-p (member todo-keyword org-done-keywords))
             (today-p (= today-day-number deadline-day-number))
             (deadline-passed-fraction (--> (- deadline-day-number today-day-number)
                                            (float it)
                                            (/ it (max org-deadline-warning-days 1))
                                            (- 1 it)))
             (face (org-agenda-deadline-face deadline-passed-fraction))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (second element)
                              (plist-put it :title title))))
        (list (car element)
              properties))
    ;; No deadline
    element))

(defun org-agenda-ng--add-todo-face (keyword)
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;; Predicates

(defun org-agenda-ng--todo-p (&rest keywords)
  "Return non-nil if current heading is a TODO item.
With KEYWORDS, return non-nil if its keyword is one of KEYWORDS."
  (when-let ((state (org-get-todo-state)))
    (pcase keywords
      ('nil t)
      ;; ((pred stringp)
      ;;  (string= state keywords))
      ((pred listp)
       (member state keywords))
      ((pred symbolp)
       (member state (symbol-value keywords)))
      (otherwise (error "Invalid keyword argument: %s" otherwise)))))

(defun org-agenda-ng--date-p (type &optional comparator target-date)
  "Return non-nil if current heading has a date property of TYPE.
TYPE should be a keyword symbol, like :scheduled or :deadline.

With COMPARATOR and TARGET-DATE, return non-nil if entry's
scheduled date compares with TARGET-DATE according to COMPARATOR.
TARGET-DATE may be a string like \"2017-08-05\", or an integer
like one returned by `date-to-day'."
  (when-let ((entry-date (awhen (pcase type
                                  (:deadline (org-entry-get (point) "DEADLINE"))
                                  (:scheduled (org-entry-get (point) "SCHEDULED")))
                           ;; FIXME: Hack: since we're using
                           ;; (org-element-property :type entry-date)
                           ;; below, we need this date parsed into an
                           ;; org-element element
                           (with-temp-buffer
                             (insert it)
                             (goto-char 0)
                             (org-element-timestamp-parser))
                           )))
    (pcase comparator
      ;; Not comparing, just checking if it has one
      ('nil t)
      ;; Compare dates
      ((and (pred functionp) (guard target-date))
       (let ((target-day-number (cl-typecase target-date
                                  ;; Append time to target-date
                                  ;; because `date-to-day' requires it
                                  (string (date-to-day (concat target-date " 00:00")))
                                  (integer target-date))))
         (pcase (org-element-property :type entry-date)
           ((or 'active 'inactive)
            (funcall comparator
                     (org-time-string-to-absolute
                      (org-element-timestamp-interpreter entry-date 'ignore))
                     target-day-number))
           (error "Unknown entry-date type: %s" (org-element-property :type entry-date)))))
      (otherwise (error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string" comparator target-date)))))
