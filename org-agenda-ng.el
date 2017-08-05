(require 'org)
(require 'org-agenda)
(require 'dash)
(require 'cl-lib)

(defun org-agenda-ng--filter-tree (tree &key filter-fns)
  (let* ((types '(headline))
         (info nil)
         (first-match nil)
         (fun (lambda (element)
                (when (--all? (cl-typecase it
                                (function (funcall it element))
                                (cons (apply (car it) element (cdr it))))
                              filter-fns)
                  element))))
    (org-element-map tree types fun info first-match)))


(defun org-agenda-ng--test ()
  (let ((tree (cddr (org-element-parse-buffer 'headline)))
        (filter-fns '((org-agenda-ng--todo-p "TODO")
                      (org-agenda-ng--scheduled-p < "2017-08-04"))))
    (org-agenda-ng--filter-tree tree :filter-fns filter-fns)))



(defun org-agenda-ng--todo-p (element &optional keyword)
  "Return non-nil if ELEMENT is a TODO item.
With KEYWORD, return non-nil if it has the same TODO keyword."
  (when-let ((element-keyword (org-element-property :todo-keyword element)))
    (pcase keyword
      ('nil t)
      ((pred stringp)
       (string= element-keyword keyword))
      (otherwise (error "Invalid keyword argument: %s" otherwise)))))

(defun org-agenda-ng--scheduled-p (entry &optional comparator target-date)
  "Return non-nil if ENTRY is scheduled.
With COMPARATOR and DATE, return non-nil if entry's scheduled
date compares with TARGET-DATE according to COMPARATOR."

  (when-let ((scheduled-date (org-element-property :scheduled entry)))
    ;; Append time to target-date because `date-to-day' requires it
    (setq target-date (concat target-date " 00:00"))
    (setq target-day-number (date-to-day target-date))
    (pcase comparator
      ('nil t)
      ((and (pred functionp) (guard target-day-number))
       (pcase (org-element-property :type scheduled-date)
         ((or 'active 'inactive)
          (funcall comparator
                   (org-time-string-to-absolute
                    (org-element-timestamp-interpreter scheduled-date 'ignore))
                   target-day-number))))
      (otherwise (error "COMPARATOR (%s) must be a function, and DATE (%s) must be a string" comparator target-date)))))

(defun org-agenda-ng--add-faces (element)
  (org-agenda-ng--add-scheduled-faces element))

(defun org-agenda-ng--add-scheduled-faces (element)
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
             (title (--> (org-element-property :title element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (second element)
                              (plist-put it :title title))))
        (list (car element)
              properties))
    ;; Not scheduled
    element))

(defun org-agenda-ng--add-text-properties (element)
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
                     (org-element-property :title it)
                     (org-link-display-format it)
                     ;; Add element properties to the title so --add-faces can find them.
                     ;; MAYBE: Rewrite --add-faces/etc. to take a props argument.
                     (org-add-props it properties)
                     ))
         (todo-keyword (--> (org-element-property :todo-keyword element)
                            (org-agenda-ng--add-todo-face it)))
         (string (s-join " " (list todo-keyword title))))
    ;; Add all the necessary properties and faces to the whole string
    (--> string
         (org-add-props it properties))))

(defun org-agenda-ng--add-todo-face (keyword)
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;; (find-file-noselect "~/org/main.org")
;; (let ((buffer (find-buffer-visiting "~/org/main.org")))
;;   (with-current-buffer buffer
;;     (length (org-agenda-ng--test))))

(defun argh ()
  (interactive)
  (with-current-buffer (find-buffer-visiting "~/org/main.org")
    (let* ((entries (org-agenda-ng--test))
           (result-string (org-agenda-finalize-entries (mapcar #'org-agenda-ng--add-text-properties
                                                               entries)
                                                       'agenda))
           (target-buffer (get-buffer-create "test-agenda-ng")))
      (with-current-buffer target-buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert result-string)
        (read-only-mode 1)
        (pop-to-buffer (current-buffer))))))

;; (with-current-buffer (find-file "~/src/org-agenda-ng/org-agenda-ng.el")
;;   (eval-buffer))

;; (decode-time (days-to-time 730119))
;; (date-to-day "2017-08-04")

;; (date-to-day "2017-08-03")
