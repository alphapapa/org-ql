(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (sub-section1 (org-ql-view-section
                      :items (list (make-org-ql-item :level 1
                                                     :todo "TODO"
                                                     :priority "A"
                                                     :heading "Alpha"
                                                     :tags '("one" "two"))
                                   (make-org-ql-item :level 1
                                                     :todo "NEXT"
                                                     :priority "B"
                                                     :heading "Bravo"
                                                     :tags '("one" "two" "three")))))
       (sub-section2 (org-ql-view-section
                      :items (list (make-org-ql-item :level 1
                                                     :todo "MAYBE"
                                                     :heading "Charlie"
                                                     :tags '("one" "two"))
                                   (make-org-ql-item :level 1
                                                     :todo "SOMEDAY"
                                                     :heading "Delta"
                                                     :tags '("one" "two" "three")))))
       (top-section (org-ql-view-section
                     :items (list sub-section1 sub-section2)))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section)
    (pop-to-buffer buffer)))

(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (items (->> (org-ql-select "~/src/emacs/org-ql/tests/data.org"
                     '(todo)
                     :action #'org-ql-item-at)
                   (-sort (-on #'string< #'org-ql-item-priority))
                   (org-ql-view-sort-todo)))
       (top-section (org-ql-view-section
                     :header (format "To-Do (%s)" (length items))
                     :items items))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section
                        :group-by '(org-ql-item-todo org-ql-item-priority))
    (pop-to-buffer buffer)))

(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (items (->> (org-ql-select "~/src/emacs/org-ql/tests/data.org"
                     '(todo)
                     :action #'org-ql-item-at)
                   (-sort (-on #'string< #'org-ql-item-priority))
                   (org-ql-view-sort-planning)))
       (top-section (org-ql-view-section
                     :header (format "To-Do (%s)" (length items))
                     :items items))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section
                        :group-by (list (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%Y-%m-%d" it)))))
    (pop-to-buffer buffer)))
(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (items (->> (org-ql-select "~/src/emacs/org-ql/tests/data.org"
                     '(todo)
                     :action #'org-ql-item-at)
                   (-sort (-on #'string< #'org-ql-item-priority))
                   (org-ql-view-sort-planning)))
       (top-section (org-ql-view-section
                     :header "To-Do by Planning Date"
                     :items items))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section
                        :group-by (list (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%B %Y" it)))
                                        (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%d %B" it)))))
    (pop-to-buffer buffer)))
(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (items (->> (org-ql-select "~/src/emacs/org-ql/tests/data.org"
                     '(todo)
                     :action #'org-ql-item-at)
                   (-sort (-on #'string< #'org-ql-item-priority))
                   (org-ql-view-sort-planning)))
       (top-section (org-ql-view-section
                     :header "To-Do by Planning Date"
                     :items items))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section
                        :group-by (list 'org-ql-item-todo
                                        (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%B %Y" it)))
                                        (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%d %B" it)))
                                        ))
    (pop-to-buffer buffer)))
(let* ((buffer (get-buffer-create "test-org-ql-view-section"))
       (items (->> (org-ql-select "~/src/emacs/org-ql/tests/data.org"
                     '(todo)
                     :action #'org-ql-item-at)
                   (-sort (-on #'string< #'org-ql-item-priority))
                   (org-ql-view-sort-planning)))
       (top-section (org-ql-view-section
                     :header "To-Do by Planning Date"
                     :items items))
       (inhibit-read-only t))
  (with-current-buffer buffer
    (read-only-mode 1)
    (erase-buffer)
    (org-ql-view-insert top-section
                        :group-by (list 'org-ql-item-todo
                                        (lambda (item)
                                          (awhen (or (org-ql-item-deadline-ts item) (org-ql-item-scheduled-ts item))
                                            (ts-format "%B %Y" it)))

                                        ))
    (pop-to-buffer buffer)))
