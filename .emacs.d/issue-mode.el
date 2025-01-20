(defvar issue-file-completed
  ""
  "* lll")
(make-variable-buffer-local 'issue-file-completed)

(defvar issue-mode-1-file
  "~/.issues.todo"
  "* lskdjfl")

(defvar issue-mode-1-file-completed
  "~/.completed.todo"
  "* lskdjfl")

(defvar issue-mode-1-name
  "todo"
  "* Buffer name *")

(defvar issue-mode-2-file
  "/home/shane/IssueFile2.issue"
  "* lskdjfl")

(defvar issue-mode-2-file-completed
  "/home/shane/IssueFile2.completed"
  "* lskdjfl")

(defvar issue-mode-2-name
  "IssueFile2"
  "* Buffer name *")

(defvar issue-mode-3-file
  "/home/shane/IssueFile3.issue"
  "* lskdjfl")

(defvar issue-mode-3-file-completed
  "/home/shane/IssueFile3.completed"
  "* lskdjfl")

(defvar issue-mode-3-name
  "IssueFile3"
  "* Buffer name *")

(defvar issue-mode-4-file
  "/home/shane/IssueFile4.issue"
  "* lskdjfl")

(defvar issue-mode-4-file-completed
  "/home/shane/IssueFile4.completed"
  "* lskdjfl")

(defvar issue-mode-4-name
  "IssueFile4"
  "* Buffer name *")

(defvar issue-mode-5-file
  "/home/shane/IssueFile5.issue"
  "* lskdjfl")

(defvar issue-mode-5-file-completed
  "/home/shane/IssueFile5.completed"
  "* lskdjfl")

(defvar issue-mode-5-name
  "IssueFile5"
  "* Buffer name *")

(defun issue-mode-1 ()
  (interactive)
  (issue-mode issue-mode-1-name issue-mode-1-file issue-mode-1-file-completed))

(defun issue-mode-2 ()
  (interactive)
  (issue-mode issue-mode-2-name issue-mode-2-file issue-mode-2-file-completed))

(defun issue-mode-3 ()
  (interactive)
  (issue-mode issue-mode-3-name issue-mode-3-file issue-mode-3-file-completed))

(defun issue-mode-4 ()
  (interactive)
  (issue-mode issue-mode-4-name issue-mode-4-file issue-mode-4-file-completed))

(defun issue-mode-5 ()
  (interactive)
  (issue-mode issue-mode-5-name issue-mode-5-file issue-mode-5-file-completed))

(defvar issue-current-user
  "xx"
  "* Two letter initials of user adding/completing todo items.")

(defvar issue-mode-map nil "Keymap for issue mode.")

(if issue-mode-map
    nil
  (setq issue-mode-map (make-sparse-keymap)))

(defun issue-mode (name file file-completed)
  "
Major mode for issues.

Mode specific key bindings:
  1        Add issue to section 1
  2        Add issue to section 2
  3        Add issue to section 3
  4        Add issue to section 4
  5        Add feature to section 5
  C-c a    Add an issue (prompts for priority)
  C-c o    Assign/Change an owner
  C-c d    Complete issue
  C-c C-c  Toggle edit mode
  C-c 1    Change item priority to 1
  C-c 2    Change item priority to 2
  C-c 3    Change item priority to 3
  C-c 4    Change item priority to 4

Variables:
  issue-file (default z:/issues/current.issue)
  issue-complete-file (default z:/issues/completed.txt)
"
  (interactive)

  (issue-buffer-display file)

  (kill-all-local-variables)
  (use-local-map issue-mode-map)
  (setq major-mode 'issue-mode)
  (setq mode-name (concat name " Issues"))
  (setq file-completed file-completed)

  (setq issue-file-completed file-completed)

  (define-key issue-mode-map "1" 'issue-add-1)
  (define-key issue-mode-map "2" 'issue-add-2)
  (define-key issue-mode-map "3" 'issue-add-3)
  (define-key issue-mode-map "4" 'issue-add-4)
  (define-key issue-mode-map "5" 'feature-add)
  (define-key issue-mode-map "\C-c1" 'issue-make-priority-1)
  (define-key issue-mode-map "\C-c2" 'issue-make-priority-2)
  (define-key issue-mode-map "\C-c3" 'issue-make-priority-3)
  (define-key issue-mode-map "\C-c4" 'issue-make-priority-4)
  (define-key issue-mode-map "\C-ca" 'issue-add)
  (define-key issue-mode-map "\C-cd" 'issue-done)
  (define-key issue-mode-map "\C-co" 'issue-owner-assign)
  (define-key issue-mode-map "\C-c\C-c" 'issue-edit)
  (setq truncate-lines t)
  (run-hooks 'issue-mode-hook))

(defun issue-buffer-display (issue-file)
  (let ((issue-buffer (get-file-buffer issue-file)))
    (if issue-buffer
        (switch-to-buffer issue-buffer)
      (find-file issue-file))
    (beginning-of-buffer)
    (if (equal nil (re-search-forward "^Priority: " nil t))
        (progn
          (insert "Priority: 1\n")
          (insert "----------------------------------------\n")
          (insert "\n")
          (insert "Priority: 2\n")
          (insert "----------------------------------------\n")
          (insert "\n")
          (insert "Priority: 3\n")
          (insert "----------------------------------------\n")
          (insert "\n")
          (insert "Priority: 4\n")
          (insert "----------------------------------------\n")
          (insert "\n")
          (insert "Features\n")
          (insert "----------------------------------------\n")
          (insert "\n")))
    (beginning-of-buffer)
    (next-line 2)
    (beginning-of-line)
    (save-buffer 1)))

(defun issue-edit ()
  (interactive)
  (define-key issue-mode-map "1" 'self-insert-command)
  (define-key issue-mode-map "2" 'self-insert-command)
  (define-key issue-mode-map "3" 'self-insert-command)
  (define-key issue-mode-map "4" 'self-insert-command)
  (define-key issue-mode-map "5" 'self-insert-command)
  (define-key issue-mode-map "\C-c\C-c" 'issue-quit-edit)
  (setq mode-name "Issue Edit")
  (force-mode-line-update))

(defun issue-quit-edit ()
  (interactive)
  (define-key issue-mode-map "1" 'issue-add-1)
  (define-key issue-mode-map "2" 'issue-add-2)
  (define-key issue-mode-map "3" 'issue-add-3)
  (define-key issue-mode-map "4" 'issue-add-4)
  (define-key issue-mode-map "5" 'feature-add)
  (define-key issue-mode-map "\C-c\C-c" 'issue-edit)
  (setq mode-name "Issue")
  (force-mode-line-update))

(defun issue-add-1 ()
  (interactive)
  (save-excursion
    (issue-add-with-priority "1" nil issue-current-user)))

(defun issue-add-2 ()
  (interactive)
  (save-excursion
    (issue-add-with-priority "2" nil issue-current-user)))

(defun issue-add-3 ()
  (interactive)
  (save-excursion
    (issue-add-with-priority "3" nil issue-current-user)))

(defun issue-add-4 ()
  (interactive)
  (save-excursion
    (issue-add-with-priority "4" nil issue-current-user)))

(defun issue-add ()
  (interactive)
  (save-excursion
    (let* ((section (read-from-minibuffer "Priority: "))
           (item (read-from-minibuffer "Item: "))
           (submitter issue-current-user))

      (issue-add-with-priority section item submitter))))

(defun issue-add-with-priority (priority issue-item submitter)

  (save-excursion

    (let* ((issue (if issue-item issue-item (read-from-minibuffer "Item: ")))
           (section (concat "Priority: " priority)))

      (beginning-of-buffer)

      (if (eq (search-forward section nil t) nil)

          (progn
            (ding)
            (message (concat section " not found, " issue " not added.")))

        (progn
          (beginning-of-line)
          (next-line 2)
          (insert (concat "  " (get-date) " (s:" submitter ") " issue "\n"))
          (previous-line 1)
          (save-buffer 1)
          (message (concat "Inserted " issue
                           " into section " priority ".")))))))

(defun feature-add ()
  (interactive)
  (save-excursion
    (let* ((feature (read-from-minibuffer "Feature: "))
           (submitter issue-current-user))
      (beginning-of-buffer)
      (if (eq (search-forward "Feature:" nil t) nil)
          (progn
            (ding)
            (message (concat "Feature section not found, " feature " not added.")))
        (progn
          (beginning-of-line)
          (next-line 2)
          (insert (concat "  " (get-date) " (s:" submitter ") " feature "\n"))
          (previous-line 1)
          (save-buffer 1)
          (message (concat "Inserted " feature)))))))

(defun issue-owner-assign ()
  (interactive)
  (save-excursion
    ;;must save a copy of current line
    (let ((old-line (buffer-substring (progn (beginning-of-line) (point))
                                      (progn (end-of-line) (point))))
          (owner (read-from-minibuffer "Owner: ")))

      ;;reload file and find line again from the copy
      (beginning-of-buffer)
      (search-forward old-line nil nil)

      ;;now assign owner
      (let ((eol-point (progn (end-of-line) (point))))

        (beginning-of-line)
        (if (re-search-forward "(o:" eol-point t nil)
            (progn
              (kill-word 1)
              (insert owner))
          (progn
            (beginning-of-line)
            (forward-word 4)
            (forward-char 2)
            (insert "(o:" owner ") ")
            (beginning-of-line)))
        (next-line 1)
        (beginning-of-line)))
    (save-buffer 1)))

(defun issue-make-priority-1 ()
  (interactive)
  (save-excursion
    (issue-make-priority "1")))

(defun issue-make-priority-2 ()
  (interactive)
  (save-excursion
    (issue-make-priority "2")))

(defun issue-make-priority-3 ()
  (interactive)
  (save-excursion
    (issue-make-priority "3")))

(defun issue-make-priority-4 ()
  (interactive)
  (save-excursion
    (issue-make-priority "4")))

(defun issue-make-priority (priority)

  (save-excursion

    (let ((issue (buffer-substring (progn (beginning-of-line) (point))
                                   (progn (end-of-line) (point)))))

      ;;find issue in current file
      (beginning-of-buffer)
      (search-forward issue nil nil)

      ;;update current file
      (delete-region (progn (beginning-of-line) (point))
                     (+ 1 (progn (end-of-line) (point))))

      (if (progn
            (beginning-of-buffer)
            (equal nil (re-search-forward
                        (concat "^Priority: " priority) nil t)))
          (progn
            (ding)
            (message (concat "Priority " priority " not found.")))

        (progn
          (beginning-of-line)
          (next-line 2)
          (insert issue "\n")
          (save-buffer 1)
          (message (concat "Issue moved to section " priority ".")))))))

(defun issue-done ()

  (interactive)

  (save-excursion
    (beginning-of-line)
    (let ((issue (buffer-substring (+ 2 (point)) (progn (end-of-line) (point))))
          (issue-buffer (current-buffer))
          (issue-complete-buffer (get-file-buffer issue-file-completed))
          (comments (read-from-minibuffer "Comments: ")))

      ;;find issue in current file
      (beginning-of-buffer)
      (search-forward issue nil nil)

      ;; Delete the issue (current line) from the issue buffer
      (delete-region (progn (beginning-of-line) (point))
                     (+ 1 (progn (end-of-line) (point))))
      (save-buffer 1)

      ;; Switch to the issue completed buffer
      (if issue-complete-buffer
          (switch-to-buffer issue-complete-buffer)
        (find-file issue-file-completed))

      ;; Append the issue to the end of the issue completed buffer
      (end-of-buffer)
      (insert (get-date) " " issue "\n")
      (insert "    " issue-current-user ": " comments "\n")
      (save-buffer 1)
      (kill-buffer (current-buffer))

      (switch-to-buffer issue-buffer)
      (message ""))))

(defun get-date ()

  (let* ((current-date (current-time-string))
         (month (substring current-date 4 7))
         (day (substring current-date 8 10)))

    (message day)

    (if (eq " " (substring day 0 1))
        (setq day (concat "0" (substring day 1))))

    (cond
     ((equal month "Jan")
      (concat "01/" day))
     ((equal month "Feb")
      (concat "02/" day))
     ((equal month "Mar")
      (concat "03/" day))
     ((equal month "Apr")
      (concat "04/" day))
     ((equal month "May")
      (concat "05/" day))
     ((equal month "Jun")
      (concat "06/" day))
     ((equal month "Jul")
      (concat "07/" day))
     ((equal month "Aug")
      (concat "08/" day))
     ((equal month "Sep")
      (concat "09/" day))
     ((equal month "Oct")
      (concat "10/" day))
     ((equal month "Nov")
      (concat "11/" day))
     ((equal month "Dec")
      (concat "12/" day)))))
