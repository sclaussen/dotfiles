(defvar todo-mode-map nil "Keymap for todo mode.")

(if todo-mode-map
    nil
  (setq todo-mode-map (make-sparse-keymap)))

(defun todo-mode ()
  "
Major mode for todos.

Mode specific key bindings:

  1        Add todo to section 1
  2        Add todo to section 2
  3        Add todo to section 3
  4        Add todo to section 4
  5        Add feature to section 5
  C-c a    Add an todo (prompts for priority)
  C-c o    Assign/Change an owner
  C-c d    Complete todo
  C-c C-c  Toggle edit mode
  C-c 1    Change item priority to 1
  C-c 2    Change item priority to 2
  C-c 3    Change item priority to 3
  C-c 4    Change item priority to 4

Variables:
  todo-file (default z:/todos/current.todo)
  todo-complete-file (default z:/todos/completed.txt)
"
  (interactive)

  (todo-buffer-display)

  (kill-all-local-variables)
  (use-local-map todo-mode-map)
  (setq major-mode 'todo-mode)
  (setq mode-name "Todo")

  (define-key todo-mode-map "n" 'todo-next-line)
  (define-key todo-mode-map "p" 'todo-previous-line)
  (define-key todo-mode-map "e" 'todo-edit)
  (define-key todo-mode-map "d" 'todo-done)
  (define-key todo-mode-map "c" 'todo-cancel)

  (define-key todo-mode-map "j" 'todo-raise-priority)
  (define-key todo-mode-map "k" 'todo-lower-priority)

  (define-key todo-mode-map "a" 'todo-create-p1)
  (define-key todo-mode-map "1" 'todo-create-p1)
  (define-key todo-mode-map "2" 'todo-create-p2)
  (define-key todo-mode-map "3" 'todo-create-p3)
  (define-key todo-mode-map "4" 'todo-create-p4)

  (setq truncate-lines t)
  (run-hooks 'todo-mode-hook))

(defun todo-buffer-display ()
  (let ((todo-buffer (get-file-buffer "~/.todo")))
    (if todo-buffer
        (switch-to-buffer todo-buffer)
      (find-file  "~/.todo"))))

(defun todo-next-line ()
  (interactive)
  (next-line 1)
  (beginning-of-line))

(defun todo-completed ()
  (interactive)
  (next-line 1)
  (beginning-of-line))

(defun todo-previous-line ()
  (interactive)
  (previous-line 1)
  (beginning-of-line))

(defun todo-edit ()
  (interactive)
  (setq mode-name "Todo Edit")
  (todo-toggle-keys mode-name))

(defun todo-quit-edit ()
  (interactive)
  (setq mode-name "Todo")
  (force-mode-line-update))

(defun todo-toggle-keys (mode-name)
  (force-mode-line-update))

  (define-key todo-mode-map "n" 'self-insert-command)
  (define-key todo-mode-map "p" 'self-insert-command)
  (define-key todo-mode-map "e" 'self-insert-command)
  (define-key todo-mode-map "c" 'self-insert-command)
  (define-key todo-mode-map "1" 'self-insert-command)
  (define-key todo-mode-map "2" 'self-insert-command)
  (define-key todo-mode-map "3" 'self-insert-command)
  (define-key todo-mode-map "4" 'self-insert-command)
  (define-key todo-mode-map "5" 'self-insert-command)
  (define-key todo-mode-map "\C-c\C-c" 'todo-quit-edit)
  (define-key todo-mode-map "1" 'todo-create-p1)
  (define-key todo-mode-map "2" 'todo-create-p2)
  (define-key todo-mode-map "3" 'todo-create-p3)
  (define-key todo-mode-map "4" 'todo-create-p4)
  (define-key todo-mode-map "\C-c\C-c" 'todo-edit)

(defun todo-create-p1 ()
  (interactive)
  (save-excursion
    (todo-create "1" nil todo-current-user)))

(defun todo-create-p2 ()
  (interactive)
  (save-excursion
    (todo-create "2" nil todo-current-user)))

(defun todo-create-p3 ()
  (interactive)
  (save-excursion
    (todo-create "3" nil todo-current-user)))

(defun todo-create-p4 ()
  (interactive)
  (save-excursion
    (todo-create "4" nil todo-current-user)))

(defun todo-create-priority ()
  (interactive)
  (save-excursion
    (let* ((section (read-from-minibuffer "Priority: "))
           (item (read-from-minibuffer "Item: "))
           (submitter todo-current-user))

      (todo-create section item submitter))))

(defun todo-create (priority)
  (save-excursion
    (let* ((todo (if todo-item todo-item (read-from-minibuffer "Item: ")))
           (section (concat "Priority: " priority)))
      (beginning-of-buffer)
      (insert (concat "  " (get-date) " (s:" submitter ") " todo "\n"))
      (save-buffer 1))))

(defun todo-update-p1 ()
  (interactive)
  (save-excursion
    (todo-update-priority "1")))

(defun todo-update-p2 ()
  (interactive)
  (save-excursion
    (todo-update-priority "2")))

(defun todo-update-p3 ()
  (interactive)
  (save-excursion
    (todo-update-priority "3")))

(defun todo-update-p4 ()
  (interactive)
  (save-excursion
    (todo-update-priority "4")))

(defun todo-update-priority (priority)

  (save-excursion

    (let ((todo (buffer-substring (progn (beginning-of-line) (point))
                                   (progn (end-of-line) (point)))))

      ;;find todo in current file
      (beginning-of-buffer)
      (search-forward todo nil nil)

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
          (insert todo "\n")
          (save-buffer 1)
          (message (concat "Todo moved to section " priority ".")))))))

(defun todo-done ()

  (interactive)

  (save-excursion
    (beginning-of-line)
    (let ((todo (buffer-substring (+ 2 (point)) (progn (end-of-line) (point))))
          (todo-buffer (current-buffer))
          (todo-complete-buffer (get-file-buffer todo-file-completed))
          (comments (read-from-minibuffer "Comments: ")))

      ;;find todo in current file
      (beginning-of-buffer)
      (search-forward todo nil nil)

      ;; Delete the todo (current line) from the todo buffer
      (delete-region (progn (beginning-of-line) (point))
                     (+ 1 (progn (end-of-line) (point))))
      (save-buffer 1)

      ;; Switch to the todo completed buffer
      (if todo-complete-buffer
          (switch-to-buffer todo-complete-buffer)
        (find-file todo-file-completed))

      ;; Append the todo to the end of the todo completed buffer
      (end-of-buffer)
      (insert (get-date) " " todo "\n")
      (insert "    " todo-current-user ": " comments "\n")
      (save-buffer 1)
      (kill-buffer (current-buffer))

      (switch-to-buffer todo-buffer)
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
