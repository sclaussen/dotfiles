;;=============================================================================
;; Smart Notes Mode - Markdown-based meeting notes with Claude integration
;;=============================================================================

(require 'markdown-mode)

;; Configuration
(defvar smart-notes-directory "~/meetings/"
  "Base directory for meeting notes.")

(defvar smart-notes-active-directory "~/meetings/active/"
  "Directory for active/in-progress meeting notes.")

(defvar smart-notes-history-directory "~/meetings/history/"
  "Directory for completed meeting notes.")

(defvar smart-notes-aggregated-directory "~/meetings/aggregated/"
  "Directory for aggregated content (todos, urls, summaries).")

(defvar smart-notes-auto-process t
  "Whether to automatically process notes with Claude on save.")

;; Ensure directories exist
(unless (file-exists-p smart-notes-directory)
  (make-directory smart-notes-directory t))

(unless (file-exists-p smart-notes-active-directory)
  (make-directory smart-notes-active-directory t))

(unless (file-exists-p smart-notes-history-directory)
  (make-directory smart-notes-history-directory t))

(unless (file-exists-p smart-notes-aggregated-directory)
  (make-directory smart-notes-aggregated-directory t))

;;=============================================================================
;; Utility Functions
;;=============================================================================

(defun smart-notes--generate-filename ()
  "Generate a filename for a new meeting: YYYYMMDD-RANDOM.md"
  (let ((date (format-time-string "%Y%m%d"))
        (random (format "%04x" (random 65536))))
    (format "%s-%s.md" date random)))

(defun smart-notes--get-current-datetime ()
  "Get current date/time for meeting headers."
  (format-time-string "%Y-%m-%d %H:%M"))

(defun smart-notes--meeting-template ()
  "Generate template content for a new meeting."
  (format "# Meeting - %s

## Attendees
-

## Agenda
-

## Notes


## Actions
-

## URLs
-

## <cc>Summary</cc>

" (smart-notes--get-current-datetime)))

;;=============================================================================
;; Core Functions
;;=============================================================================

(defun smart-notes-new-meeting ()
  "Create a new meeting file with template in active directory."
  (interactive)
  (let* ((filename (smart-notes--generate-filename))
         (filepath (expand-file-name filename smart-notes-active-directory)))
    (find-file filepath)
    (insert (smart-notes--meeting-template))
    (goto-char (point-min))
    (forward-line 2) ; Position after "## Attendees"
    (end-of-line)
    (message "New meeting created: %s" filename)))

(defun smart-notes-complete-meeting ()
  "Complete meeting: move to history, add to master file."
  (interactive)
  (when (and buffer-file-name
             (string-match-p (regexp-quote smart-notes-active-directory) buffer-file-name))
    (let* ((current-name (file-name-nondirectory buffer-file-name))
           (new-name (read-string "Meeting name: "
                                 (replace-regexp-in-string
                                  "-[0-9a-f]+\\.md$" "" current-name)))
           (date-part (substring current-name 0 8))
           (new-filename (format "%s-%s.md" date-part new-name))
           (new-filepath (expand-file-name new-filename smart-notes-history-directory)))

      ;; Save current content
      (save-buffer)

      ;; Process with Claude one final time
      (smart-notes-process-meeting)

      ;; Move to history
      (rename-file buffer-file-name new-filepath)
      (set-visited-file-name new-filepath)
      (set-buffer-modified-p nil)

      ;; Add to master aggregated file
      (smart-notes--add-to-master new-filepath)

      (message "Meeting completed and moved to history: %s" new-filename))))

;;=============================================================================
;; Claude Integration
;;=============================================================================

(defun smart-notes-process-cc-tags ()
  "Process <cc>...</cc> tags in current buffer with Claude."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<cc>\\(.*?\\)</cc>" nil t)
      (let* ((prompt (match-string 1))
             (start (match-beginning 0))
             (end (match-end 0))
             (claude-response (smart-notes--call-claude prompt)))
        (when claude-response
          (delete-region start end)
          (insert claude-response))))))

(defun smart-notes--call-claude (prompt)
  "Call Claude with a prompt and return response."
  (let ((temp-file (make-temp-file "smart-notes-")))
    (with-temp-file temp-file
      (insert prompt))
    (let ((result (shell-command-to-string
                   (format "claude -p '%s'" temp-file))))
      (delete-file temp-file)
      (string-trim result))))

(defun smart-notes--extract-sections (pattern)
  "Extract content from sections matching PATTERN in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (matches)
      (while (re-search-forward pattern nil t)
        (forward-line 1)
        (let ((start (point))
              (end (if (re-search-forward "^##\\|^#\\|\\'" nil t)
                       (match-beginning 0)
                     (point-max))))
          (push (string-trim (buffer-substring-no-properties start end)) matches)))
      (nreverse matches))))

(defun smart-notes--extract-todos ()
  "Extract todos from current buffer."
  (let ((actions (smart-notes--extract-sections "^## Actions"))
        (todos '()))
    (dolist (section actions)
      (with-temp-buffer
        (insert section)
        (goto-char (point-min))
        (while (re-search-forward "^- \\(.+\\)" nil t)
          (push (match-string 1) todos))))
    (nreverse todos)))

(defun smart-notes--extract-urls ()
  "Extract URLs from current buffer."
  (let ((url-sections (smart-notes--extract-sections "^## URLs"))
        (urls '()))
    (dolist (section url-sections)
      (with-temp-buffer
        (insert section)
        (goto-char (point-min))
        (while (re-search-forward "^- \\(.+\\)" nil t)
          (push (match-string 1) urls))))
    ;; Also find URLs in content
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "https?://[[:graph:]]+" nil t)
        (push (match-string 0) urls)))
    (delete-dups (nreverse urls))))

(defun smart-notes--append-to-aggregated (filename content)
  "Append CONTENT to aggregated file FILENAME."
  (when content
    (let ((filepath (expand-file-name filename smart-notes-aggregated-directory))
          (timestamp (format-time-string "%Y-%m-%d %H:%M"))
          (meeting-name (if buffer-file-name
                           (file-name-nondirectory buffer-file-name)
                         "unknown")))
      (with-temp-buffer
        (when (file-exists-p filepath)
          (insert-file-contents filepath))
        (goto-char (point-max))
        (insert (format "\n\n## %s - %s\n" timestamp meeting-name))
        (dolist (item content)
          (insert (format "- %s\n" item)))
        (write-file filepath)))))

(defun smart-notes-process-meeting ()
  "Process current meeting: extract todos, URLs, generate summary."
  (interactive)
  (when (and buffer-file-name
             (or (string-match-p (regexp-quote smart-notes-active-directory) buffer-file-name)
                 (string-match-p (regexp-quote smart-notes-history-directory) buffer-file-name)))
    (save-buffer)

    ;; Process <cc> tags
    (smart-notes-process-cc-tags)

    ;; Extract and aggregate content
    (let ((todos (smart-notes--extract-todos))
          (urls (smart-notes--extract-urls)))
      (smart-notes--append-to-aggregated "todos.md" todos)
      (smart-notes--append-to-aggregated "urls.md" urls))

    ;; Generate summary
    (smart-notes--generate-summary)

    (save-buffer)
    (message "Meeting processed and aggregated")))

(defun smart-notes--generate-summary ()
  "Generate meeting summary using Claude."
  (let* ((content (buffer-string))
         (prompt (format "Summarize this meeting in 2-3 sentences:\n\n%s" content))
         (summary (smart-notes--call-claude prompt))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (meeting-name (if buffer-file-name
                          (file-name-nondirectory buffer-file-name)
                        "unknown"))
         (filepath (expand-file-name "summaries.md" smart-notes-aggregated-directory)))
    (when summary
      (with-temp-buffer
        (when (file-exists-p filepath)
          (insert-file-contents filepath))
        (goto-char (point-max))
        (insert (format "\n\n## %s - %s\n\n%s\n" timestamp meeting-name summary))
        (write-file filepath)))))

(defun smart-notes--add-to-master (meeting-filepath)
  "Add completed meeting to master aggregated file with Claude enhancement."
  (let* ((content (with-temp-buffer
                    (insert-file-contents meeting-filepath)
                    (buffer-string)))
         (meeting-name (file-name-nondirectory meeting-filepath))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (master-filepath (expand-file-name "master.md" smart-notes-aggregated-directory))

         ;; Enhanced processing with Claude
         (enhanced-prompt (format "Process this completed meeting for the master archive.
Provide:
1. Comprehensive summary (3-4 sentences)
2. Key decisions made
3. Important action items
4. Notable discussion points
5. Any connections to previous meetings/topics

Meeting content:
%s" content))
         (enhanced-content (smart-notes--call-claude enhanced-prompt)))

    (when enhanced-content
      (with-temp-buffer
        (when (file-exists-p master-filepath)
          (insert-file-contents master-filepath))
        (goto-char (point-max))

        ;; Add separator and header
        (insert (format "\n\n%s\n" (make-string 80 ?=)))
        (insert (format "# %s - %s\n\n" timestamp meeting-name))

        ;; Add Claude-enhanced content
        (insert enhanced-content)
        (insert "\n\n")

        ;; Add original content in collapsed section
        (insert "<details>\n<summary>Original Meeting Notes</summary>\n\n")
        (insert content)
        (insert "\n</details>\n")

        (write-file master-filepath)))))

;;=============================================================================
;; Navigation and Search
;;=============================================================================

(defun smart-notes-find-meetings ()
  "Search across all meeting files (active and history)."
  (interactive)
  (let ((search-term (read-string "Search meetings for: ")))
    (with-current-buffer (get-buffer-create "*Smart Notes Search*")
      (erase-buffer)
      (insert "# Meeting Search Results\n\n")
      (insert (format "Search term: **%s**\n\n" search-term))

      ;; Search active meetings
      (insert "## Active Meetings\n\n")
      (let ((default-directory smart-notes-active-directory))
        (shell-command (format "rg -n '%s' *.md 2>/dev/null || echo 'No matches in active meetings'"
                              search-term) t))

      ;; Search history
      (insert "\n## Meeting History\n\n")
      (let ((default-directory smart-notes-history-directory))
        (shell-command (format "rg -n '%s' *.md 2>/dev/null || echo 'No matches in meeting history'"
                              search-term) t))

      ;; Search aggregated content
      (insert "\n## Aggregated Content\n\n")
      (let ((default-directory smart-notes-aggregated-directory))
        (shell-command (format "rg -n '%s' *.md 2>/dev/null || echo 'No matches in aggregated content'"
                              search-term) t))

      (goto-char (point-min))
      (markdown-mode)
      (pop-to-buffer (current-buffer)))))

(defun smart-notes-view-todos ()
  "View aggregated todos."
  (interactive)
  (find-file (expand-file-name "todos.md" smart-notes-aggregated-directory)))

(defun smart-notes-view-urls ()
  "View aggregated URLs."
  (interactive)
  (find-file (expand-file-name "urls.md" smart-notes-aggregated-directory)))

(defun smart-notes-view-summaries ()
  "View meeting summaries."
  (interactive)
  (find-file (expand-file-name "summaries.md" smart-notes-aggregated-directory)))

(defun smart-notes-view-master ()
  "View master aggregated file with all meetings."
  (interactive)
  (find-file (expand-file-name "master.md" smart-notes-aggregated-directory)))

;;=============================================================================
;; Smart List Management
;;=============================================================================

(defun smart-notes-smart-return ()
  "Smart return: continue lists, create new items, or normal return."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (cond
     ;; Bullet list
     ((string-match "^\\s-*- " line)
      (let ((indent (string-match "[^ \t]" line)))
        (end-of-line)
        (newline)
        (insert (make-string indent ?\s) "- ")))

     ;; Numbered list
     ((string-match "^\\s-*\\([0-9]+\\)\\. " line)
      (let* ((indent (string-match "[^ \t]" line))
             (num (1+ (string-to-number (match-string 1 line)))))
        (end-of-line)
        (newline)
        (insert (make-string indent ?\s) (format "%d. " num))))

     ;; Default
     (t
      (newline-and-indent)))))

(defun smart-notes-smart-tab ()
  "Smart tab: indent/outdent lists or normal tab."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (cond
     ;; On a list item
     ((string-match "^\\s-*\\(- \\|[0-9]+\\. \\)" line)
      (beginning-of-line)
      (insert "    "))

     ;; Default
     (t
      (indent-for-tab-command)))))

(defun smart-notes-smart-backtab ()
  "Smart backtab: outdent lists."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^\\s-*\\(- \\|[0-9]+\\. \\)" line)
      (beginning-of-line)
      (when (looking-at "    ")
        (delete-char 4)))))

;;=============================================================================
;; Mode Definition
;;=============================================================================

(defvar smart-notes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Core workflow
    (define-key map (kbd "C-c n") 'smart-notes-new-meeting)
    (define-key map (kbd "C-c s") 'smart-notes-process-meeting)
    (define-key map (kbd "C-c c") 'smart-notes-process-cc-tags)
    (define-key map (kbd "C-c f") 'smart-notes-find-meetings)
    (define-key map (kbd "C-c C-c") 'smart-notes-complete-meeting)

    ;; List management
    (define-key map (kbd "M-RET") 'smart-notes-smart-return)
    (define-key map (kbd "TAB") 'smart-notes-smart-tab)
    (define-key map (kbd "<backtab>") 'smart-notes-smart-backtab)

    ;; View aggregated content
    (define-key map (kbd "C-c v") 'smart-notes-view-todos)
    (define-key map (kbd "C-c u") 'smart-notes-view-urls)
    (define-key map (kbd "C-c r") 'smart-notes-view-summaries)
    (define-key map (kbd "C-c m") 'smart-notes-view-master)

    map)
  "Keymap for Smart Notes mode.")

;;;###autoload
(define-derived-mode smart-notes-mode markdown-mode "SmartNotes"
  "Major mode for smart meeting notes with Claude integration.

\\{smart-notes-mode-map}"
  ;; Auto-process on save
  (when smart-notes-auto-process
    (add-hook 'after-save-hook 'smart-notes-process-meeting nil t))

  ;; Enable visual-line-mode for better text flow
  (visual-line-mode 1)

  ;; Customize markdown settings
  (setq-local markdown-asymmetric-header t)
  (setq-local markdown-list-item-bullets '("- " "+ " "* "))

  (message "Smart Notes mode enabled. Use C-c n for new meeting."))

;; Auto-mode for .md files in meetings directories
(add-to-list 'auto-mode-alist
             (cons (concat (regexp-quote (expand-file-name smart-notes-active-directory)) ".*\\.md\\'")
                   'smart-notes-mode))

(add-to-list 'auto-mode-alist
             (cons (concat (regexp-quote (expand-file-name smart-notes-history-directory)) ".*\\.md\\'")
                   'smart-notes-mode))

;; Global keybinding for new meeting
(global-set-key (kbd "C-c n") 'smart-notes-new-meeting)

(provide 'smart-notes)

;;; smart-notes.el ends here