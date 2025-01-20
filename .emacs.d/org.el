;;=============================================================================
;; Org Mode
;;=============================================================================
(require 'org)
(use-package org
  :ensure t)

(setq org-hide-block-startup t)
(setq org-startup-indented t)
(setq org-time-stamp-formats '("<W%W %a %m/%e>" . "<W%W %a %m/%e>"))
(setq org-blank-before-new-entry nil)
(setq org-indent-indentation-per-level 4)
(setq org-hide-block-startup t)
(setq org-hide-emphasis-markers t)
(setq org-tags-column 0)  ;; Place tags right after the heading text
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (setq require-final-newline nil)))

;;=============================================================================
;; Utilities
;;=============================================================================

(defun my-org-beginning-of-heading-p ()
  (interactive)
  (and (org-at-heading-p)
       (looking-back  "^\\*+\\s-?" (line-beginning-position))))

(defun my-org-beginning-of-level1-heading-p ()
  (interactive)
  (and (org-at-heading-p)
       (looking-back  "^\\* " (line-beginning-position))))

(defun my-org-beginning-of-level2-heading-p ()
  (interactive)
  (and (org-at-heading-p)
       (looking-back  "^\\*\\* " (line-beginning-position))))

(defun my-org-level1-heading-p ()
  (interactive)
  (and (org-at-heading-p)
       (save-excursion
	 (beginning-of-line)
	 (looking-at  "^\\* "))))

(defun my-org-level2-heading-p ()
  (interactive)
  (and (org-at-heading-p)
       (save-excursion
	 (beginning-of-line)
	 (looking-at  "^\\*\\* "))))

(defun my-org-skip-bullets-forward ()
  (interactive)
  (if (and (org-at-heading-p)
	   (looking-at "^\\*+\\s-?"))
      (progn
	(skip-chars-forward "*")
	(if (looking-at " ")
	    (forward-char 1)))))

(defun my-org-skip-bullets-backward ()
  (interactive)
  (if (and (org-at-heading-p)
	   (looking-back "^\\*+\\s-?" (line-beginning-position)))
      (skip-chars-backward "* ")))

(defun my-org-delete-backward-char-bullets ()
  (interactive)
  (if (looking-back "^\\*+ " (line-beginning-position))
      (progn
	(kill-region (point) (progn (beginning-of-line) (point))))))

(defun my-org-goto-level1-heading ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^\\* " nil t))

(defun my-org-search-forward-level2-heading ()
  (interactive)
  (re-search-forward "^\\*\\* " nil t))

(defun my-org-search-backward-level2-heading ()
  (interactive)
  (let ((response (re-search-backward "^\\*\\* " nil t)))
    (progn
      (my-org-skip-bullets-forward)
      response)))

;;=============================================================================
;; Functions for smarter movement 
;;=============================================================================

;; forward-char
(defun my-org-forward-char ()
  (interactive)
  (call-interactively 'forward-char)
  (my-org-skip-bullets-forward))

;; backward-char
(defun my-org-backward-char (&optional chars)
  (interactive)
  (my-org-skip-bullets-backward)
  (call-interactively 'backward-char))

;; forward-word
(defun my-org-forward-word ()
  (interactive)
  (call-interactively 'forward-word)
  (my-org-skip-bullets-forward))

;; backward-word
(defun my-org-backward-word (&optional chars)
  (interactive)
  (my-org-skip-bullets-backward)
  (call-interactively 'backward-word))

;; delete-backward-char
(defun my-org-delete-backward-char ()
  (interactive)
  (my-org-delete-backward-char-bullets)
  (call-interactively 'org-delete-backward-char))

;; next-line
(defun my-org-next-line ()
  (interactive)
  (if (last-line-p)
      (message "Last line")
    (progn
      (if (my-org-level2-heading-p)
	  (progn 
	    (next-line 1)
	    (my-org-beginning-of-line)
	    (if (looking-at "Attendees: ")
		(forward-char (length "Attendees: "))))
	(progn
	  (next-line 1)
	  (my-org-skip-bullets-forward)
	  (if (looking-back "^\\*+")
	      (forward-char 1))
	  (if (my-org-level2-heading-p)
	      (my-org-beginning-of-line)))))))

;; previous-line
(defun my-org-previous-line ()
  (interactive)
  (if (first-line-p)
      (message "First line")
    (previous-line 1))
  (my-org-skip-bullets-forward)
  (if (looking-back "^\\*+")
      (forward-char 1))
  (if (save-excursion
	(my-org-beginning-of-line)
	(looking-at "Attendees: "))
      (progn
	(my-org-beginning-of-line)
	(forward-char (length "Attendees: ")))
    (if (my-org-level2-heading-p)
	(my-org-beginning-of-line))))

;; beginning-of-line
(defun my-org-beginning-of-line ()
  (interactive)
  (org-beginning-of-line)
  (my-org-skip-bullets-forward))

;; beginning-of-buffer
(defun my-org-beginning-of-buffer ()
  (interactive)
  (my-org-goto-level1-heading)
  (recenter 0))

;; end-of-buffer
(defun my-org-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (recenter -1)
  (my-org-search-backward-level2-heading))

;; scroll-down-hard
(defun my-scroll-down-hard ()
  (interactive)
  (let ((pos (point)))
    (if (not (pos-visible-in-window-p (point-max)))
	(scroll-down-hard))
    (if (pos-visible-in-window-p (point-max))
	(progn
	  (goto-char (point-max))
	  (recenter -1)
	  (my-org-search-backward-level2-heading)
	  (if (= pos (point))
	      (progn
		(ding)
		(message "End of buffer")))))))

;; scroll-up-hard
(defun my-scroll-up-hard ()
  (interactive)
  (let ((pos (point)))
    (if (not (pos-visible-in-window-p (point-min)))
	(scroll-up-hard))
    (if (pos-visible-in-window-p (point-min))
	(progn
	  (my-org-goto-level1-heading)
	  (if (= pos (point))
	      (progn
		(ding)
		(message "Beginning of buffer")))))))

;; kill-line
(defun my-org-kill-line ()
  (interactive)
  (if (my-org-beginning-of-heading-p)
      (progn
        (beginning-of-line)
        (kill-line 1)
	(if (last-line-p)
	    (progn
	      (previous-line 1)
	      (my-org-beginning-of-line))))
    (if (and (eolp)
	     (bolp))
	(org-delete-char 1)
      (if (eolp)
	  (progn
	    (call-interactively 'kill-line)
	    (while (looking-at "*")
	      (delete-char 1)))
	(call-interactively 'kill-line))))
  (my-org-skip-bullets-forward))

;; open-line
(defun my-org-open-line ()
  (interactive)
  (if (my-org-beginning-of-heading-p)
      (let ((bullets (buffer-substring (line-beginning-position) (point))))
        (beginning-of-line)
        (open-line 1)
        (insert bullets)
        (end-of-line))
    (call-interactively 'open-line)))

;; return
(defun my-org-return ()
  (interactive)

  ;; Check the no heading options first
  (if (not (org-at-heading-p))

      ;; In list item, insert new item
      (if (org-at-item-p)
          (call-interactively 'org-insert-item)

	;; Not in a heading, not in a list, just return
	(call-interactively 'org-return))

    ;; Level 2 heading, expand/hide subtree
    (if (or (my-org-beginning-of-level2-heading-p)
	    (my-org-beginning-of-level1-heading-p))
	(my-org-toggle-expand-collapse-heading)

      ;; Not level 2 heading, open-line
      (if (my-org-beginning-of-heading-p)
	  (my-org-open-line)

	;; End of a heading line, new heading respecting content
	(if (and (org-at-heading-p) (eolp))
	    (call-interactively 'org-insert-heading-respect-content)

	  ;; Split line creating new heading
	  (progn
	    (let ((s (buffer-substring (point) (save-excursion (end-of-line) (point)))))
	      (kill-line nil)
	      (delete-horizontal-space)
	      (call-interactively 'org-insert-heading-respect-content)
	      (insert s)
	      (my-org-beginning-of-line)))
	  )))))

;; ctrl-return
(defun my-org-control-return ()
  (interactive)
  
  ;; Check the no heading options first
  (if (not (org-at-heading-p))
      (progn
	(call-interactively 'org-return)
	(delete-horizontal-space))
    
    ;; Level 1 or 2 heading, expand/hide subtree
    (if (or (my-org-beginning-of-level2-heading-p)
	    (my-org-beginning-of-level1-heading-p))
	(my-org-toggle-expand-collapse-heading)

      ;; Non level 1 or 2 heading, just open-line,
      ;; leaves *** in the buffer otherwise
      (if (my-org-beginning-of-heading-p)
	  (progn
	    (open-line 1)
	    (next-line 1)
	    (beginning-of-line))

	(call-interactively 'org-return)))))

;; meta-return
(defun my-org-meta-return ()
  (interactive)
  (if (my-org-beginning-of-level2-heading-p)
      (my-org-toggle-expand-collapse-heading)
    (progn
      (end-of-line)
      (call-interactively 'org-insert-heading-respect-content))))

;; next-line-level2
(defun my-org-next-line-level2-heading ()
  (interactive)
  (let ((pos (point)))
    (re-search-forward "^\\*\\* " nil t)
    (if (= pos (point))
	(message "Last heading"))))

;; previous-line-level2
(defun my-org-previous-line-level2-heading ()
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (re-search-backward "^\\*\\* " nil t)
    (my-org-beginning-of-line)
    (if (= pos (point))
	(message "First heading"))))

;; l
(defun l-key ()
  (interactive)
  (if (my-org-beginning-of-level2-heading-p)
      (recenter)
    (call-interactively 'org-self-insert-command)))

;; n
(defun n-key ()
  (interactive)
  (if (my-org-beginning-of-level2-heading-p)
      (my-org-next-line-level2)
    (call-interactively 'org-self-insert-command)))

;; p
(defun p-key ()
  (interactive)
  (if (my-org-beginning-of-level2-heading-p)
      (my-org-previous-line-level2)
    (call-interactively 'org-self-insert-command)))

;; o
(defun o-key ()
  (interactive)
  (if (my-org-beginning-of-level2-heading-p)
      (my-org-return)
    (call-interactively 'org-self-insert-command)))

;;=============================================================================
;; Functions to jump directly into narrowed meeting bound to C-c m
;;=============================================================================
(defun my-org-capture-meeting ()
  (interactive)
  (org-capture nil "m"))

(defun my-org-capture-hook (&rest _args)
  (when (and (boundp 'org-capture-current-plist)
             (string= (plist-get org-capture-current-plist :key) "m"))
    (org-capture-finalize)
    (switch-to-buffer "meetings.org")
    (goto-char (point-min))
    (re-search-forward "^\\*\\* " nil t)
    (my-org-toggle-narrow-widen)))

(advice-add 'org-capture :after (lambda (&rest args) 
                                  (apply #'my-org-capture-hook args)))

;;=============================================================================
;; Functions to expand/contract/narrow
;;=============================================================================

;; line-visible-p
(defun my-org-line-visible-p ()
  (interactive)
  (let ((pos (line-beginning-position))
        (invis nil))
    (setq invis (get-char-property pos 'invisible))
    (when (not invis)
      (dolist (ov (overlays-at pos))
        (when (overlay-get ov 'invisible)
          (setq invis t))))
    (not invis)))

;; expanded-p
(defun my-org-expanded-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (expanded)
      (while (and (not expanded)
                  (re-search-forward "^\\*\\*\\* " nil t))
	(beginning-of-line)  
        (when (my-org-line-visible-p)
          (setq expanded t))
        (end-of-line))       
      expanded)))

;; toggle-expand-collapse-buffer
(defun my-org-toggle-expand-collapse-buffer ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen))
  (if (my-org-expanded-p)
      (outline-hide-sublevels 2)
    (outline-show-all))
  (save-excursion
    (if (re-search-backward "^\\*\\* " nil t)
	(recenter 10))))

;; toggle-expand-collapse-heading
(defun my-org-toggle-expand-collapse-heading ()
  (interactive)
  (when (org-at-heading-p) 
    (if (org-fold-folded-p (line-end-position)) 
        (org-show-subtree) 
      (org-fold-hide-subtree)))) 

;; toggle-narrow-widen
(defun my-org-toggle-narrow-widen ()
  (interactive)
  (save-excursion
    (if (not (my-org-search-backward-level2-heading))
	(message "Not currently in a second level list heading.")
      (progn
	;; Collapse everything to 2 levels, and
	;; Expand the current level 2 list item subtree
	(outline-hide-sublevels 2)
	(org-show-subtree)

	;; Toggle wide/narrow
	(if (buffer-narrowed-p)
	    (progn
	      (widen)
	      (recenter 10)) ; move list 2 heading to line 10
	  (org-narrow-to-subtree))))))

;;=============================================================================
;; copy-to-slack
;;=============================================================================
(defun org-copy-to-slack ()
  (interactive)
  (save-excursion
    ;; Ensure we are on a heading
    (unless (org-at-heading-p)
      (error "Not at an Org heading"))
    ;; Go to the beginning of the current heading
    (org-back-to-heading t)
    (let ((beg (point)))  ;; Start of the subtree
      ;; Move to the end of the subtree
      (org-end-of-subtree t t)
      (let ((end (point)))  ;; End of the subtree
	;; Extract the raw text of the subtree
	(let* ((raw-text (buffer-substring-no-properties beg end))
	       (lines (split-string raw-text "\n"))
	       (ast-regex "^\\*+")
	       ;; Determine how many asterisks are on the first line
	       (first-line (car lines))
	       (first-match-len (if (string-match ast-regex first-line)
				    (length (match-string 0 first-line))
				  0))
	       ;; Calculate the shift to make the first line have 1 asterisk
	       (shift (max 0 (1- first-match-len)))
	       (converted-lines
		(mapcar
		 (lambda (line)
		   (if (string-match ast-regex line)
		       (let* ((stars (match-string 0 line))
			      (star-len (length stars))
			      (new-as (max 1 (- star-len shift))) ; Normalize asterisks
			      (rest (replace-regexp-in-string "^\\*+ *" "" line))
			      (num-initial (max 0 (1- new-as))) ; Indent by level
			      (indent-string (make-string (* 4 num-initial) ?\ ))
			      (dash "- "))
			 (concat indent-string dash rest))
		     ;; If no leading asterisks, keep line unchanged
		     line))
		 lines))
	       (converted-text (string-join converted-lines "\n")))
	  ;; Copy the transformed text to the clipboard
	  (kill-new converted-text)
	  (message "Converted %d lines, copied to clipboard for Slack." (length lines)))))))

;;=============================================================================
;; Capture Templates
;;=============================================================================
(setq org-capture-templates
      '(;; 1. Meeting Notes
        ("m" "Meeting" entry
         (file+headline "~/doc/meetings.org" "Meetings")
         "* %? %U\n** Attendees: \n** Discussion\n*** \n"
         ;; "* %? %(my-org-date)\n** Attendees: \n** Discussion\n*** \n"
         :prepend t)))

(setq org-agenda-files '("~/doc/meetings.org"
                         "~/doc/followups.org"
                         "~/doc/todos.org"
                         "~/doc/projects.org"))

;;=============================================================================
;; Custom Faces
;;=============================================================================
(custom-set-faces
 '(org-level-1 ((t (:height 1.05 :foreground "systemOrangeColor"))))
 '(org-level-2 ((t (:height 1.05 :foreground "systemOrangeColor"))))
 '(org-level-3 ((t (:foreground "LightGreen"))))
 '(org-level-4 ((t (:foreground "LightGoldenrod2"))))
 '(org-level-5 ((t (:foreground "LightCyan1"))))
 '(org-level-6 ((t (:foreground "LightCyan1"))))
 '(org-level-7 ((t (:foreground "LightCyan1"))))
 '(org-level-8 ((t (:foreground "LightCyan1"))))
 '(org-level-9 ((t (:foreground "LightCyan1")))))

;;=============================================================================
;; Org Modern package
;;=============================================================================
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star "•"
	org-modern-table nil))

;;=============================================================================
;; TODO Management
;;=============================================================================
(setq org-agenda-sorting-strategy
      '((todo priority-down name-up)))

(defun my-set-priority-1 () (interactive) (my-org-agenda-set-priority ?A))
(defun my-set-priority-2 () (interactive) (my-org-agenda-set-priority ?B))
(defun my-set-priority-3 () (interactive) (my-org-agenda-set-priority ?C))
(defun my-set-priority-4 () (interactive) (my-org-agenda-set-priority ?D))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "1") #'my-set-priority-1)
  (define-key org-agenda-mode-map (kbd "2") #'my-set-priority-2)
  (define-key org-agenda-mode-map (kbd "3") #'my-set-priority-3)
  (define-key org-agenda-mode-map (kbd "4") #'my-set-priority-4))

(defun my-org-agenda-set-priority (priority-char)
  "Set the priority of the current Org Agenda item to PRIORITY-CHAR."
  (interactive "cSet priority to (A-D): ")
  (org-agenda-with-point-at (point)
    (org-priority priority-char)))

;;=============================================================================
;; Key bindings
;;=============================================================================
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-f") #'my-org-forward-char)
  (define-key org-mode-map (kbd "<right>") #'my-org-forward-char)

  (define-key org-mode-map (kbd "M-b") #'my-org-backward-char)
  (define-key org-mode-map (kbd "<left>") #'my-org-backward-char)

  (define-key org-mode-map (kbd "C-f") #'my-org-forward-word)
  (define-key org-mode-map (kbd "C-<right>") #'my-org-forward-word)

  (define-key org-mode-map (kbd "C-b") #'my-org-backward-word)
  (define-key org-mode-map (kbd "C-<left>") #'my-org-backward-word)

  (define-key org-mode-map (kbd "<backspace>") #'my-org-delete-backward-char)

  (define-key org-mode-map (kbd "C-n") #'my-org-next-line)
  (define-key org-mode-map (kbd "<down>") #'my-org-next-line)

  (define-key org-mode-map (kbd "C-p") #'my-org-previous-line)
  (define-key org-mode-map (kbd "<up>") #'my-org-previous-line)

  (define-key org-mode-map (kbd "M-n") #'my-org-next-line-level2-heading)
  (define-key org-mode-map (kbd "M-p") #'my-org-previous-line-level2-heading)

  (define-key org-mode-map (kbd "C-a") #'my-org-beginning-of-line)

  (define-key org-mode-map (kbd "C-v") #'my-scroll-down-hard)
  (define-key org-mode-map (kbd "<next>") #'my-scroll-down-hard)

  (define-key org-mode-map (kbd "M-v") #'my-scroll-up-hard)
  (define-key org-mode-map (kbd "<prior>") #'my-scroll-up-hard)

  (define-key org-mode-map (kbd "M-<") #'my-org-beginning-of-buffer)
  (define-key org-mode-map (kbd "C-<up>") #'my-org-beginning-of-buffer)
  (define-key org-mode-map (kbd "<home>") #'my-org-beginning-of-buffer)

  (define-key org-mode-map (kbd "M->") #'my-org-end-of-buffer)
  (define-key org-mode-map (kbd "C-<down>") #'my-org-end-of-buffer)
  (define-key org-mode-map (kbd "<end>") #'my-org-end-of-buffer)

  (define-key org-mode-map (kbd "TAB") #'org-metaright)
  (define-key org-mode-map (kbd "<backtab>") #'org-metaleft)

  (define-key org-mode-map (kbd "M-<right>") #'org-demote-subtree)
  (define-key org-mode-map (kbd "C-<tab>") #'org-demote-subtree)

  (define-key org-mode-map (kbd "M-<left>") #'org-promote-subtree)
  (define-key org-mode-map (kbd "S-C-<tab>") #'org-promote-subtree)

  (define-key org-mode-map (kbd "<return>") #'my-org-return)
  (define-key org-mode-map (kbd "C-<return>") #'my-org-control-return)
  (define-key org-mode-map (kbd "M-<return>") #'my-org-meta-return)

  (define-key org-mode-map (kbd "C-o") #'my-org-open-line)
  (define-key org-mode-map (kbd "C-k") #'my-org-kill-line)

  (define-key org-mode-map (kbd "C-c s") #'org-copy-to-slack)

  (define-key org-mode-map (kbd "l") #'l-key)
  (define-key org-mode-map (kbd "n") #'n-key)
  (define-key org-mode-map (kbd "p") #'p-key)
  (define-key org-mode-map (kbd "o") #'o-key)

  (define-key org-mode-map (kbd "C-.") #'my-org-toggle-narrow-widen)
  (define-key org-mode-map (kbd "M-.") #'my-org-toggle-narrow-widen)
  (define-key org-mode-map (kbd "C-,") #'my-org-toggle-expand-collapse-buffer)
  (define-key org-mode-map (kbd "M-,") #'my-org-toggle-expand-collapse-buffer))

(global-set-key (kbd "C-c m") #'my-org-capture-meeting)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c t") 
  (lambda () 
    (interactive) 
    (org-agenda nil "t")))
