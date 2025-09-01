;;=============================================================================
;; Visual settings
;;=============================================================================
(set-face-attribute 'default nil :family "Monaco" :height 200)
(blink-cursor-mode 'toggle)
(set-face-foreground 'region "yellow")
(set-face-background 'region "lightslateblue")
(global-display-line-numbers-mode)
(tool-bar-mode -1)


;;=============================================================================
;; Turn off auto save/backup features
;;=============================================================================
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(add-hook 'prog-mode-hook (lambda () (setq make-backup-files nil)))
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)


;;=============================================================================
;; Change search and abbrev expand to be case insensitive
;;=============================================================================
;; Emacs uses a “smart case” approach where the presence of uppercase
;; letters in the search string makes the search case-sensitive, and
;; an all-lowercase search is case-insensitive. To completely disable
;; this behavior and enforce always case-sensitive searches, ensure
;; that no other configurations override case-fold-search
(setq-default case-fold-search nil)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (setq case-fold-search nil)))
(setq dabbrev-case-fold-search nil)


;;=============================================================================
;; Uses spaces instead of tabs and cleanup whitespace upon save
;;=============================================================================
(setq-default indent-tabs-mode nil)

(defun my-untabify-buffer ()
  "Untabify the entire buffer, replacing all tabs with spaces."
  (untabify (point-min) (point-max)))

(defun my-cleanup-buffer-before-save ()
  "Untabify the buffer and delete trailing whitespace before saving."
  (my-untabify-buffer)
  (delete-trailing-whitespace))

(add-hook 'before-save-hook #'my-cleanup-buffer-before-save)


;;=============================================================================
;; Miscellaneous configuration
;;=============================================================================
(define-key minibuffer-local-completion-map (kbd "SPC") #'minibuffer-complete-word) ;; Bind SPC to `minibuffer-complete-word` in generic completions
(define-key minibuffer-local-filename-completion-map (kbd "SPC") #'minibuffer-complete-word) ;; Bind SPC to `minibuffer-complete-word` in filename completions
(setq-default truncate-lines t)
(set-variable 'scroll-step 1)
(setq scroll-conservatively 101)
(setq  mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))
(line-number-mode t)
(scroll-bar-mode -1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-short-answers t) ;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(setq frame-resize-pixelwise t)
(blink-cursor-mode 0)
(setq x-colors (ns-list-colors))
(column-number-mode)
(setq gc-cons-threshold 100000000)  ;; 100 MB
(setq gc-cons-percentage 0.1)       ;; 10%
;; (advice-add 'scroll-up-command :override #'scroll-down-hard)
(set-variable 'require-final-newline nil)
(setq delete-trailing-lines t)

;; (set-variable 'track-eol t)
;; (transient-mark-mode t)
;; (display-time)
;; (column-number-mode t)


;;=============================================================================
;; Open emacs sized to occupy the left half the screen
;;=============================================================================
(setq initial-frame-alist
      (let* ((screen-width (display-pixel-width))
             (screen-height (display-pixel-height))
             (frame-width (floor (* 0.5 (/ screen-width (frame-char-width)))))
             (frame-height (floor (/ screen-height (frame-char-height)))))
        `((top . 0)
          (left . 0)
          (width . ,frame-width)
          (height . ,frame-height))))


;;=============================================================================
;; ISpell Configuration (brew install aspell) (used by markdown-mode)
;;=============================================================================
(add-to-list 'exec-path "/opt/homebrew/bin")  ;; or wherever brew installed the binary
(setq ispell-program-name "aspell")


;;=============================================================================
;; Define a Function to Recenter After isearch
;;=============================================================================
(defun my-isearch-recenter ()
  "Recenter the window on the search hit if it's not visible."
  (unless (pos-visible-in-window-p (point))
    (recenter nil 1)))

(unless (member #'my-isearch-recenter isearch-update-post-hook)
  (add-hook 'isearch-update-post-hook #'my-isearch-recenter))


;;=============================================================================
;; Turn off features that can be annoying if accidentally used
;;=============================================================================
(global-unset-key (kbd "C-z"))
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
