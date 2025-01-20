;;=============================================================================
;; Package manager
;;=============================================================================
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;=============================================================================
;; Load my basic extensions and configuration
;;=============================================================================
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/keys.el")
(load "~/.emacs.d/vars.el")
(load "~/.emacs.d/org.el")


;;=============================================================================
;; Doom Configuraiton
;;=============================================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d")
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-shane t)  ;; Replace with your chosen Doom theme
  (global-hl-line-mode +1))


;; ;;=============================================================================
;; ;; Python Mode Configuration
;; ;;=============================================================================
;; (setq python-indent-offset 4)
;; (use-package python-mode
;;   :defer t)


;; ;;=============================================================================
;; ;; Load yaml mode
;; ;;=============================================================================
;; (use-package yaml-mode
;;   :ensure t
;;   :mode ("\\.yml\\'" "\\.yaml\\'" "\\.template\\'")
;;   :hook (yaml-mode . (lambda ()
;;                        (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; ;;=============================================================================
;; ;; ISpell Configuration (brew install aspell) (used by markdown-mode)
;; ;;=============================================================================
;; (add-to-list 'exec-path "/opt/homebrew/bin")  ;; or wherever brew installed the binary
;; (setq ispell-program-name "aspell")


;; ;;=============================================================================
;; ;; Markdown Mode Configuration
;; ;;=============================================================================
;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'"       . markdown-mode))
;;   :init
;;   ;; If you want to interpret YAML front matter in your .md
;;   (add-hook 'markdown-mode-hook 'flyspell-mode)
;;   (setq markdown-enable-math t)
;;   (setq markdown-command "pandoc --from markdown --to html5 --mathjax")) ;; brew install pandoc


;; ;;=============================================================================
;; ;; Text-Mode Configuration
;; ;;=============================================================================
;; (defun my-text-mode-setup ()
;;   "Configure settings specific to `text-mode`."
;;   (auto-fill-mode 1)                ;; Enable auto-fill-mode explicitly
;;   (setq-local fill-column 77))      ;; Set fill-column to 77 in the buffer

;; (add-hook 'text-mode-hook #'my-text-mode-setup)



;;=============================================================================
;; Vertico Configuration
;;=============================================================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "SPC") nil)
  (define-key vertico-map (kbd "SPC") #'minibuffer-complete-word))


;; ;;=============================================================================
;; ;; Embark Configuraiton
;; ;;=============================================================================
;; (use-package embark
;;   :ensure t
;;   ;; Optionally bind a key for embark-act globally, e.g.,:
;;   :bind
;;   (("C-." . embark-act)
;;    ("C-;" . embark-dwim)))

;; (use-package embark-consult
;;   :ensure t
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


;; ;;=============================================================================
;; ;; Marginalia Configuration (additional file/buffer annotations)
;; ;;=============================================================================
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode)
;;   ;; Optional settings:
;;   :custom
;;   (marginalia-max-relative-age 0)  ; Show dates for all files, not just recent ones
;;   (marginalia-multiline nil)       ; Use single line annotations
;;   (setq marginalia-annotators '(marginalia-annotators-heavy))  ; use heavy annotators
;;   ;; (marginalia-align 'right)        ; Align annotations to the right side
;;   )


;; ;;=============================================================================
;; ;; Writeroom Mode Configuration
;; ;;=============================================================================
;; (use-package writeroom-mode
;;   :ensure t
;;   :commands (writeroom-mode)
;;   :init
;;   (setq writeroom-width 120)
;;   (setq writeroom-fullscreen-effect 'maximized)
;;   :config
;;   (global-set-key (kbd "C-c w") 'writeroom-mode))


;; ;;=============================================================================
;; ;; Which Key Configuration
;; ;;=============================================================================
;; (use-package which-key
;;   :ensure t
;;   :init
;;   (setq which-key-idle-delay 2              ; Time before popup shows up
;;         which-key-max-description-length 25 ; Max length of descriptions
;;         which-key-separator " → "           ; Separator between keys
;;         which-key-prefix-prefix "+ ")       ; Prefix for prefix keys
;;   :config
;;   (which-key-mode)                          ; Enable which-key
;;   (which-key-setup-side-window-bottom)      ; Position the popup at the bottom
;;   ;; Optional: Customize the appearance
;;   (setq which-key-popup-type 'side-window)  ; 'frame, 'minibuffer, 'side-window, etc.
;;   (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
;;     "C-c C-c" "Compile current buffer"
;;     "C-c C-k" "Kill Compilation"))


;; ;;=============================================================================
;; ;; Uses spaces instead of tabs and cleanup whitespace upon save
;; ;;=============================================================================
;; (setq-default indent-tabs-mode nil)

;; (defun my-untabify-buffer ()
;;   "Untabify the entire buffer, replacing all tabs with spaces."
;;   (untabify (point-min) (point-max)))

;; (defun my-cleanup-buffer-before-save ()
;;   "Untabify the buffer and delete trailing whitespace before saving."
;;   (my-untabify-buffer)
;;   (delete-trailing-whitespace))

;; (add-hook 'before-save-hook #'my-cleanup-buffer-before-save)

;; (setq-default show-trailing-whitespace t)


;; ;;=============================================================================
;; ;; Define a Function to Recenter After isearch
;; ;;=============================================================================
;; (defun my-isearch-recenter ()
;;   "Recenter the window on the search hit if it's not visible."
;;   (unless (pos-visible-in-window-p (point))
;;     (recenter nil 1)))

;; (unless (member #'my-isearch-recenter isearch-update-post-hook)
;;   (add-hook 'isearch-update-post-hook #'my-isearch-recenter))


;; ;;=============================================================================
;; ;; Set custom registers to commonly used files
;; ;;=============================================================================
;; (set-register ?x (cons 'file "~/src/mathgen/view.js"))
;; (setq register-preview-delay 0) ;; Show registers ASAP


;; ;;=============================================================================
;; ;; Start the server enabling emacsclient to open files
;; ;;=============================================================================
;; (server-start)


;; ;;=============================================================================
;; ;; Automatically create buffers for everything in initial-buffers.txt
;; ;;=============================================================================
;; (defvar my-find-files-loaded nil
;;   "Indicates whether `find-files` has been executed.")
;; (unless my-find-files-loaded
;;   (find-files)                  ;; Replace with your actual function call
;;   (setq my-find-files-loaded t)) ;; Set the flag to indicate execution


;; ;;=============================================================================
;; ;; Remove scratch buffer
;; ;;=============================================================================
;; (if (get-buffer "*scratch*")
;;     (kill-buffer "*scratch*"))
;; (if (get-buffer "*Messages*")
;;     (kill-buffer "*Messages*"))


;; (message "Welcome!")
