;;-----------------------------------------------------------------------------
;; Package manager
;;-----------------------------------------------------------------------------
(require 'package)
(setq python-indent-offset 4)
(setq vc-follow-symlinks t)
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

;;-----------------------------------------------------------------------------
;; Define a Function to Recenter After isearch
;;-----------------------------------------------------------------------------
(defun my-isearch-recenter ()
  "Recenter the window on the search hit if it's not visible."
  (unless (pos-visible-in-window-p (point))
    (recenter nil 1)))
(unless (member #'my-isearch-recenter isearch-update-post-hook)
  (add-hook 'isearch-update-post-hook #'my-isearch-recenter))

;;-----------------------------------------------------------------------------
;; Doom-Modeline, All-The-Icons, Doom-Themes Configuration
;;-----------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d")

;;-----------------------------------------------------------------------------
;; Custom Face Definitions for Active and Inactive Buffers
;;-----------------------------------------------------------------------------
(defface my-active-buffer-face
  '((t (:background "#212121")))  ;; Active buffer background color
  "Face for the active buffer.")

;; Define a custom face for inactive buffers
(defface my-inactive-buffer-face
  ;; '((t (:background "#2D2D44")))  ;; Inactive buffer background color
  '((t (:background "#2D2D44")))  ;; Inactive buffer background color
  "Face for inactive buffers.")

(require 'cl-lib)

(defun my-update-buffer-faces ()
  "Apply `my-active-buffer-face` to active buffers and `my-inactive-buffer-face` to inactive buffers.
A buffer is considered active if it's displayed in the selected window.
Handles multiple windows displaying the same buffer by applying the active face
if any window displaying the buffer is selected."
  (let ((selected-window (selected-window))
        (buffers-in-windows
         (cl-remove-duplicates
          (mapcar #'window-buffer (window-list (selected-frame)))
          :test #'eq)))
    (dolist (buf buffers-in-windows)
      (with-current-buffer buf
        ;; Reset existing face remaps for 'default' to prevent accumulation
        (face-remap-reset-base 'default)
        ;; Determine if any window displaying this buffer is selected
        (if (cl-some (lambda (win)
                      (and (eq (window-buffer win) buf)
                           (eq win selected-window)))
                    (window-list (selected-frame)))
            ;; Apply active buffer face
            (face-remap-add-relative 'default 'my-active-buffer-face)
          ;; Apply inactive buffer face
          (face-remap-add-relative 'default 'my-inactive-buffer-face))))))

(remove-hook 'window-configuration-change-hook #'my-update-buffer-faces)
(remove-hook 'buffer-list-update-hook #'my-update-buffer-faces)
(defun my-add-hook-unless-present (hook function)
  "Add FUNCTION to HOOK only if it's not already present."
  (unless (memq function (symbol-value hook))
    (add-hook hook function)))
(my-add-hook-unless-present 'window-configuration-change-hook #'my-update-buffer-faces)
(my-add-hook-unless-present 'buffer-list-update-hook #'my-update-buffer-faces)
(add-hook 'window-configuration-change-hook #'my-update-buffer-faces)
(add-hook 'buffer-list-update-hook #'my-update-buffer-faces)

;;-----------------------------------------------------------------------------
;; Theme Configuration (Ensure Theme is Loaded Before Applying Faces)
;;-----------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-shane t)  ;; Replace with your chosen Doom theme
  (my-update-buffer-faces)   ;; Apply custom buffer faces after the theme is loaded
)

;;-----------------------------------------------------------------------------
;; 5. Initial Application of Buffer Faces
;;-----------------------------------------------------------------------------
;(my-update-buffer-faces)

;; Enable global hl-line mode
(global-hl-line-mode +1)

;;-----------------------------------------------------------------------------
;; Treemacs Configuration
;;-----------------------------------------------------------------------------
(use-package treemacs
  :ensure t
  ;; :hook (after-init . treemacs)  ;; Automatically start Treemacs after Emacs initializes
  :config
  ;; Customize Treemacs behavior
  (setq treemacs-single-click-expand-action
        'treemacs-visit-node-in-most-recently-used-window)
  (setq treemacs-icon-size 16)                ;; Set icon size to 16
  (treemacs-resize-icons 16)                   ;; Apply the size
  (setq treemacs-width 40)                     ;; Set Treemacs window width
  (setq treemacs-follow-mode t)                ;; Enable follow mode
  (setq treemacs-git-mode 'extended)           ;; Enhanced Git integration
  (setq treemacs-is-never-other-window t))     ;; Treemacs is never the other window

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (setq doom-modeline-buffer-file-name-style 'file-name-quote
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-height 20
        doom-modeline-buffer-modified :foreground green
        doom-modeline-bar-width 3))

;; Optional: Install All-the-Icons for Doom Modeline
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))



;;-----------------------------------------------------------------------------
;; Load my custom functions
;;-----------------------------------------------------------------------------
(load "~/.emacs.d/functions.el")


;;-----------------------------------------------------------------------------
;; Window Divider Configuration for Enhanced Window Separation
;;-----------------------------------------------------------------------------
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)

;; Enable window-divider-mode
(window-divider-mode 1)


;;-----------------------------------------------------------------------------
;; Python Mode Configuration
;;-----------------------------------------------------------------------------
(use-package python-mode
  :defer t)


;;-----------------------------------------------------------------------------
;; TypeScript Mode Configuration
;;-----------------------------------------------------------------------------
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-mode . lsp-deferred) ; Integrate with lsp-mode for IDE features
  :bind (:map typescript-mode-map
              ("C-c C-c" . lsp-format-buffer) ; Format buffer with lsp
              ("C-c C-r" . lsp-rename))      ; Rename symbol with lsp
  :config
  ;; Set indentation level
  (setq typescript-indent-level 4)

  ;; Enable TypeScript-specific features
  (setq typescript-indent-level 4)
  (setq typescript-always-compile t)

  ;; Optional: Customize company-mode backends for TypeScript
  (setq company-backends '((company-capf company-files company-dabbrev-code))))

(use-package flycheck
  :hook (typescript-mode . flycheck-mode))


;;-----------------------------------------------------------------------------
;; Load yaml mode
;;-----------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;;-----------------------------------------------------------------------------
;; Swift Mode Configuration
;;-----------------------------------------------------------------------------
(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" "\\.h\\'" "\\.m\\'")
  :interpreter ("swift" . swift-mode)
  :config
  ;; Optional: Customize indentation
  (setq swift-indent-offset 4)

  ;; Optional: Enable auto-format on save
  (add-hook 'swift-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'swift-format-buffer nil t))))


;;-----------------------------------------------------------------------------
;; Markdown Mode Configuration
;;-----------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :init
  ;; If you want to interpret YAML front matter in your .md
  (setq markdown-enable-math t))


;;-----------------------------------------------------------------------------
;; Markdown Preview Mode Configuration
;;-----------------------------------------------------------------------------
(use-package markdown-preview-mode
  :bind (
         ("C-c p p" . markdown-preview-mode))) ; Press C-c p p to toggle preview


;;-----------------------------------------------------------------------------
;; Web Mode Configuration
;;-----------------------------------------------------------------------------
(use-package web-mode
  :mode ("\\.html?\\'" "\\.jsx?\\'" "\\.vue\\'" "\\.php\\'" "\\.erb\\'")
  :hook ((web-mode . lsp-deferred)) ; Integrate with lsp-mode for IDE features
  :bind (:map web-mode-map
              ("C-c C-f" . web-mode-tag-close)        ; Close current tag
              ("C-c C-i" . web-mode-mark-and-indent)) ; Mark and indent region
  :config
  ;; Set indentation levels
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JavaScript, PHP, etc.

  ;; Enable auto-pairing of brackets and quotes
  (setq web-mode-enable-auto-pairing t)

  ;; Enable auto-closing of tags
  (setq web-mode-enable-auto-closing t)

  ;; Highlight matching pairs
  (setq web-mode-enable-current-element-highlight t)

  ;; Customize color themes if necessary
  ;; (add-hook 'web-mode-hook (lambda () (setq web-mode-style-padding 0)))

  ;; Optional: Integrate with prettier for code formatting
  (setq web-mode-enable-auto-quoting nil)) ;; Disable auto-quoting to prevent conflicts

;; Emmet Mode is an Emacs minor mode that provides a powerful toolkit
;; for writing HTML, CSS, and other markup languages
;; efficiently. Inspired by the Emmet (formerly Zen Coding) plugin
;; originally developed for other editors like Sublime Text, Visual
;; Studio Code, and Atom, Emmet Mode brings the same rapid coding
;; capabilities to Emacs users.
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :config
  ;; Optional: Customize emmet settings
  (setq emmet-indentation 2))

;; Optional: Integrate with prettier via add-hook for automatic formatting
(use-package prettier-js
  :hook ((web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))


;;-----------------------------------------------------------------------------
;; Auctex Configuration (Latex/Tex documents)
;;-----------------------------------------------------------------------------
(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . LaTeX-math-mode)
  :config
  ;; Enable PDF mode by default
  (setq TeX-PDF-mode t)

  ;; Set up TeX-master to avoid prompts
  (setq-default TeX-master nil)

  ;; Enable RefTeX integration
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)

  ;; Configure TeX-view-program-selection
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-dvi "dvi2pdf")))

  ;; Define PDF Tools as the default viewer
  (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-mode")))

  ;; Additional AUCTeX settings
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Enable automatic compilation on save
  (setq TeX-save-query nil)
  (setq TeX-command-default "LaTeX")
  (setq TeX-source-correlate-start-server t)

  ;; Enable RefTeX parsing of citations, labels, etc.
  (setq reftex-cite-format 'natbib))


;;-----------------------------------------------------------------------------
;; Text-Mode Configuration
;;-----------------------------------------------------------------------------
(defun my-text-mode-setup ()
  "Configure settings specific to `text-mode`."
  (auto-fill-mode 1)                ;; Enable auto-fill-mode explicitly
  (setq-local fill-column 77))      ;; Set fill-column to 77 in the buffer

(add-hook 'text-mode-hook #'my-text-mode-setup)


;;-----------------------------------------------------------------------------
;; ChatGPT Shell Configuration
;;-----------------------------------------------------------------------------
(use-package chatgpt-shell
  :defer t
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))  ;; Use environment variable
  (chatgpt-shell-model-version "gpt-3.5-turbo")   ;; The model to use: e.g., "gpt-3.5-turbo" or "gpt-4" (if you have access)
  (chatgpt-shell-system-prompt "You are ChatGPT, a large language model trained by OpenAI.  Be concise by default.  Assume I'm on MacOS using GNU Emacs and sometimes Visual Studio Code.")   ;; A system prompt: sets the context or tone for the conversation
  (chatgpt-shell-history-dir "~/.emacs.d/chatgpt-shell-history")   ;; Directory for storing conversation histories
  (chatgpt-shell-history-file "chatgpt-history")   ;; Filename for storing conversation history in the above directory
  (chatgpt-shell-streaming t)
  (chatgpt-shell-max-tokens 1024))   ;; Maximum tokens requested per response

;;-----------------------------------------------------------------------------
;; Avy Configuration - Jump to characters in the visual window
;;-----------------------------------------------------------------------------
(use-package avy
  :defer t
  :bind
  ;; Bind Avy commands to convenient keys
  (("C-;"   . avy-goto-char)       ;; Jump to a specific character
   ("C-'"   . avy-goto-char-2)     ;; Jump to a two-character sequence
   ("C-M-;" . avy-goto-word-1)     ;; Jump to the start of a word
   ("C-M-'" . avy-goto-word-0)     ;; Jump to the end of a word
   ("C-M-:" . avy-goto-line))       ;; Jump to a specific line
  :config
  ;; Customize Avy settings if desired
  (setq avy-background t)            ;; Dim non-target areas
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))) ;; Define the set of keys used for navigation


;; ;;-----------------------------------------------------------------------------
;; ;; Vertico Configuration - Minimal, Vertical Completion
;; ;;-----------------------------------------------------------------------------
;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode))

;; ;; (with-eval-after-load 'vertico
;; ;;   ;; Unbind first, just in case something else captured it
;; ;;   (define-key vertico-map (kbd "SPC") nil)
;; ;;   ;; Now bind it
;; ;;   (define-key vertico-map (kbd "SPC") #'minibuffer-complete-word))


;; ;;-----------------------------------------------------------------------------
;; ;; Orderless Configuration - Fuzzy Matching
;; ;;-----------------------------------------------------------------------------
;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless))
;;   (completion-category-defaults nil)
;;   ;; For file completions, we want partial-completion (which respects space)
;;   (completion-category-overrides '((file (styles basic partial-completion)))))


;; ;;-----------------------------------------------------------------------------
;; ;; Embark with Consult (Optional)
;; ;;-----------------------------------------------------------------------------
;; (use-package embark-consult
;;   :ensure t
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


;; ;;-----------------------------------------------------------------------------
;; ;; Marginalia Configuration - Rich Annotations
;; ;;-----------------------------------------------------------------------------
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode))


;;-----------------------------------------------------------------------------
;; Writeroom Mode Configuration
;;-----------------------------------------------------------------------------
(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :init
  ;; Set default options before writeroom-mode is activated
  (setq writeroom-width 80) ; Set text area width to 80 columns
  (setq writeroom-fullscreen-effect 'maximized) ; Maximize the frame on activation
  :config
  ;; Optional: Define a keybinding to toggle writeroom-mode
  (global-set-key (kbd "C-c w") 'writeroom-mode))


;;-----------------------------------------------------------------------------
;; Uniquify Configuration
;;
;; uniquify is a built-in Emacs library that enhances the default
;; buffer naming mechanism. Its primary purpose is to make buffer
;; names unique and more informative, especially when multiple files
;; with the same name are open simultaneously. By appending parts of
;; their directory paths to buffer names, uniquify helps users easily
;; distinguish between buffers without cluttering the buffer list.
;;
;; uniquify-buffer-name-style:
;;   forward                       bar/mumble/name    quux/mumble/name
;;   reverse                       name\mumble\bar    name\mumble\quux
;;   post-forward                  name|bar/mumble    name|quux/mumble
;;   post-forward-angle-brackets   name<bar/mumble>   name<quux/mumble>
;;   nil                           name               name<2>
;; -----------------------------------------------------------------------------
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward)


;;-----------------------------------------------------------------------------
;; Unfill Configuration
;;-----------------------------------------------------------------------------
(use-package unfill :defer t)


;;-----------------------------------------------------------------------------
;; Reveal In OSX Finder Configuration
;;-----------------------------------------------------------------------------
(use-package reveal-in-osx-finder :commands (reveal-in-osx-finder))


;;-----------------------------------------------------------------------------
;; Hide-Mode-Line Configuration
;;-----------------------------------------------------------------------------
(use-package hide-mode-line)


;;-----------------------------------------------------------------------------
;; Which Key Configuration
;;-----------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5        ; Time before popup shows up
        which-key-max-description-length 25 ; Max length of descriptions
        which-key-separator " → "         ; Separator between keys
        which-key-prefix-prefix "+ ")     ; Prefix for prefix keys
  :config
  (which-key-mode)                       ; Enable which-key
  (which-key-setup-side-window-bottom)   ; Position the popup at the bottom
  ;; Optional: Customize the appearance
  (setq which-key-popup-type 'side-window) ; 'frame, 'minibuffer, 'side-window, etc.
  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c C-c" "Compile current buffer"
    "C-c C-k" "Kill Compilation")
  )

;;-----------------------------------------------------------------------------
;; Performance settings
;;-----------------------------------------------------------------------------
(setq gc-cons-threshold 100000000)  ;; 100 MB
(setq gc-cons-percentage 0.1)       ;; 10%

;;-----------------------------------------------------------------------------
;; Emacs Meeting Notes Configuration and Package Recommendations
;;-----------------------------------------------------------------------------
;; (use-package org
;;   :ensure t
;;   :config
;;   ;; Set default org files directories
;;   (setq org-directory "~/org")
;;   (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;;   ;; Enable org-capture templates
;;   (setq org-capture-templates
;;         '(("m" "Meeting Notes" entry
;;            (file+headline "~/org/meetings.org" "Meetings")
;;            "* MEETING: %^{Meeting Title}\n:Date: %U\n:END:\n\n** Subject\n- \n\n** Attendees\n- \n\n** Notes\n- \n\n** Actions\n- [ ] ")
;;           ("t" "Todo" entry
;;            (file+headline "~/org/tasks.org" "Tasks")
;;            "* TODO %?\nEntered on %U\n")))

;;   ;; Bind org-capture to C-c n
;;   (global-set-key (kbd "C-c n") 'org-capture)

;;   ;; Enable visual-line-mode for better readability
;;   (add-hook 'org-mode-hook 'visual-line-mode))

;; (use-package org-roam
;;   :ensure t
;;   :init
;;   (setq org-roam-directory (file-truename "~/org/roam"))
;;   :hook (after-init . org-roam-mode)
;;   :config
;;   (org-roam-db-autosync-mode))

;; (use-package deft
;;   :ensure t
;;   :after org
;;   :config
;;   (setq deft-directory "~/org") ; Changed from ~/org/roam to ~/org
;;   (setq deft-extensions '("org" "txt"))
;;   (setq deft-recursive t)
;;   (setq deft-use-filename-as-title t)
;;   (global-set-key (kbd "C-c d") 'deft))

;; (use-package hl-line
;;   :ensure nil
;;   :hook (prog-mode . hl-line-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))


;;-----------------------------------------------------------------------------
;; Key bindings
;;-----------------------------------------------------------------------------
(setq mac-command-modifier 'meta  ; Map Command key to Meta
      mac-option-modifier 'none  ; Keep Option key unmapped
      ns-command-modifier 'meta   ; Map Command key to Meta for newer Emacs
      ns-option-modifier 'none)   ; Keep Option key unmapped

;; Key binding modifications to the emacs defaults
(global-set-key (kbd "M-d") 'delete-char)
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "M-b") 'backward-char)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "M-d") 'delete-char)
(global-set-key (kbd "C-v") 'scroll-down-hard)
(global-set-key (kbd "M-v") 'scroll-up-hard)
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Unprefixed function and arrow keys
(global-set-key (kbd "<f1>") 'visit-tags-table)
(global-set-key (kbd "<f2>") 'tags-search)
(global-set-key (kbd "<f3>") 'tags-query-replace)
(global-set-key (kbd "<f4>") 'unassigned)
(global-set-key (kbd "<f5>") 'unassigned)
(global-set-key (kbd "<f6>") 'unassigned)
(global-set-key (kbd "<f7>") 'unassigned)
(global-set-key (kbd "<f8>") 'unassigned)
(global-set-key (kbd "<f9>") 'recenter-top)
(global-set-key (kbd "<f10>") 'recenter-bottom)
(global-set-key (kbd "<f11>") 'unassigned)
(global-set-key (kbd "<f12>") 'eval-buffer)
(global-set-key (kbd "<left>") 'backward-char)
(global-set-key (kbd "<right>") 'forward-char)
(global-set-key (kbd "<up>") 'previous-line)
(global-set-key (kbd "<down>") 'next-line)

;; Shift prefixed function and arrow keys
(global-set-key (kbd "S-<f1>") 'unassigned)
(global-set-key (kbd "S-<f2>") 'unassigned)
(global-set-key (kbd "S-<f3>") 'unassigned)
(global-set-key (kbd "S-<f4>") 'unassigned)
(global-set-key (kbd "S-<f5>") 'unassigned)
(global-set-key (kbd "S-<f6>") 'unassigned)
(global-set-key (kbd "S-<f7>") 'unassigned)
(global-set-key (kbd "S-<f8>") 'unassigned)
(global-set-key (kbd "S-<f9>") 'unassigned)
(global-set-key (kbd "S-<f10>") 'unassigned)
(global-set-key (kbd "S-<f11>") 'unassigned)
(global-set-key (kbd "S-<f12>") 'unassigned)
(global-set-key (kbd "S-<left>") 'beginning-of-line)
(global-set-key (kbd "S-<right>") 'end-of-line)
(global-set-key (kbd "S-<up>") 'scroll-up-hard)
(global-set-key (kbd "S-<down>") 'scroll-down-hard)

;; Control prefixed function and arrow keys
(global-set-key (kbd "C-<f1>") 'unassigned)
(global-set-key (kbd "C-<f2>") 'unassigned)
(global-set-key (kbd "C-<f3>") 'unassigned)
(global-set-key (kbd "C-<f4>") 'unassigned)
(global-set-key (kbd "C-<f5>") 'unassigned)
(global-set-key (kbd "C-<f6>") 'unassigned)
(global-set-key (kbd "C-<f7>") 'unassigned)
(global-set-key (kbd "C-<f8>") 'unassigned)
(global-set-key (kbd "C-<f9>") 'unassigned)
(global-set-key (kbd "C-<f10>") 'unassigned)
(global-set-key (kbd "C-<f11>") 'unassigned)
(global-set-key (kbd "C-<f12>") 'unassigned)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<up>") 'my-beginning-of-buffer)
(global-set-key (kbd "C-<down>") 'my-end-of-buffer)

;; Escape/Command/Clover prefixed function and arrow keys
(global-set-key (kbd "M-<f1>") 'unassigned)
(global-set-key (kbd "M-<f2>") 'unassigned)
(global-set-key (kbd "M-<f3>") 'unassigned)
(global-set-key (kbd "M-<f4>") 'unassigned)
(global-set-key (kbd "M-<f5>") 'unassigned)
(global-set-key (kbd "M-<f6>") 'unassigned)
(global-set-key (kbd "M-<f7>") 'unassigned)
(global-set-key (kbd "M-<f8>") 'unassigned)
(global-set-key (kbd "M-<f9>") 'unassigned)
(global-set-key (kbd "M-<f10>") 'unassigned)
(global-set-key (kbd "M-<f11>") 'unassigned)
(global-set-key (kbd "M-<f12>") 'unassigned)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<up>") 'scroll-up-hard)
(global-set-key (kbd "M-<down>") 'scroll-down-hard)

;; Escape/Control prefixed key binding additions
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "M-s") 'tags-search)
(global-set-key (kbd "M-i") 'dabbrev-expand)
(global-set-key (kbd "M-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))
;; (global-set-key (kbd "M-%") 'query-replace-regexp)

;; Ctrl-c prefixed key binding additions
(global-set-key (kbd "C-c 1") 'fundamental-mode)
(global-set-key (kbd "C-c 2") 'indented-text-mode)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c f") 'treemacs)
(global-set-key (kbd "C-c m") 'hide-mode-line-mode)
;; (global-set-key (kbd "C-c f") 'toggle-case-fold-search)

;; Ctrl-c Ctrl prefixed key binding additions
(global-set-key (kbd "C-c C-w") 'copy-region-as-kill)
(global-set-key (kbd "C-c C-u") 'unfill-paragraph)
;; (global-set-key (kbd "C-c C-u") 'camel-up)
;; (global-set-key (kbd "C-c C-d") 'camel-down)
(global-set-key (kbd "C-c C-a") 'add-integer-in-buffer)
(global-set-key (kbd "C-c C-s") 'subtract-integer-in-buffer)
;; (global-set-key "\C-c\C-t" 'toggle-tab-width)
;; (global-set-key [8388652] 'tags-loop-continue)

;; Ctrl-x prefixed key binding additions
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-x /") 'point-to-register)

;; Ctrl-x Ctrl prefixed key binding additions
(global-set-key (kbd "C-x C-k") 'bury-buffer)
(global-set-key "\C-x\C-i" 'indent-buffer)
;; (global-set-key "\C-x\C-c" 'kill-emacs)

;;-----------------------------------------------------------------------------
;; Turn off auto save/backup features
;;-----------------------------------------------------------------------------
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(add-hook 'prog-mode-hook (lambda () (setq make-backup-files nil)))
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;;-----------------------------------------------------------------------------
;; Change search and abbrev expand to be case insensitive
;;-----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
;; Uses spaces instead of tabs and cleanup whitespace upon save
;;-----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

(defun my-untabify-buffer ()
  "Untabify the entire buffer, replacing all tabs with spaces."
  (untabify (point-min) (point-max)))

(defun my-cleanup-buffer-before-save ()
  "Untabify the buffer and delete trailing whitespace before saving."
  (interactive)
  (my-untabify-buffer)
  (delete-trailing-whitespace))

(add-hook 'before-save-hook #'my-cleanup-buffer-before-save)

(setq-default show-trailing-whitespace t)

;;-----------------------------------------------------------------------------
;; Turn off features that can be annoying if accidentally used
;;-----------------------------------------------------------------------------
(global-unset-key (kbd "C-z"))
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;-----------------------------------------------------------------------------
;; Set custom registers to commonly used files
;;-----------------------------------------------------------------------------
;; TODO
(setq register-preview-delay 0) ;; Show registers ASAP
;; (set-register ?x (cons 'file "~/src/mathgen/view.js"))

;;-----------------------------------------------------------------------------
;; Miscellaneous configuration
;;-----------------------------------------------------------------------------
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
;; (advice-add 'scroll-up-command :override #'scroll-down-hard)
;; (set-variable 'require-final-newline t)
;; (set-variable 'track-eol t)
;; (transient-mark-mode t)
;; (display-time)
;; (column-number-mode t)

;;-----------------------------------------------------------------------------
;; Open emacs sized to occupy the left half the screen
;;-----------------------------------------------------------------------------
(setq initial-frame-alist
      (let* ((screen-width (display-pixel-width))
             (screen-height (display-pixel-height))
             (frame-width (floor (* 0.5 (/ screen-width (frame-char-width)))))
             (frame-height (floor (/ screen-height (frame-char-height)))))
        `((top . 0)
          (left . 0)
          (width . ,frame-width)
          (height . ,frame-height))))

;;-----------------------------------------------------------------------------
;; Visual settings
;;-----------------------------------------------------------------------------
(set-face-attribute 'default nil :family "Monaco" :height 200)
;; (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 130) ;; Monospace for code
;; (set-face-attribute 'variable-pitch nil :family "Arial" :height 140)  ;; Proportional for text
;; (blink-cursor-mode 'toggle)
(set-face-foreground 'region "yellow")
(set-face-background 'region "lightslateblue")
(global-display-line-numbers-mode)
(tool-bar-mode -1)

;;-----------------------------------------------------------------------------
;; Start the server enabling emacsclient to open files
;;-----------------------------------------------------------------------------
(server-start)

;;-----------------------------------------------------------------------------
;; Automatically create buffers for everything in initial-buffers.txt
;;-----------------------------------------------------------------------------
(defvar my-find-files-loaded nil
  "Indicates whether `find-files` has been executed.")
(unless my-find-files-loaded
  (find-files)                  ;; Replace with your actual function call
  (setq my-find-files-loaded t)) ;; Set the flag to indicate execution

;;-----------------------------------------------------------------------------
;; Remove scratch buffer
;;-----------------------------------------------------------------------------
(if (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))
(if (get-buffer "*Messages*")
    (kill-buffer "*Messages*"))

(message "Welcome!")
