;; ;;-----------------------------------------------------------------------------
;; ;; Treemacs Configuration
;; ;;-----------------------------------------------------------------------------
;; (use-package treemacs
;;   :ensure t
;;   ;; :hook (after-init . treemacs)  ;; Automatically start Treemacs after Emacs initializes
;;   :config
;;   ;; Customize Treemacs behavior
;;   (setq treemacs-single-click-expand-action
;;         'treemacs-visit-node-in-most-recently-used-window)
;;   (setq treemacs-icon-size 16)                ;; Set icon size to 16
;;   (treemacs-resize-icons 16)                   ;; Apply the size
;;   (setq treemacs-width 40)                     ;; Set Treemacs window width
;;   (setq treemacs-follow-mode t)                ;; Enable follow mode
;;   (setq treemacs-git-mode 'extended)           ;; Enhanced Git integration
;;   (setq treemacs-is-never-other-window t))     ;; Treemacs is never the other window

;; (use-package doom-modeline
;;   :init (doom-modeline-mode)
;;   :custom
;;   (setq doom-modeline-buffer-file-name-style 'file-name-quote
;;         doom-modeline-buffer-encoding nil
;;         doom-modeline-icon t
;;         doom-modeline-modal-icon nil
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-buffer-state-icon t
;;         doom-modeline-height 20
;;         doom-modeline-buffer-modified :foreground green
;;         doom-modeline-bar-width 3))

;; ;; Optional: Install All-the-Icons for Doom Modeline
;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :config
;;   (unless (member "all-the-icons" (font-family-list))
;;     (all-the-icons-install-fonts t)))

;; (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))

;; (global-set-key (kbd "C-c f") 'treemacs)


;; ;;-----------------------------------------------------------------------------
;; ;; Window Divider Configuration for Enhanced Window Separation
;; ;;-----------------------------------------------------------------------------
;; (setq window-divider-default-right-width 1)
;; (setq window-divider-default-bottom-width 1)

;; ;; Enable window-divider-mode
;; (window-divider-mode 1)


;; ;;=============================================================================
;; ;; TypeScript Mode Configuration
;; ;;=============================================================================
;; (use-package typescript-mode
;;   :mode ("\\.ts\\'" "\\.tsx\\'")
;;   :hook (typescript-mode . lsp-deferred) ; Integrate with lsp-mode for IDE features
;;   :bind (:map typescript-mode-map
;;               ("C-c C-c" . lsp-format-buffer) ; Format buffer with lsp
;;               ("C-c C-r" . lsp-rename))      ; Rename symbol with lsp
;;   :config
;;   ;; Set indentation level
;;   (setq typescript-indent-level 4)

;;   ;; Enable TypeScript-specific features
;;   (setq typescript-indent-level 4)
;;   (setq typescript-always-compile t)

;;   ;; Optional: Customize company-mode backends for TypeScript
;;   (setq company-backends '((company-capf company-files company-dabbrev-code))))

;; (use-package flycheck
;;   :hook (typescript-mode . flycheck-mode))


;; ;;=============================================================================
;; ;; Swift Mode Configuration
;; ;;=============================================================================
;; (use-package swift-mode
;;   :ensure t
;;   :mode ("\\.swift\\'" "\\.h\\'" "\\.m\\'")
;;   :interpreter ("swift" . swift-mode)
;;   :config
;;   ;; Optional: Customize indentation
;;   (setq swift-indent-offset 4)

;;   ;; Optional: Enable auto-format on save
;;   (add-hook 'swift-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook 'swift-format-buffer nil t))))


;; npm install -g markdown-preview
(use-package markdown-preview-mode
  :bind (("C-c p p" . markdown-preview-mode))) ; Press C-c p p to toggle preview


;; ;;=============================================================================
;; ;; Web Mode Configuration
;; ;;=============================================================================
;; (use-package web-mode
;;   :mode ("\\.html?\\'" "\\.jsx?\\'" "\\.vue\\'" "\\.php\\'" "\\.erb\\'")
;;   :hook ((web-mode . lsp-deferred)) ; Integrate with lsp-mode for IDE features
;;   :bind (:map web-mode-map
;;               ("C-c C-f" . web-mode-tag-close)        ; Close current tag
;;               ("C-c C-i" . web-mode-mark-and-indent)) ; Mark and indent region
;;   :config
;;   ;; Set indentation levels
;;   (setq web-mode-markup-indent-offset 2) ; HTML
;;   (setq web-mode-css-indent-offset 2)    ; CSS
;;   (setq web-mode-code-indent-offset 2)   ; JavaScript, PHP, etc.

;;   ;; Enable auto-pairing of brackets and quotes
;;   (setq web-mode-enable-auto-pairing t)

;;   ;; Enable auto-closing of tags
;;   (setq web-mode-enable-auto-closing t)

;;   ;; Highlight matching pairs
;;   (setq web-mode-enable-current-element-highlight t)

;;   ;; Customize color themes if necessary
;;   ;; (add-hook 'web-mode-hook (lambda () (setq web-mode-style-padding 0)))

;;   ;; Optional: Integrate with prettier for code formatting
;;   (setq web-mode-enable-auto-quoting nil)) ;; Disable auto-quoting to prevent conflicts

;; ;; Emmet Mode is an Emacs minor mode that provides a powerful toolkit
;; ;; for writing HTML, CSS, and other markup languages
;; ;; efficiently. Inspired by the Emmet (formerly Zen Coding) plugin
;; ;; originally developed for other editors like Sublime Text, Visual
;; ;; Studio Code, and Atom, Emmet Mode brings the same rapid coding
;; ;; capabilities to Emacs users.
;; (use-package emmet-mode
;;   :hook ((web-mode . emmet-mode)
;;          (css-mode . emmet-mode))
;;   :config
;;   ;; Optional: Customize emmet settings
;;   (setq emmet-indentation 2))



;; ;;=============================================================================
;; ;; Auctex Configuration (Latex/Tex documents)
;; ;;=============================================================================
;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . LaTeX-math-mode)
;;   :config
;;   ;; Enable PDF mode by default
;;   (setq TeX-PDF-mode t)

;;   ;; Set up TeX-master to avoid prompts
;;   (setq-default TeX-master nil)

;;   ;; Enable RefTeX integration
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   (setq reftex-plug-into-AUCTeX t)

;;   ;; Configure TeX-view-program-selection
;;   (setq TeX-view-program-selection
;;         '((output-pdf "PDF Tools")
;;           (output-dvi "dvi2pdf")))

;;   ;; Define PDF Tools as the default viewer
;;   (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-mode")))

;;   ;; Additional AUCTeX settings
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)

;;   ;; Enable automatic compilation on save
;;   (setq TeX-save-query nil)
;;   (setq TeX-command-default "LaTeX")
;;   (setq TeX-source-correlate-start-server t)

;;   ;; Enable RefTeX parsing of citations, labels, etc.
;;   (setq reftex-cite-format 'natbib))


;; ;;=============================================================================
;; ;; ChatGPT Shell Configuration
;; ;;=============================================================================
;; (use-package chatgpt-shell
;;   :defer t
;;   :custom
;;   (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))  ;; Use environment variable
;;   (chatgpt-shell-model-version "gpt-3.5-turbo")   ;; The model to use: e.g., "gpt-3.5-turbo" or "gpt-4" (if you have access)
;;   (chatgpt-shell-system-prompt "You are ChatGPT, a large language model trained by OpenAI.  Be concise by default.  Assume I'm on MacOS using GNU Emacs and sometimes Visual Studio Code.")   ;; A system prompt: sets the context or tone for the conversation
;;   (chatgpt-shell-history-dir "~/.emacs.d/chatgpt-shell-history")   ;; Directory for storing conversation histories
;;   (chatgpt-shell-history-file "chatgpt-history")   ;; Filename for storing conversation history in the above directory
;;   (chatgpt-shell-streaming t)
;;   (chatgpt-shell-max-tokens 1024))   ;; Maximum tokens requested per response


;;=============================================================================
;; Prettier
;;=============================================================================
;; npm install -g prettier
(use-package prettier-js
  :ensure t
  :hook (
	 ;; (web-mode . prettier-js-mode)
         ;; (typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))


;; ;;=============================================================================
;; ;; Avy Configuration - Jump to characters in the visual window
;; ;;=============================================================================
;; (use-package avy
;;   :defer t
;;   :bind
;;   ;; Bind Avy commands to convenient keys
;;   (("C-;"   . avy-goto-char)       ;; Jump to a specific character
;;    ("C-'"   . avy-goto-char-2)     ;; Jump to a two-character sequence
;;    ("C-M-;" . avy-goto-word-1)     ;; Jump to the start of a word
;;    ("C-M-'" . avy-goto-word-0)     ;; Jump to the end of a word
;;    ("C-M-:" . avy-goto-line))       ;; Jump to a specific line
;;   :config
;;   ;; Customize Avy settings if desired
;;   (setq avy-background t)            ;; Dim non-target areas
;;   (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))) ;; Define the set of keys used for navigation


;;=============================================================================
;; Orderless Configuration - Fuzzy Matching
;;=============================================================================
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  ;; For file completions, we want partial-completion (which respects space)
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;=============================================================================
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


;; ;;=============================================================================
;; ;; Unfill Configuration
;; ;;=============================================================================
;; (use-package unfill :defer t)


;; ;;=============================================================================
;; ;; Reveal In OSX Finder Configuration
;; ;;=============================================================================
;; (use-package reveal-in-osx-finder :commands (reveal-in-osx-finder))


;; ;;=============================================================================
;; ;; Hide-Mode-Line Configuration
;; ;;=============================================================================
;; (use-package hide-mode-line)
;; (global-set-key (kbd "C-c m") 'hide-mode-line-mode)




;; (use-package deft
;;   :ensure t
;;   :after org
;;   :config
;;   (setq deft-directory "~/org") ; Changed from ~/org/roam to ~/org
;;   (setq deft-extensions '("org" "txt"))
;;   (setq deft-recursive t)
;;   (setq deft-use-filename-as-title t)
;;   (global-set-key (kbd "C-c d") 'deft))
