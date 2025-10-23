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
(global-set-key (kbd "C-l") 'recenter)
(global-set-key (kbd "C-k") 'my-kill-line)
(global-set-key (kbd "C-w") 'copy-region-as-kill)
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
(global-set-key (kbd "<f11>") 'eval-defun)
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
(global-set-key (kbd "M-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "M-s") 'tags-search)
(global-set-key (kbd "M-i") 'dabbrev-expand)
;; (global-set-key (kbd "M-%") 'query-replace-regexp)

;; Ctrl-c prefixed key binding additions
(global-set-key (kbd "C-c 1") 'fundamental-mode)
(global-set-key (kbd "C-c 2") 'indented-text-mode)
(global-set-key (kbd "C-c 3") 'org-mode)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'goto-line)
;; (global-set-key (kbd "C-c f") 'treemacs)
;; (global-set-key (kbd "C-c m") 'hide-mode-line-mode)
;; (global-set-key (kbd "C-c w") 'writeroom-mode)
(global-set-key (kbd "C-c f") 'toggle-case-fold-search)

;; Ctrl-c Ctrl prefixed key binding additions
(global-set-key (kbd "C-c C-w") 'copy-region-as-kill)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-c C-u") 'uncomment-region)
;; (global-set-key (kbd "C-c C-a") 'add-integer-in-buffer)
;; (global-set-key (kbd "C-c C-s") 'subtract-integer-in-buffer)
;; (global-set-key "\C-c\C-t" 'toggle-tab-width)
;; (global-set-key [8388652] 'tags-loop-continue)

;; Ctrl-x prefixed key binding additions
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-x /") 'point-to-register)

;; Ctrl-x Ctrl prefixed key binding additions
(global-set-key (kbd "C-x C-k") 'bury-buffer)
(global-set-key "\C-x\C-i" 'indent-buffer)
;; (global-set-key "\C-x\C-c" 'kill-emacs)
