(require 'shell)
(require 'comint)

;;-----------------------------------------------------------------------------
;; comint-previuos-input
;;
;; Redefine the comint-previous-input function for the shell mode so that if
;; the cursor is somewhere other than after the last prompt in the file that
;; this command will do normal previous/next lines.  This allows the user to
;; back up one character and then treat the shell buffer as a normal buffer
;; with respect to Ctrl-n and Ctrl-p movement.
;;-----------------------------------------------------------------------------
(defun comint-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (if (not (comint-after-pmark-p))
      ;;then
      (previous-line arg)
    ;;else
    (comint-previous-matching-input "." arg)))

(defun comint-next-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (if (not (comint-after-pmark-p))
      ;;then
      (next-line arg)
    ;;else
    (comint-next-matching-input "." arg)))

;;-----------------------------------------------------------------------------
;; shell-create
;;-----------------------------------------------------------------------------
(defun shell-create (shell-number)
  (interactive "sShell Number (ie 1, 2, etc): ")
  (let ((shell-buffer-name (concat "*shell" shell-number "*"))
	(shell-buffer-name2 (concat "shell" shell-number)))
    (cond ((not (comint-check-proc shell-buffer-name))
	   (let* ((prog (or explicit-shell-file-name
			    (getenv "ESHELL")
			    (getenv "SHELL")
			    "/bin/sh"))		     
		  (name (file-name-nondirectory prog))
		  (startfile (concat "~/.emacs_" name))
		  (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	     (set-buffer (apply 'make-comint shell-buffer-name2 prog
				(if (file-exists-p startfile) startfile)
				(if (and xargs-name (boundp xargs-name))
				    (symbol-value xargs-name)
				  '("-i"))))
	     (shell-mode))))
    (switch-to-buffer shell-buffer-name)
    (run-hooks 'shell-mode-hook)))

(defun shell-create-1 ()
  (interactive)
  (shell-create "1"))

(defun shell-create-2 ()
  (interactive)
  (shell-create "2"))

(defun shell-create-3 ()
  (interactive)
  (shell-create "3"))

(defun shell-create-4 ()
  (interactive)
  (shell-create "4"))

(defun shell-create-5 ()
  (interactive)
  (shell-create "5"))

(defun shell-create-6 ()
  (interactive)
  (shell-create "6"))

(defun shell-create-7 ()
  (interactive)
  (shell-create "7"))

(defun shell-create-8 ()
  (interactive)
  (shell-create "8"))

(defun shell-create-9 ()
  (interactive)
  (shell-create "9"))


;; I've copied this function from lisp/shell.el.  I've hacked it to
;; do a better job of keeping the current shell's current working
;; directory following one of the following commands:
;; $ d:
;; $ cd d:
(defun shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp',
and  `shell-popd-regexp', while `shell-pushd-tohome', `shell-pushd-dextract', 
and `shell-pushd-dunique' control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match "^[; \t]*" str) ; skip whitespace
			      (match-end 0)))
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (cond ((string-match (concat "\\`\\(" shell-popd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-popd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-pushd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-pushd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-cd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-cd (comint-substitute-in-file-name arg1)))
		    ((and shell-chdrive-regexp
			  (string-match (concat "\\`\\(" shell-chdrive-regexp
						"\\)\\($\\|[ \t]\\)")
					cmd))
		     (progn
		       (shell-process-cd (comint-substitute-in-file-name cmd))
		       (shell-resync-dirs)
		       )))
		    
	      (setq start (progn (string-match "[; \t]*" str end) ; skip again
				 (match-end 0)))))
	(error "Couldn't cd"))))
