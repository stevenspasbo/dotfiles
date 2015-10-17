(add-hook 'eshell-mode-hook
  (lambda ()
    (linum-mode 0)
    (setq show-trailing-whitespace nil)))

;; eshell-banner stuff

;; (add-hook 'eshell-post-command-hook
;;   (lambda ()
;;     (if (not (eq linum-mode 0)) (linum-mode 0))))

(defun eshell/emacs (&rest args)
  "Open a file in emacs"
  (if (null args)
      ;; Already in emacs, but whatever
      (bury-buffer)
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun get-abbriv-cd ()
  "Gets the current directory, replaces home with ~"
  (interactive)
  (abbreviate-file-name (eshell/pwd)))

(defun myclear ()
  (let ((heigth (- (window-body-height) 1)))
    (while (> heigth 0)
      (eshell-send-input)
      (setq heigth (- heigth 1)))))

(defun current-git-branch (pwd)
    "Returns current git branch as a string.
If string is empty, current directory is not a git repo"
    (interactive)
    (when (and (eshell-search-path "git")
	       (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
	(if (> (length git-output) 0)
	    (concat " (" (substring git-output 0 -1) ")" )
	  ""))))

(setq eshell-prompt-function
      (lambda ()
        (let* ((dirz (get-abbriv-cd))
	       (my/host (system-name))
	       (uzr (getenv "USER"))
	       (git-branch (or (current-git-branch (substring (pwd) 10)) "")))
          (concat
	   (propertize "[" 'face `(:foreground "#FFFFFF"))
	   (propertize uzr 'face `(:foreground "#1585C6"))
	   (propertize "@" 'face `(:foreground "#D63883" :weight bold))
	   (propertize my/host 'face `(:foreground "#22A198"))
	   (propertize ": " 'face `(:foreground "#22A198"))
	   (propertize dirz 'face `(:foreground "#7BC783"))
	   (propertize "]" 'face `(:foreground "#FFFFFF"))
	   (propertize git-branch 'face `(:foreground "#FFFFFF"))
	   (propertize "\nλ " 'face `(:foreground "#7BC783"))))))

(setq eshell-prompt-regexp ". ")
;; 	       (directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
;;                (parent (car directory))
;;                (name (cadr directory))
;;                (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))
