
(add-hook 'eshell-mode-hook
  (lambda ()
    (linum-mode 0)
    (setq show-trailing-whitespace nil)))

(setenv "PATH"
        (concat
         "/usr/local/bin:"
         (getenv "PATH")))

(defun get-abbriv-cd ()
  "Gets the current directory, replaces home with ~"
  (interactive)
  (abbreviate-file-name (eshell/pwd)))

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

;; Sets the eshell prompt, based on my bash/zsh prompt
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

(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; (eval-after-load 'esh
;;   '(progn
;;     (add-to-list 'eshell-visual-commands "ssh")
;;     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))
