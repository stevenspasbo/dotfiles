(add-hook 'eshell-mode-hook
	  (lambda ()
	    (linum-mode 0)
	    (hl-line-mode 0)))

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and
	 (>= (length pwd) home-len)
	 (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len)) pwd)))

(defun current-git-branch (pwd)
    "Returns current git branch as a string.
If string is empty, current directory is not a git repo"
    (interactive)
    (when (and (eshell-search-path "git")
	       (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
	(if (> (length git-output) 0)
	    (concat "(" (substring git-output 0 -1) ")" )
	    ""))))

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(let ((uzr (getenv "USER"))
;; 	      (hozt (system-name))
;; 	      (dirz (pwd)))
;; 	  (concat "[" uzr "@" hozt ": " dirz "]\n" "λ "))))
;; (setq eshell-highlight-prompt nil)
;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(current-git-branch (eshell/pwd))
;;         (let* (
;; 	       (directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
;;                (parent (car directory))
;;                (name (cadr directory))
;;                (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))

;;           (if (eq 'dark (frame-parameter nil 'background-mode))
;;               (concat   ;; Prompt for Dark Themes
;;                (propertize parent 'face `(:foreground "#8888FF"))
;;                (propertize name   'face `(:foreground "#8888FF" :weight bold))
;;                (propertize branch 'face `(:foreground "green"))
;;                (propertize " $"   'face `(:weight ultra-bold))
;;                (propertize " "    'face `(:weight bold)))

;;             (concat    ;; Prompt for Light Themes
;;              (propertize parent 'face `(:foreground "blue"))
;;              (propertize name   'face `(:foreground "blue" :weight bold))
;;              (propertize branch 'face `(:foreground "dark green"))
;;              (propertize " $"   'face `(:weight ultra-bold))
;;              (propertize " "    'face `(:weight bold)))))))
