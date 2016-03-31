;;;; Keybindings

;;; prog-mode only keys
(defun set-prog-mode-keys ()
  ;; Dash
  (local-set-key (kbd "C-c C-d") 'dash-at-point))

(add-hook 'prog-mode-hook 'set-prog-mode-keys)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
;;; Adjust text size
(global-set-key (kbd "C-M-=") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;;; Custom
(global-set-key (kbd "C-M-<backspace>") 'sanityinc/kill-back-to-indentation)

;;; Helm
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-z") 'helm-resume)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Disable scroll wheel
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
