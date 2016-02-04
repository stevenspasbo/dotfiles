;;;; Keybindings

(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;;; Adjust text size
(global-set-key (kbd "C-M-=") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)


;;; Dash
(global-set-key (kbd "C-c C-d") 'dash-at-point)

;;; Custom
(global-set-key (kbd "C-M-<backspace>") 'sanityinc/kill-back-to-indentation)

;;; Helm
(require 'helm)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)
(helm-mode 1) ; Sets global helm-mode
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-M-z") 'helm-resume)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "C-h a") 'helm-apropos)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t ; For helm-mini
      helm-swoop-split-direction 'split-window-vertically
      )

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Disable scroll wheel
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
