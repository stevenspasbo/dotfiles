;;;; Functions and keybindings

;; Enable tab completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Search for symbol
(global-set-key (kbd "C-h a") 'apropos)
