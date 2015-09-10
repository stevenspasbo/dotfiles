;;;; Functions and keybindings

(fset 'yes-or-no-p 'y-or-n-p) ;; Makes yes-or-no questions accept 'y' or 'n'

;; Enable tab completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Search for symbol
(global-set-key (kbd "C-h a") 'apropos)
