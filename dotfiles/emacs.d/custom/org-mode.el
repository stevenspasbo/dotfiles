;;;; Org mode settings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Babel
(setq org-babel-tangle-pad-newline nil)

(add-hook 'org-mode-hook 'linum-mode)
