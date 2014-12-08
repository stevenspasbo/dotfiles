;; Text options

(electric-pair-mode 1) ;; Auto-inserts paren and bracket pairs

(show-paren-mode 1) ;; Highlight matching parens

(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete whitespace on save
