;;;; Text configs

;; Auto-inserts paren and bracket pairs
(electric-pair-mode 1)

(setq indent-tabs-mode nil)		; Spaces, no tabs
(show-paren-mode 1)		        ; Highlight matching parens
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)	; Remove whitespace on save
(setq require-final-newline 't)

(global-hl-line-mode)		        ; Highline current line

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
