;;;; Text configs

;; Auto-inserts paren and bracket pairs
(electric-pair-mode 1)

(setq indent-tabs-mode nil)		; Spaces, no tabs
(show-paren-mode 1)		        ; Highlight matching parens
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)	; Remove whitespace on save

(global-hl-line-mode)		        ; Highline current line
(set-face-background
 'hl-line "color-234")			; Sets line color

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
