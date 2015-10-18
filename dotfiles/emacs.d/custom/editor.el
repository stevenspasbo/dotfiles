;;;; System stuff

;;; Personalization
(setq shell-file-name "/usr/local/bin/zsh"
      user-full-name "Steven Spasbo"
      user-mail-address "stevenspasbo@gmail.com")

;;; Helm
(require 'helm)
;(helm-mode 1) ; Sets global helm-mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-h a") 'helm-apropos)
;(global-set-key (kbd "C-h f") 'helm-describe-function)
;(global-set-key (kbd "C-h v") 'helm-describe-variable)

;;; Window
(global-linum-mode 1)  ; Enable line numbers
(global-hl-line-mode)  ; Highline current line
(column-number-mode 1) ; Enable (line,column)
(menu-bar-mode -1)     ; Disable menu
(electric-pair-mode 1)
(setq-default linum-format "%4d ") ; Add space after linum)

(when window-system    ; If standalone emacs application
  (scroll-bar-mode -1) ; Disable scroll bar
  (tool-bar-mode -1)   ; Disable tool bar
  (global-unset-key (kbd "C-z"))) ; Disable minimize to dock key

;;; Text
;; Rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(setq rainbow-delimiters-max-face-count 4)
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground "#FFFFFF")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground "#E8079B")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil
                    :foreground "#2100FF")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil
                    :foreground "#0CD2E8")

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "#E8079B"
                    :background "#FFFFFF"
                    :strike-through t)

(add-hook 'prog-mode-hook
          (lambda()
            (setq show-trailing-whitespace t)))

(show-paren-mode 1) ; Highlight matching parens
(setq-default tab-always-indent 'complete ; Enable tab completion
              indent-tabs-mode nil ; Disable all tabs
              require-final-newline 'visit-save ; Insert final newline
              )
(add-hook 'before-save-hook
	  'delete-trailing-whitespace) ; Remove whitespace on save

(delete-selection-mode t) ; Allows deletions on highlighted text

;; Startup
;(add-to-list 'exec-path "/usr/local/bin")

(setq inhibit-splash-screen t
      inhibit-startup-screen t ; Skip startup screen
      debug-on-error t)

(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(add-to-list 'completion-styles 'initials t)

(setq-default
 undo-limit 10000 ; Increase undo limit
 make-backup-files nil ; Disable backup~
 auto-save-default nil ; Disable #autosave# files
 confirm-kill-emacs 'y-or-n-p ; Disallow accidental exits
 ring-bell-function nil
 vc-follow-symlinks t)

;; Disable scrollwheel
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
