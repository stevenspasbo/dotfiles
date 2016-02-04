;;;; System stuff

;;; Personalization
(setq shell-file-name "/usr/local/bin/zsh"
      user-full-name "Steven Spasbo"
      user-mail-address "stevenspasbo@gmail.com")

;;; Window
;(global-linum-mode 1)  ; Enable line numbers
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
(defun prog-setup ()
  (rainbow-delimiters-mode)
  (rainbow-mode)
  (linum-mode))

(add-hook 'prog-mode-hook 'prog-setup)

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
                    :background "#00FF00"
                    :weight 'ultra-bold
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
(when (and
       (not (null (window-system)))    ; If running in a window
       (string= system-type "darwin")) ; And if on a mac
  (exec-path-from-shell-initialize))   ; Match PATH from shell

(setq inhibit-splash-screen t  ; Don't show splash screen
      inhibit-startup-screen t ; Or startup screen
      debug-on-error t)

(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(add-to-list 'completion-styles 'initials t)

(setq-default
 scroll-margin 5        ; Scroll when cursor is 5 lines from top or bottom
 line-spacing 1         ; Easier on the eyes
 undo-limit 10000       ; Who needs 80k undos?
 vc-follow-symlinks t   ; Silently follow symlinks
 make-backup-files nil  ; Disable backup~
 auto-save-default nil  ; Disable #autosave# files
 ring-bell-function (lambda ()
                      (message "*beep*"))
 confirm-kill-emacs 'y-or-n-p ; Disallow accidental exits
 initial-scratch-message ""
 )

(require 'golden-ratio)
(golden-ratio-mode 1)

;; (nyan-mode)
;; (setq nyan-wavy-trail t)

;; smart-mode-line
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/theme 'dark)
;; (sml/setup)
;; (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":DOTFILES:"))
;; (setq hidden-minor-modes '(
;;                            " AC"
;;                            "Undo-Tree"
;;                            "Golden"
;;                            "Helm"))
;; (setq sml/hidden-modes (mapconcat 'identity hidden-minor-modes "\\| *"))
