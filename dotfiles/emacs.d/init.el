;;;; Author:     Steven Spasbo
;;;; Created:    11-30-2014
;;;; Updated:    03-31-2015

;; Sets location of custom file to unclutter init file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Enable tab completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Loads load-directory function
(load-file
 (file-truename "~/.emacs.d/elisp/load-directory.el"))

;; Sets list of directories to load, then iterates over each
(setq dirs '("custom" "langs"))
(dolist (dir dirs)
  (load-directory (concat user-emacs-directory dir)))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; Load theme
(load-theme 'hipster t)

;; Make them parens purdy
(require 'rainbow-delimiters)
