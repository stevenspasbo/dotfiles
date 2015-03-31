;;;; Author:     Steven Spasbo
;;;; Created:    11-30-2014
;;;; Updated:    03-30-2015

(load-file
 (file-truename "~/.emacs.d/elisp/load-directory.el"))

;; Load everything and set the theme
(defvar emacs-home (file-truename "~/.emacs.d")
  "Retrieves the full path to my emacs directory")

;; Sets location of custom file to unclutter init file
(setq custom-file
      (concat emacs-home "/init-custom.el"))
(load custom-file)
;; Sets list of directories to load, then iterates over each
(setq dirs
      '("/custom" "/langs"))
(dolist (dir dirs)
  (load-directory (concat emacs-home dir)))

;; Load theme
(load-theme 'hipster t)

;; Make them parens purdy
(require 'rainbow-delimiters)
