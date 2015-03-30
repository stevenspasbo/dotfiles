;;; Author:     Steven Spasbo
;;; Created:    11-30-2014

(require 'cl)

(defvar emacs-home (file-truename "~/.emacs.d/")
  "Retrieves the full path to my emacs directory")

(load-file (file-truename "~/.emacs.d/elisp/load-directory.el"))

;; Adds themes
(add-to-list 'custom-theme-load-path (file-truename "~/.emacs.d/themes/"))

;; Loads all files in each directory
(load-directory (concat emacs-home "custom"))
(load-directory (concat emacs-home "langs"))

(load-theme 'hipster t)
