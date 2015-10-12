(package-initialize)

;;;; Author:     Steven Spasbo
;;;; Created:    11-30-2014
;;;; Updated:    08-13-2015

(require 'cl)

;; Sets location of custom file to unclutter init file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Loads load-directory function
(load-file
 (file-truename "~/.emacs.d/elisp/load-directory.el"))

;; Sets list of directories to load, then iterates over each
(dolist (dir '("custom" "langs"))
  (load-directory (concat user-emacs-directory dir)))

(set-theme)
