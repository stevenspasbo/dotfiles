;;;; Author:     Steven Spasbo
;;;; Created:    11-30-2014
;;;; Updated:    10-18-2015

(package-initialize)

(load-file (expand-file-name "spasbo.el" user-emacs-directory))

(spasbo/load)

;; Sets list of directories to load, then iterates over each
;; (dolist (dir '("custom" "langs"))
;;   (load-directory (concat user-emacs-directory dir)))
(load-directory (concat user-emacs-directory "custom"))
(load-directory (concat user-emacs-directory "langs"))
