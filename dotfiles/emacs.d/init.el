;; Author:     Steven Spasbo
;; Created:    11-30-2014

(require 'cl)

(defvar emacs-home (file-truename "~/.emacs.d/")
  "Retrieves the full path to my emacs directory")

(load-file (file-truename "~/.emacs.d/elisp/load-directory.el"))

;; Adds themes
(add-to-list 'custom-theme-load-path (file-truename "~/.emacs.d/themes/"))

;; Loads all files in each directory
(load-directory (concat emacs-home "custom"))
(load-directory (concat emacs-home "langs"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "b6c9e4fe2fe9b9fc799fc2ccae5218e6b9fe23aab9b55efc41ba9e640827d6f4" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e587bd7ea49915da4556c1f5b535e920cb3f65f033ae636ba8ed0d2ca2a14df4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-weekend-header ((t (:foreground "color-198" :weight extra-bold))))
 '(holiday ((t (:background "color-202")))))
