;;;; Author: Steven Spasbo

;;; Bootstraps the loading of emacs-init.org

;; Speed up loading
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Load the two packages we need
(require 'package)
(require 'ob-tangle)

;; If emacs-init.el doesn't exist, tangle it
(let ((emacs-init-dot-el (concat user-emacs-directory "emacs-init.el"))
      (emacs-init-dot-org (concat user-emacs-directory "emacs-init.org")))
  (if (not (file-exists-p emacs-init-dot-el))
      (org-babel-tangle-file emacs-init-dot-org))
  (load-file emacs-init-dot-el))
