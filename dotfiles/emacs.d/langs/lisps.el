;;;; All the slimy settings
(require 'slime)
(require 'slime-autoloads)
(require 'scheme)

;; (defun slime-setup ()
;;   (slime-mode t)
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;; 	lisp-indent-function 'common-lisp-indent-function))

;(add-hook 'lisp-mode-hook 'slime-setup)
(add-to-list 'slime-contribs 'slime-fancy)
(eval-after-load 'slime
  `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))
(slime-setup '(slime-fancy))

(defun emacs-lisp-stuff ()
  (eldoc-mode)
  (prettify-symbols-mode))

;;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-stuff)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'sldb-mode-hook 'sldb-font-lock)

;;;; Scheme
(setq scheme-program-name "scheme")
