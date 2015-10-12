;;;; All the slimy settings
(require 'slime)
(require 'slime-autoloads)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-to-list 'slime-contribs 'slime-fancy)
(setq inferior-lisp-program "/usr/local/bin/sbcl"
      lisp-indent-function 'common-lisp-indent-function)
(eval-after-load 'slime
  `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))
(slime-setup '(slime-fancy))

;;;; Emacs Lisp
(defun my/elisp-setup ()
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-c f") 'find-function))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-setup)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(add-hook 'sldb-mode-hook 'sldb-font-lock)

;;;; Scheme
(setq scheme-program-name "scheme")

;;;; Common Lisp
