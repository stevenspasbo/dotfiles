;;; Emacs Lisp
(defun my/elisp-setup ()
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-c f") 'find-function))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-setup)

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; Clojure
(defun my/clojure-setup()
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t))

;;; Scheme
(setq scheme-program-name "scheme")

;;; Common Lisp
'(inferior-lisp-program "/usr/local/bin/sbcl") ; Sets location of sbcl binary
