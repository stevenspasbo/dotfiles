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

;;;; Clojure
(require 'clojure-mode)
(defun my/clojure-setup()
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t))
; This will make relevant lines stand out more in stack traces
(defun sldb-font-lock ()
  (font-lock-add-keywords nil
			  '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)"
			     1 starter-kit-clojure-trace-face)
			    ("[0-9]+: \\(java.*\\)"
			     1 starter-kit-clojure-trace-face)
			    ("[0-9]+: \\(swank.*\\)"
			     1 starter-kit-clojure-trace-face)
			    ("\\[\\([A-Z]+\\)\\]"
			     1 font-lock-function-name-face))))
(add-hook 'sldb-mode-hook 'sldb-font-lock)

;;;; Scheme
(setq scheme-program-name "scheme")

;;;; Common Lisp
