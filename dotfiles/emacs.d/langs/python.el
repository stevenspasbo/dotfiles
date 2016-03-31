(require 'elpy)
(require 'company)

(defun python-mode-setup ()
  (elpy-mode)
  (when (executable-find "ipython")
    (elpy-use-ipython))
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

  (define-key python-mode-map (kbd "RET")
    'newline-and-indent))

(add-hook 'python-mode-hook 'python-mode-setup)
