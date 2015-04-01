
(defun my/reload-init ()
    (interactive)
    (load-file (concat emacs-home "/init.el")))
(global-set-key (kbd "C-c r") 'my/reload-init)
