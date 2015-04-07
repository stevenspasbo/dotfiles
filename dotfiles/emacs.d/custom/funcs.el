;;;; Emacs functions

;; Reloads init.el
(defun my/reload-init ()
    (interactive)
    (load-file (concat emacs-home "/init.el")))
(global-set-key (kbd "C-c r") 'my/reload-init)

(defun date (&optional insert)
  "Gets the current date and time"
  (interactive "P")
  (funcall (if insert 'insert 'message)
	   (format-time-string "%a, %d %b %Y %r" (current-time))))
