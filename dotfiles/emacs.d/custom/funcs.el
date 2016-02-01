;;;; Emacs functions

;; Reloads init.el
(defun my/reload-init ()
    (interactive)
    (load-file (concat user-emacs-directory "/init.el")))
(global-set-key (kbd "C-c r") 'my/reload-init)

(defun date (&optional insert)
  "Gets the current date and time"
  (interactive "P")
  (funcall (if insert 'insert 'message)
	   (format-time-string "%a, %d %b %Y %r" (current-time))))

;; Stolen from http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline131
(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
