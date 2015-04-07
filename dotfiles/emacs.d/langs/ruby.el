
;; (add-hook 'ruby-mode-hook )

(require 'inf-ruby)
(require 'ruby-electric)

(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
;(defun select-all-and-run ()
;  (interactive)
;    (mark-whole-buffer)
;    (ruby-send-region))
;(add-hook 'ruby-mode-hook
;	  (local-set-key (kbd "C-c C-c") 'select-all-and-run))
