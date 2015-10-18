;;;; Theme stuff

;; Add custom themes dir
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defun set-theme ()
  "Sets the theme depending on window-system"
  (interactive)
  (cond ((string-equal window-system "ns")
         (load-theme 'monokai t))
        ((string-equal window-system nil)
         (load-theme 'monokai t))))
