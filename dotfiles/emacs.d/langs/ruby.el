(require 'inf-ruby)
(require 'yaml-mode)


(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
;(defun select-all-and-run ()
;  (interactive)
;    (mark-whole-buffer)
;    (ruby-send-region))
;(add-hook 'ruby-mode-hook
;	  (local-set-key (kbd "C-c C-c") 'select-all-and-run))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc") ; Ignore rubinius bytecode


;; Stupidly the non-bundled ruby-mode isn't a derived mode of
;; prog-mode: we run the latter's hooks anyway in that case.
(add-hook 'ruby-mode-hook
          (lambda ()
            (unless (derived-mode-p 'prog-mode)
              (run-hooks 'prog-mode-hook))))
