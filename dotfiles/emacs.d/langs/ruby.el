;;;; ruby.el - Ruby settings

;; Set file extensions
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'completion-ignored-extensions ".rbc") ; Ignore rubinius bytecode

;; Function to enable syntax checking, ruby electric, robe and company mode
(defun ruby-stuff ()
  (require 'inf-ruby)
  (require 'robe)

  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil
        ruby-deep-indent-paren nil)

  ;; (ruby-electric-mode t)
  ;; (robe-mode)
  (push 'company-robe company-backends)
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Launches inf-ruby and starts robe for auto completion
  (defun run-ruby-and-start-robe ()
    (interactive)
    (inf-ruby)
    (robe-start))

  (global-set-key (kbd "C-c C-e") 'run-ruby-and-start-robe))

;; Add to ruby-mode-hook
(add-hook 'ruby-mode-hook 'ruby-stuff)
