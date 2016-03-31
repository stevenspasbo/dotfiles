
(defun haskell-mode-stuff ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (interactive-haskell-mode)
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  ;; Ignore compiled files
  (add-to-list 'completion-ignored-extensions ".hi"))

(add-hook 'haskell-mode-hook 'haskell-mode-stuff)
