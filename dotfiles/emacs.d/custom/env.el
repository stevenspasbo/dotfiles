;; File options

(setq make-backup-files nil) ;; Disable backup~ files
(setq auto-save-default nil) ;; Disable #autosave# files

(setq shell-file-name "/bin/bash") ;; Sets shell

(setq linum-format "%4d ") ;; Add space after linum

(setq undo-limit 100000) ;; Increase undo limit

;; Sets location of sbcl binary
(setq inferior-lisp-program "/usr/local/bin/sbcl")
