(setq-default
 undo-limit 10000 ;; Increase undo limit
 linum-format "%4d " ;; Add space after linum
 make-backup-files nil ;; Disable backup~
 auto-save-default nil ;; Disable #autosave# files
 shell-file-name "/bin/bash" ;; Sets shell
 inhibit-startup-screen t ;; Skip startup screen
 inferior-lisp-program "/usr/local/bin/sbcl" ;; Sets location of sbcl binary
 confirm-kill-emacs 'y-or-n-p ;; Disallow accidental exits
 user-full-name "Steven Spasbo")


(autoload 'neotree-toggle "neotree")
