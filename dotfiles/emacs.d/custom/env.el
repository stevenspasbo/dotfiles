(setq-default
 shell-file-name "/usr/local/bin/bash" ;; Sets shell
 user-full-name "Steven Spasbo"
 user-mail-address "stevenspasbo@gmail.com"
 undo-limit 10000 ;; Increase undo limit
 linum-format "%4d " ;; Add space after linum
 inhibit-startup-screen t ;; Skip startup screen
 make-backup-files nil ;; Disable backup~
 auto-save-default nil ;; Disable #autosave# files
 confirm-kill-emacs 'y-or-n-p ;; Disallow accidental exits
 debug-on-error t
 ring-bell-function 'ignore
)
