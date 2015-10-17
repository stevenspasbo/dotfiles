;;;; Environment vars

(setq-default
 shell-file-name "/usr/local/bin/zsh" ; Sets shell
 user-full-name "Steven Spasbo" ; Full name
 user-mail-address "stevenspasbo@gmail.com" ; Email
 undo-limit 10000 ; Increase undo limit
 linum-format "%4d " ; Add space after linum
 inhibit-startup-screen t ; Skip startup screen
 make-backup-files nil ; Disable backup~
 auto-save-default nil ; Disable #autosave# files
 yes-or-no-p 'y-or-n-p ; y or n exit
 confirm-kill-emacs 'y-or-n-p ; Disallow accidental exits
 debug-on-error t
 ring-bell-function nil
 vc-follow-symlinks t
 mouse-wheel-mode nil
 )

(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)

(add-to-list 'exec-path "/usr/local/bin")
