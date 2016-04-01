;; (defun drupal-mode ()
;;   "Drupal php-mode."
;;   (interactive)
;;   (php-mode)
;;   (message "Drupal mode activated.")
;;   (set 'tab-width 2)
;;   (set 'c-basic-offset 2)
;;   (set 'indent-tabs-mode nil)
;;   (c-set-offset 'case-label '+)
;;   (c-set-offset 'arglist-intro '+)                     ; for FAPI arrays and DBTNG
;;   (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
;;   ;; More Drupal-specific customizations here
;; )

;; (defconst my-php-style
;;   '((c-offsets-alist . (
;;     (arglist-close . c-lineup-close-paren))))
;;   "My PHP Programming style"
;;   )
;; (c-add-style "my-php-style" my-php-style)
;; (defun my-php-mode ()
;;   "My personal php-mode customizations."
;;   (c-set-style "my-php-style")
;;   ;; More generic PHP customizations here
;; )

;; (defun setup-php ()
;;   ; PHP
;;   (add-hook 'php-mode-hook 'my-php-mode)

;;   ; Drupal
;;   (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
;;   (add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|inc\\)$" . drupal-mode))
;;   (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
;;   ;; More startup-setup for PHP customizations to work here
;; )

;; (setup-php)

;; (add-hook 'php-mode-hook 'drupal-mode)
