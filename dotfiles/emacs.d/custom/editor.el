;;;; System stuff

(require 'rainbow-delimiters)
(require 'diminish)
(require 'saveplace)
(require 'golden-ratio)
(require 'undo-tree)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-config)
(require 'helm-descbinds)
(require 'helm-swoop)
(require 'helm-projectile)
(require 'company)
(require 'flycheck)
(require 'multiple-cursors)

(set-frame-font "Source Code Pro for Powerline-13")

(global-company-mode +1)
(global-flycheck-mode +1)
(yas-global-mode)
(golden-ratio-mode 1)
(blink-cursor-mode 0)
(global-hl-line-mode)  ; Highline current line
(column-number-mode 1) ; Enable (line,column)
(menu-bar-mode -1)     ; Disable menu
(delete-selection-mode t) ; Allows deletions on highlighted text
(global-undo-tree-mode 1)
(helm-descbinds-mode)
(helm-mode 1) ; Sets global helm-mode
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1)   ; Disable tool bar
(transient-mark-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system "helm")
(helm-projectile-on)
(setq projectile-switch-project-action 'projectile-commander)


;;; Personalization
(setq shell-file-name "/usr/local/bin/zsh"
      user-full-name "Steven Spasbo"
      user-mail-address "stevenspasbo@gmail.com"
      save-place-file (concat user-emacs-directory "places")
      inhibit-splash-screen t  ; Don't show splash screen
      inhibit-startup-screen t ; Or startup screen
      debug-on-error t
      comint-prompt-read-only t
      auto-revert-verbose nil
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t ; For helm-mini
      helm-M-x-fuzzy-match t
      helm-swoop-split-direction 'split-window-vertically
      helm-ff-file-name-history-use-recentf t
      company-minimum-prefix-length 2
      company-idle-delay 0.2
      company-tooltip-align-annotations t
      )

(set-face-background 'company-tooltip-annotation (face-background 'company-tooltip))
(set-face-background 'company-tooltip-annotation-selection (face-background 'company-tooltip-selection))

(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (defun nadvice/flycheck-mode-line-status-text (&optional status)
    (let ((text (pcase (or status flycheck-last-status-change)
                  (`not-checked "")
                  (`no-checker "-")
                  (`running "*")
                  (`errored "!")
                  (`finished
                   (if flycheck-current-errors
                       (let ((error-counts (flycheck-count-errors
                                            flycheck-current-errors)))
                         (format "%s/%s"
                                 (or (cdr (assq 'error error-counts)) "")
                                 (or (cdr (assq 'warning error-counts)) "")))
                     ""))
                  (`interrupted "-")
                  (`suspicious "?"))))
      (concat (if (display-graphic-p) " ✓" " Γ") text)))

(advice-add 'flycheck-mode-line-status-text :override
            #'nadvice/flycheck-mode-line-status-text)




(set-face-background 'flycheck-fringe-warning nil)

(set-face-attribute 'flycheck-error nil
                    :foreground nil
                    :background nil
                    :underline "#dc322f")

(set-face-attribute 'flycheck-warning nil
                    :foreground nil
                    :background nil
                    :underline "#b58900")

(set-face-attribute 'flycheck-info nil
                    :foreground nil
                    :background nil
                    :underline "#268bd2")

(setq-default tab-always-indent 'complete ; Enable tab completion
              indent-tabs-mode nil ; Disable all tabs
              require-final-newline 'visit-save ; Insert final newline
              indicate-empty-lines t
              linum-format "%4d  " ; Add space after linum)
              ;; When you visit a file, point goes to the last place where it
              ;; was when you previously visited the same file.
              ;; http://www.emacswiki.org/emacs/SavePlace
              save-place t

              line-spacing 1         ; Easier on the eyes
              undo-limit 10000       ; Who needs 80k undos?
              vc-follow-symlinks t   ; Silently follow symlinks
              make-backup-files nil  ; Disable backup~
              auto-save-default nil  ; Disable #autosave# files
              auto-save-list-file-prefix nil
              ring-bell-function (lambda ()
                                   (message "*beep*"))
              confirm-kill-emacs 'y-or-n-p ; Disallow accidental exits
              initial-scratch-message ""
              frame-title-format "%b (%f)")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-encoding-system 'utf-8)
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(defun ignore-minor-modes ()
  (diminish 'rainbow-mode)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'helm-mode)
  (diminish 'yas-minor-mode)
  (diminish 'eldoc-mode)
  (diminish 'golden-ratio-mode)
  (diminish 'beacon-mode)
  (diminish 'auto-revert-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-mode)
  (diminish 'visual-line-mode))

(setq comint-prompt-read-only t)

;; Don't warn about narrowing
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun prog-setup ()
  (require 'yasnippet)
  (rainbow-delimiters-mode)
  (font-lock-mode 1)
  (rainbow-mode)
  (linum-mode)
  (setq show-trailing-whitespace t)
  ;; Highlight matching parens
  (show-paren-mode 1)
  ;; (yas-minor-mode-on)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)
        rainbow-delimiters-max-face-count 4))

(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground "#FFFFFF")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground "#E8079B")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil
                    :foreground "#2100FF")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil
                    :foreground "#0CD2E8")
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "#E8079B"
                    :background "#00FF00"
                    :weight 'ultra-bold
                    :strike-through t)

(add-hook 'prog-mode-hook 'prog-setup)

(when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))

;; Startup
(when (and
       (not (null (window-system)))    ; If running in a window
       (string= system-type "darwin")) ; And if on a mac
  (exec-path-from-shell-initialize))   ; Match PATH from shell

(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(add-to-list 'completion-styles 'initials t)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*") ; It needs the space

(add-hook 'after-change-major-mode-hook 'ignore-minor-modes)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Remove whitespace on save
