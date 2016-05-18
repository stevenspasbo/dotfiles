;;;; Author:     Steven Spasbo
;;;; Created:    11-30-2014
;;;; Updated:    05-17-2016

;; Lowers startup time by about ~25%
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;; Variables - constants/aliases
(defconst emacs-start-time (current-time))
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

;;; Custom-file - Auto generated from customize-faces, etc
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Variables - defaults
(setq shell-file-name "/usr/local/bin/zsh"
      user-full-name "Steven Spasbo"
      user-mail-address "stevenspasbo@gmail.com"
      inhibit-splash-screen t  ; Don't show splash screen
      inhibit-startup-screen t ; Or startup screen
      debug-on-error t
      system-uses-terminfo nil)

(setq-default tab-always-indent 'complete ; Enable tab completion
              indent-tabs-mode nil ; Disable all tabs
              require-final-newline 'visit-save ; Insert final newline
              indicate-empty-lines t
              linum-format "%4d  " ; Add space after linum)
              line-spacing 1         ; Easier on the eyes
              undo-limit 10000
              vc-follow-symlinks t   ; Silently follow symlinks
              make-backup-files nil  ; Disable backup~
              auto-save-default nil  ; Disable #autosave# files
              auto-save-list-file-prefix nil
              ring-bell-function (lambda ())
              confirm-kill-emacs 'y-or-n-p ; Disallow accidental exits
              initial-scratch-message ""
              frame-title-format "%b (%f)")

;;; Text - Overkill encoding
(prefer-coding-system 'utf-8)
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
 (set-selection-coding-system 'utf-8))

;; Don't warn about narrowing
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Don't warn about upcase/downcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Functions - elisp
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
	(load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(defun my/turn-on-hl-line-mode ()
  "Turn on hl-line-mode"
  (interactive)
  (hl-line-mode 1))

(add-hook 'prog-mode-hook #'my/add-watchwords)
(add-hook 'prog-mode-hook #'my/turn-on-hl-line-mode)

;; Reloads init.el
(defun my/reload-init ()
    (interactive)
    (load-file (concat user-emacs-directory "/init.el")))
(global-set-key (kbd "C-c r") 'my/reload-init)

;; Stolen from http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline131
(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(global-set-key (kbd "C-M-<backspace>") 'sanityinc/kill-back-to-indentation)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;;; Packages - Get ready for them
(package-initialize)
(require 'package)
(setq package-archives
      '(("marmalade"    . "http://marmalade-repo.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))
(let ((my-packages
       '(use-package diminish
          ;; Themes
          hipster-theme moe-theme zenburn-theme ujelly-theme tronesque-theme
          tangotango-theme color-theme-sanityinc-tomorrow cyberpunk-theme
          solarized-theme monokai-theme gotham-theme farmhouse-theme
          material-theme afternoon-theme))
         (refreshed? nil))
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (when (null refreshed?)
        (package-refresh-contents)
        (setq refreshed? t))
      (package-install p))))

(use-package comint
  :defer
  :config
  (setq comint-prompt-read-only nil))


;; (use-package use-package
;;   :config
;; (setq use-package-verbose t)
;; )
;;         ;; use-package-always-ensure t))

;; (use-package diminish :ensure :defer)

;;; Packages - theming
(use-package powerline
  :ensure
  :defer
  :config
  (setq ns-use-srgb-colorspace nil)) ;; Displays arrows incorrectly if not set

(use-package spaceline-config
  :ensure spaceline
  :commands (spaceline-emacs-theme spacemacs-helm-mode)
  :config
  ;; Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
  ;; butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
  ;; utf-8.
  ;; (setq powerline-default-separator 'utf-8)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package nyan-mode :ensure :defer)

;;; Packages - Text
;; Change text size
(global-set-key (kbd "C-M-=") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)

(use-package multiple-cursors
  :ensure
  :bind (("C-c SPC" . set-rectangular-region-anchor)))
(use-package paredit
  :ensure
  :defer
  :config
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))

(use-package avy :ensure :defer)

(use-package undo-tree
  :ensure
  :diminish ""
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)
         ("C-S-z" . redo)))
(use-package autorevert
  :defer
  :config
  (setq auto-revert-verbose  nil))

(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "places"))
  ;; When you visit a file, point goes to the last place where it
  ;; was when you previously visited the same file.
  ;; http://www.emacswiki.org/emacs/SavePlace
  (setq-default save-place t))

(use-package rainbow-delimiters
  :ensure
  :defer
  :config
  (setq rainbow-delimiters-max-face-count 4)
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
                      :strike-through t))

(use-package rainbow-mode
  :ensure
  :diminish rainbow-mode
  :commands rainbow-mode)


(when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))


;;; Packages - Code checking
(use-package flycheck
  :ensure
  :init
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))
  :diminish "" ; Currently using spaceline that sets custom flycheck... checking
  :config
  ;; Set flycheck faces
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
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(use-package helm-flycheck :defer :ensure)
(use-package flycheck-pos-tip :defer :ensure)

;; (require 'ivy)
;; (use-package ivy
;;   :ensure swiper
;;   :commands (ivy-mode)
;;   :defer
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-height 10)
;;   (setq ivy-count-format "(%d/%d) ")
;;   :bind
;;   (("C-s" . swiper)
;;    ("M-x" . counsel-M-x)))
;; (ivy-mode 1)

;;; Packages - Buffer navigation
(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))


;;; Packages - All the helm stuff
(use-package helm
  :ensure
  :diminish ""
  :init
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x r b" . helm-bookmarks)
         ("M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-M-z" . helm-resume)
         ("C-f" . helm-semantic-or-imenu)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  (setq helm-split-window-in-side-p t))

(use-package helm-ag :ensure :defer)

(use-package helm-grep
  :bind
  (("C-c h g" . helm-do-grep-ag)))

(use-package helm-command
  :defer
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package helm-buffers
  :defer
  :config
  (setq helm-buffers-fuzzy-matching t))

(use-package helm-files
  :defer
  :config
  (setq helm-recentf-fuzzy-match t ; For helm-mini
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files t)
  (add-to-list 'helm-boring-file-regexp-list "GTAGS$"))

(use-package helm-descbinds
  :ensure
  :defer 10
  :init (helm-descbinds-mode)
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-swoop
  :ensure
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop)
         :map helm-swoop-map
         ("M-I" . helm-multi-swoop-all-from-helm-swoop)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  :config
  (setq helm-swoop-split-direction 'split-window-vertically))

;;; Packages - Project navigation
(use-package neotree
  :ensure
  :bind (("C-c n" . neotree))
  :config
  (setq-default neo-smart-open t
                neo-dont-be-alone t)
  (setq neo-theme 'classic)
  )

(use-package projectile
  :ensure
  :commands (projectile-mode)
  :config
  (progn
    (setq projectile-completion-system "helm")
    (helm-projectile-on)
    (setq projectile-switch-project-action 'projectile-commander)))
(use-package magit
  :ensure
  :bind (("C-x g" . magit-status)))
(use-package helm-projectile :ensure :defer)

;;; Packages - code completion
(use-package company
  :ensure
  :defer 1
  :diminish ""
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-tooltip-align-annotations t)
  (set-face-background 'company-tooltip-annotation (face-background 'company-tooltip))
  (set-face-background 'company-tooltip-annotation-selection (face-background 'company-tooltip-selection)))

(use-package yasnippet
  :ensure
  :commands (yas-global-mode yas-minor-mode)
  :diminish (yas-minor-mode "y")
  ;; :config
  ;; ;; Add yasnippet support for all company backends
  ;; ;; https://github.com/syl20bnr/spacemacs/pull/179
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")
  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))
)
;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(use-package hippie-exp
  :bind
  (("M-/" . hippie-expand))
  :init
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))



;;; Packagess - System
(use-package exec-path-from-shell :ensure :defer)

(use-package keyfreq
  :ensure
  :defer)

(use-package esup :ensure :defer)

(use-package golden-ratio
  :ensure
  :defer
  :diminish ""
  :config
  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*"))

;; Startup
;; (when (and
;;        (not (null (window-system)))    ; If running in a window
;;        (string= system-type "darwin")) ; And if on a mac
;;   (exec-path-from-shell-initialize))   ; Match PATH from shell

;; (add-to-list 'completion-styles 'initials t)

;; Disable scroll wheel
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)


;;;; FINAL after-init function - Sets theme and fonts, global modes
(defun after-init-enable-global-modes ()
  "Enables, disables, and diminishes some modes"
  (global-auto-revert-mode)
  (blink-cursor-mode 0)
  (global-hl-line-mode)  ; Highline current line
  (column-number-mode 1) ; Enable (line,column)
  (delete-selection-mode t) ; Allows deletions on highlighted text
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (global-undo-tree-mode 1)
  (golden-ratio-mode 1)
  (global-flycheck-mode 1)
  (global-company-mode)
  (global-prettify-symbols-mode)
  (electric-pair-mode 1)
  (transient-mark-mode 1)
  (yas-global-mode 1)

  (helm-mode 1)
  (helm-descbinds-mode 1)

  (diminish 'visual-line-mode)
  (diminish 'helm-mode)

  (global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

  (if (display-graphic-p)
      (progn
        (load-theme 'farmhouse-dark t)
        ;; (powerline-default-theme)
        (spaceline-emacs-theme)
        (spaceline-helm-mode)
        (set-frame-font "Source Code Pro for Powerline-13"))
    (load-theme 'sanityinc-tomorrow-night t)))

(defun prog-setup ()
  (font-lock-mode 1)
  (rainbow-mode 1)
  (linum-mode 1)
  (setq show-trailing-whitespace t)
  ;; Highlight matching parens
  (show-paren-mode 1)
  (rainbow-delimiters-mode))

;;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace) ; Remove whitespace on save
(add-hook 'prog-mode-hook #'prog-setup)
(add-hook 'before-save-hook
          (lambda ()
            ;; nothing yet
            ))


(add-hook 'after-init-hook #'after-init-enable-global-modes)


;;;; Langs

;;; Javascript
(use-package js2-mode
  :ensure
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))))

;;; Elm
(use-package elm-mode
  :ensure
  :defer)

;;; Lang - Elixer
(use-package alchemist
  :ensure
  :defer
  :config
  (setq alchemist-mix-command "/usr/local/bin/mix")
  (setq alchemist-iex-program-name "/usr/local/bin/iex")
  (setq alchemist-execute-command "/usr/local/bin/elixir")
  (setq alchemist-compile-command "/usr/local/bin/elixirc"))

;;; Lang - Haskell
(use-package haskell-mode
  :ensure
  :commands haskell-mode
  :config
  (progn
    (use-package flycheck-haskell :ensure)
    (use-package company-ghci :ensure)
    (use-package ghc :ensure)
    (use-package hi2 :ensure)

    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t)))


(add-hook 'haskell-mode-hook
          (lambda ()
            (push 'company-ghci company-backends)
            (haskell-doc-mode)
            (turn-on-haskell-indent)
            (interactive-haskell-mode)))


(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Ignore compiled files
(add-to-list 'completion-ignored-extensions ".hi")

(use-package eldoc
  :defer
  :diminish eldoc-mode)

(use-package slime
  :ensure
  :defer
  :bind (:map slime-prefix-map ("M-h" . slime-documentation-lookup))
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (slime-setup '(slime-fancy))
  (let ((homebrew-sbcl "/usr/local/bin/sbcl"))
    (if (file-exists-p homebrew-sbcl)
        (setq inferior-lisp-program homebrew-sbcl)))
  (setq lisp-indent-function 'common-lisp-indent-function))

(add-hook 'sldb-mode-hook 'sldb-font-lock)

;;;; Lang - Scheme
(use-package scheme
  :ensure
  :defer
  :config
  (use-package scheme-complete :ensure :defer)
  (setq scheme-program-name "scheme"))

;;; Lang - Racket
(use-package racket-mode
  :ensure
  :defer)

(use-package geiser
  :ensure
  :defer)

;;; Lang - LISP??? WHICH??
(use-package lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c v" . eval-buffer)))

(defun emacs-lisp-stuff ()
  (eldoc-mode)
  (add-to-list 'prettify-symbols-alist '("defun" . ?ƒ)))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-stuff)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;;; Lang - python
(use-package python-mode
  :ensure
  :defer
  :init
  (use-package company-jedi :ensure :defer)
  (use-package elpy
    :ensure
    :defer
    :init
    :config
    (when (executable-find "ipython")
      (elpy-use-ipython))
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))
  :bind (:map python-mode-map
              ("RET" . newline-and-indent))
  :config
  (add-to-list 'prettify-symbols-alist '("lambda" . 955))
  (elpy-enable))

;;; Lang - Android
(use-package android-mode
  :ensure
  :defer
  :config
  (let ((local-android-home "/Development/Android/sdk"))
    (if (file-exists-p local-android-home)
        (setq android-mode-sdk-dir local-android-home))))

;;; Lang - C
(use-package cc-mode
  :defer
  :init
  (use-package company-c-headers
    :ensure
    :defer
    :config)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  ;; (global-semanticdb-minor-mode 1)
  ;; (global-semantic-idle-scheduler-mode 1)
  ;; (semantic-mode 1)
)

;;; Lang - Clojure
(use-package clojure-mode
  :ensure
  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode)
         ("lein-env" . enh-ruby-mode))
  :bind
  (:map clojure-mode-map
        ("C-c C-h" . clojure-cheatsheet))
  :init
  (progn
    (font-lock-add-keywords
     nil
     '(("(\\(facts?\\)"
        (1 font-lock-keyword-face))
       ("(\\(background?\\)"
        (1 font-lock-keyword-face)))))
  :config
  (use-package flycheck-clojure :ensure :defer)
  (use-package clojure-cheatsheet :ensure :defer)
  (use-package clojure-mode-extra-font-locking :ensure :defer)
  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (subword-mode)
  (enable-paredit-mode)
  (eldoc-mode)
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (add-to-list 'prettify-symbols-alist '("defn" . ?ƒ)))

(use-package cider
  :ensure
  :bind (:map clojure-mode-map
              ("C-c C-v" . cider-start-http-server)
              ("C-M-r" . cider-refresh)
              ("C-c u" . cider-user-ns))
  :config
  (progn
    (paredit-mode 1)
    (setq
     ;; go right to the REPL buffer when it's finished connecting
     cider-repl-pop-to-buffer-on-connect t
     ;; When there's a cider error, show its buffer and switch to it
     cider-show-error-buffer t
     cider-auto-select-error-buffer t
     ;; Where to store the cider history.
     cider-repl-history-file "~/.emacs.d/cider-history"
     ;; Wrap when navigating history.
     cider-repl-wrap-history t)
    (cider-turn-on-eldoc-mode)

    (defun cider-start-http-server ()
      (interactive)
      (cider-load-current-buffer)
      (let ((ns (cider-current-ns)))
        (cider-repl-set-ns ns)
        (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
        (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

    (defun cider-refresh ()
      (interactive)
      (cider-interactive-eval (format "(user/reset)")))

    (defun cider-user-ns ()
      (interactive)
      (cider-repl-set-ns "user"))))

;;; Lang - PHP / Drupal
(use-package company :ensure)
(use-package ac-php :ensure :defer)
(use-package ac-php-company :defer)

(use-package php-mode
  :ensure
  :defer
  :config
  (setq php-mode-coding-style `Drupal
        tab-width 2
        c-basic-offset 2
        indent-tabs-mode nil)

  )

(use-package helm-gtags
  :ensure
  :defer
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t
        helm-gtags-prefix-key "\C-cg"))


(use-package ggtags
  :ensure
  :defer
  :config
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  ;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

  ;;;; Patch ggtags-process-string
  (defun ggtags-process-string (program &rest args)
    (with-temp-buffer
      (let ((exit (apply #'process-file
                         (ggtags-program-path program) nil t nil args))
            (output (progn
                      (goto-char (point-max))
                      (skip-chars-backward " \t\n")
                      (buffer-substring (point-min) (point)))))
        (or (zerop exit)
            (error "`%s' non-zero exit: %s" program output))
        output))))

(defgroup drupal nil "Drupal IDE" :group 'programming)

(defvar drupal-mode-hook nil)

(define-derived-mode drupal-mode php-mode "Drupal"
  "Custom major mode for Drupal."
  :group 'drupal

  (defconst drupal-mode-version "v0.0.1"
    "Current version of drupal-mode")

  (defcustom drupal-api-version 8
    "Drupal API version"
    :type 'string
    :group 'drupal)

  (defcustom drupal-api-url "http://api.drupal.org/api/search"
	"URL for API search."
	:type 'string
	:group 'drupal)
  (defcustom drupal-ide-use-etags nil
	"Use TAGS file prepared with etags/ctags for code navigation and hook templates."
	:type 'boolean
	:group 'drupal)

  (prettify-symbols-mode)
  (php-enable-drupal-coding-style)
  (add-to-list 'company-backends 'company-ac-php-backend)
  (ggtags-mode 1)
  (setq ggtags-highlight-tag nil)
  (helm-gtags-mode)
  (run-hooks 'drupal-mode-hook))

(add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|inc\\)$" . drupal-mode))


;;; WIP
;; (defun drupal-module-name ()
;;   (let ((current-file (buffer-file-name)))
;;     )
;;   "Something or other")

;; (defun drupal-module-name-insert ()
;;      (interactive)
;;      (insert (drupal-module-name)))


;;; ENDWIP


;; (defconst my-php-style
;;   '((c-offsets-alist . (
;;     (arglist-close . c-lineup-close-paren))))

;;   (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
;;   (add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|inc\\)$" . drupal-mode))
;;   (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
;;   ;; More startup-setup for PHP customizations to work here
;; )

;; (setup-php)

;; (add-hook 'php-mode-hook 'drupal-mode)

;;;; ruby.el - Ruby settings
(defun run-ruby-and-start-robe ()
  (interactive)
  (inf-ruby)
  (robe-start))

(use-package yaml-mode :ensure :defer)

(use-package ruby-mode
  :defer
  :bind (("C-c C-c" . run-ruby-and-start-robe))
  :mode (("Vagrantfile$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode))
  :init
  (use-package robe
    :ensure
    :defer
    :config
    (push 'company-robe company-backends))
  (use-package ruby-electric :ensure :defer)
  (use-package inf-ruby :ensure :defer)

  :config
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil
        ruby-deep-indent-paren nil)
  (ruby-electric-mode t)
    (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook)))

(add-hook 'ruby-mode-hook
          (lambda ()
            ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
            ;; prog-mode: we run the latter's hooks anyway in that case.
            (unless (derived-mode-p 'prog-mode)
              (run-hooks 'prog-mode-hook))))

(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'completion-ignored-extensions ".rbc") ; Ignore rubinius bytecode


;;;; Org mode
(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-capture
  :defer
  :config
  (progn
    (setq org-capture-templates '())

    ;; Prefixes
    ;; Creates Personal and Work prefixes so we can access to more keys
    (add-to-list 'org-capture-templates '("p" "Prefix for personal tasks"))
    (add-to-list 'org-capture-templates '("w" "Prefix for work tasks"))

    ;; Headlines
    ;; planner.org has two main headlines, Work and Personal,
    ;; this creates a template for adding in new headlines to them
    (add-to-list 'org-capture-templates
                 '("ph" "Personal headline" entry (file+headline org-default-notes-file "Personal")
                   "* %?"))
    (add-to-list 'org-capture-templates
                 '("wh" "Work headline" entry (file+headline org-default-notes-file "Work")
                   "* %?"))

    ;; Personal templates
    (add-to-list 'org-capture-templates
                 '("pt" "Personal task" entry (file+olp org-default-notes-file "Personal" "TODOs")
                   "* TODO %^{Task}%^g\nSCHEDULED: %T"))

    ;; Work templates
    (add-to-list 'org-capture-templates
                 '("wm" "Meeting notes" entry (file+headline org-default-notes-file "Meetings")
                   "* %^{Meeting name} on %U\n%?"
                   :prepend))
    (add-to-list 'org-capture-templates
                 '("wt" "Work task" entry (file+olp org-default-notes-file "Work" "TODOs")
                   "* TODO %^{Task} %^g\nSCHEDULED: %T"))

    (add-to-list 'org-capture-templates
                 '("wl" "Logbook entry" entry (file+datetree org-default-notes-file)
                   "* %U - %^{Activity}  :LOG:"))

    ;; Misc templates
    ;; (add-to-list 'org-capture-templates
    ;;              '("p" "Programming TODO"))
    ;; (add-to-list 'org-capture-templates
    ;;              '("b" "Birthday" entry (file+headline org-default-notes-file "Birthdays")
    ;;                "* %^{Name} - "))

    ))

;; (require 'recentf)
;; (setq recentf-max-saved-items 200
;;       recentf-max-menu-items 15)
;; (recentf-mode)

(use-package org
  :ensure
  :defer 1
  :init
  (global-set-key (kbd "C-c o") (lambda () (interactive) (find-file org-default-notes-file)))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link)
         ("C-c C-n" . org-add-note))
  :config
  (progn
    (use-package org-indent
      :diminish "")

    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'hl-line-mode)

    (let ((db-org "~/Dropbox/org-files"))
      (if (file-exists-p db-org)
          (setq org-directory db-org)))
    (setq
     org-default-notes-file "~/Dropbox/org-files/planner.org"
     org-startup-indented t

     org-refile-targets '((org-agenda-files . (:maxlevel . 4)))

     org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "APPT(a)" "|" "DONE(d)")
                         (sequence "WAITING(w)" "|")
                         (sequence "|" "CANCELED(c)"))

     org-columns-default-format "%15CATEGORY %50ITEM %9TODO(Status) %TAGS(Tags)"

     org-todo-keyword-faces
     '(("TODO" . org-warning)
       ("STARTED" . (:background "dark cyan" :foreground "light green" :weight "bold"))
       ("WAITING" . (:background "dark magenta" :foreground "white"))
       ("CANCELED" . (:background "red" :foreground "yellow" :strike-through)))
     org-log-done t
     org-ellipsis " ⤵"
     org-src-fontify-natively t
     org-reverse-note-order t)))


  (defun get-abbriv-cd ()
  "Gets the current directory, replaces home with ~"
  (interactive)
  (abbreviate-file-name (eshell/pwd)))

(defun current-git-branch (pwd)
  "Returns current git branch as a string.
If string is empty, current directory is not a git repo"
  (interactive)
  (when (and (eshell-search-path "git")
	     (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
	  (concat " (" (substring git-output 0 -1) ")" )
	""))))

(use-package eshell-prompt-extras :ensure :defer)
(use-package eshell
  :bind (("C-c s"  . eshell))
  :config
  (progn
    (use-package em-prompt)
    (use-package em-cmpl)
    ;; (setq eshell-prompt-function
    ;;   (lambda ()
    ;;     (let* ((dirz (get-abbriv-cd))
    ;;            (my/host (system-name))
    ;;            (uzr (getenv "USER"))
    ;;            (git-branch (or (current-git-branch (substring (pwd) 10)) "")))
    ;;       (concat
    ;;        (propertize "[" 'face `(:foreground "#FFFFFF"))
    ;;        (propertize uzr 'face `(:foreground "#1585C6"))
    ;;        (propertize "@" 'face `(:foreground "#D63883" :weight bold))
    ;;        (propertize my/host 'face `(:foreground "#22A198"))
    ;;        (propertize ": " 'face `(:foreground "#22A198"))
    ;;        (propertize dirz 'face `(:foreground "#7BC783"))
    ;;        (propertize "]" 'face `(:foreground "#FFFFFF"))
    ;;        (propertize git-branch 'face `(:foreground "#FFFFFF"))
    ;;        (propertize "\nλ " 'face `(:foreground "#7BC783"))))))
    ;; (setq eshell-prompt-regexp ". ")

    ;; (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-dakrone))
    ))

(add-to-list 'auto-mode-alist '(".*bash.*" . sh-mode))
(add-to-list 'auto-mode-alist '(".*zsh.*" . sh-mode))
(add-to-list 'auto-mode-alist '(".+[^bash|zsh|vim]rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".+login$" . sh-mode))
(add-to-list 'auto-mode-alist '(".+profile$" . sh-mode))
(add-to-list 'auto-mode-alist '(".+aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '(".+history$" . sh-mode))
(add-to-list 'auto-mode-alist '(".+env_vars" . sh-mode))

(add-to-list 'auto-mode-alist
             '(".+history$" . sh-mode)
             '(".+env_vars" . sh-mode))


(add-hook 'eshell-mode-hook
          (lambda ()
            (exec-path-from-shell-initialize)
            (setq show-trailing-whitespace nil)))

;; Sets the eshell prompt, based on my bash/zsh prompt

;; (eval-after-load 'esh
;;   '(progn
;;     (add-to-list 'eshell-visual-commands "ssh")))

;;; Calendar
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(use-package calendar
  :defer
  :config
  (setq calendar-latitude 37.813
        calendar-longitude -122.256
        calendar-location-name "Oakland, CA" ; Show my location to the internet
        calendar-mark-holidays-flag nil)) ; Show holidays

;;; ERC
(use-package erc
  :defer
  :config
  (progn
    (erc-autojoin-mode)
    (erc-track-mode)
    (setq erc-server "weber.freenode.net"
          erc-port 6667
          erc-nick "_sjs"
          erc-autojoin-timing 'ident
          erc-autojoin-channels-alist
          '((".*\\.freenode.net" "#emacs" "#racket" "#scheme" "#haskell" "#haskell-beginners" "#lisp" "#stackoverflow" "#programming" "#php" "#startups" "#drupal-support"))
          erc-track-exclude-types
          '("JOIN" "NICK" "PART" "QUIT" "MODE"
            "324" "329" "332" "333" "353" "477")
          erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))))
