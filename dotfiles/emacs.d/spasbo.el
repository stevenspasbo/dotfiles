;;;; spasbo.el - Load thems, packages, stuff I need before loading files from custom/ directory

(package-initialize)

(require 'cl)
(require 'package)

(defun spasbo/load ()
  (load-packages)
  (set-theme)
  (set-custom-dir))

(defun set-custom-dir ()
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(defun load-packages ()
  (setq package-archives
        '(("marmalade"   . "http://marmalade-repo.org/packages/")
          ("gnu"         . "http://elpa.gnu.org/packages/")
          ("org"         . "http://orgmode.org/elpa/")
          ("melpa"       . "http://melpa.milkbox.net/packages/")))

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar mypackages '(
                       ;; Stand alone
                       rainbow-delimiters
                       rainbow-mode
                       paredit
                       undo-tree
                       helm
                       helm-swoop
                       magit
                       git-timemachine
                       exec-path-from-shell
                       nyan-mode
                       ;; Modes
                       racket-mode
                       scheme-complete
                       slime
                       cl-lib
                       haskell-mode
                       ruby-electric
                       inf-ruby
                       yaml-mode
                       cider
                       clojure-mode
                       clojure-mode-extra-font-locking
                       hi2
                       ghc
                       scala-mode2
                       sbt-mode
                       php-mode
                       vimrc-mode
                       js2-mode
                       ac-js2
                       ;; Themes
                       hipster-theme
                       moe-theme
                       zenburn-theme
                       ujelly-theme
                       tronesque-theme
                       tangotango-theme
                       color-theme-sanityinc-tomorrow
                       cyberpunk-theme
                       solarized-theme
                       monokai-theme
                       ))
  (dolist (p mypackages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Add custom themes dir
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))


(defun set-theme ()
  "Sets the theme depending on window-system"
  (interactive)
  (cond ((string-equal window-system "ns")
         (load-theme 'monokai t)
         (nyan-mode))
        ((string-equal window-system nil)
         (load-theme 'hipster t))))

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
