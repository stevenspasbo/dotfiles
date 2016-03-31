;;;; spasbo.el - Load thems, packages, stuff I need before loading files from custom/ directory

(defun spasbo/load ()
  (require 'cl)
  (require 'package)
  (load-packages)
  (set-theme)
  (set-custom-dir))

(defun set-custom-dir ()
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(defun load-packages ()
  (setq mypackages
    '( ;; Stand alone
      rainbow-delimiters
      rainbow-mode
      paredit
      undo-tree
      beacon
      helm
      helm-swoop
      helm-flycheck
      magit
      boxquote
      git-timemachine
      esup
      avy
      exec-path-from-shell
      nyan-mode
      android-mode
      golden-ratio
      dash-at-point
      helm-descbinds
      helm-dash
      projectile
      helm-projectile
      dash
      yasnippet
      neotree
      diminish
      ;; Modes
      company
      robe
      flycheck
      flycheck-clojure
      flycheck-pos-tip
      racket-mode
      scheme-complete
      slime
      cl-lib
      haskell-mode
      ruby-electric
      inf-ruby
      yaml-mode
      clojure-mode
      cider
      clojure-mode-extra-font-locking
      clojure-cheatsheet
      tagedit
      python-mode
      hi2
      ghc
      scala-mode2
      sbt-mode
      php-mode
      vimrc-mode
      js2-mode
      smart-mode-line
      smart-mode-line-powerline-theme
      elm-mode
      elpy
      company-jedi
      alchemist
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
      gotham-theme
      farmhouse-theme
      afternoon-theme))

  (add-to-list 'package-pinned-packages '(alchemist . "melpa-stable") t)

  (setq package-archives
        '(("marmalade"   . "http://marmalade-repo.org/packages/")
          ("gnu"         . "http://elpa.gnu.org/packages/")
          ("org"         . "http://orgmode.org/elpa/")
          ("melpa"       . "http://melpa.milkbox.net/packages/")))

  (package-initialize) ; Activates all packages

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p mypackages)
;;    (message "Checking for %s" p)
    (unless (package-installed-p p)
      (message "Installing %s" p)
      (package-install p))))

(defun set-theme ()
  "Sets the theme depending on window-system"
  (interactive)
  ;; Add custom themes dir
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
  (cond ((string-equal window-system "ns") ; Emacs client settings
         ;; (load-theme 'farmhouse-dark t)
         (load-theme 'material t)
         (custom-set-faces '(default ((t (:height 130 :width normal :family "Fira Code"))))))
        ((string-equal window-system nil) ; Terminal settings
         ;; (load-theme 'afternoon t)
         (load-theme 'monokai t))))

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
