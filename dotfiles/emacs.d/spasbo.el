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
  (package-initialize) ; Activates all packages

  (setq package-archives
        '(("marmalade"   . "http://marmalade-repo.org/packages/")
          ("gnu"         . "http://elpa.gnu.org/packages/")
          ("org"         . "http://orgmode.org/elpa/")
          ("melpa"       . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))

  (add-to-list 'package-pinned-packages '(alchemist . "melpa-stable") t)

  (let ((my-packages '(
      rainbow-delimiters
      rainbow-mode
      paredit
      undo-tree
      helm
      helm-swoop
      helm-flycheck
      helm-descbinds
      helm-dash
      projectile
      helm-projectile
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
      afternoon-theme
      persistent-scratch))
      (refreshed? nil))
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (when (null refreshed?)
        (package-refresh-contents)
        (setq refreshed? t))
      (package-install p)))))

(defun set-theme ()
  "Sets the theme depending on window-system"
  (cond ((string-equal window-system "ns") ; Emacs client settings
         (load-theme 'material t))
        ((string-equal window-system nil) ; Terminal settings
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
