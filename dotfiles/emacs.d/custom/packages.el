;; Load packages

(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mypackages '(
		     ;; Stand alone
		     neotree
		     rainbow-delimiters
		     rainbow-mode
		     paredit
		     ;; Modes
		     cl-lib
		     haskell-mode
		     ruby-electric
		     inf-ruby
		     yaml-mode
		     cider
		     clojure-mode
		     clojure-mode-extra-font-locking
		     dash
		     ;; Themes
		     hipster-theme
		     moe-theme
		     zenburn-theme
		     ujelly-theme
		     tronesque-theme
		     tangotango-theme
		     color-theme-sanityinc-tomorrow
		     cyberpunk-theme
		     ))

(dolist (p mypackages)
  (when (not (package-installed-p p))
    (package-install p)))
