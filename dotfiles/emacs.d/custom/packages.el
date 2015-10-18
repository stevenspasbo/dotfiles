;;;; Load packages

(require 'package)

(setq package-archives
      '(("marmalade"   . "http://marmalade-repo.org/packages/")
	("gnu"         . "http://elpa.gnu.org/packages/")
	("org"         . "http://orgmode.org/elpa/")
	("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mypackages '(
		     ;; Stand alone
		     neotree
		     rainbow-delimiters
		     rainbow-mode
		     paredit
		     flycheck
		     flyspell
		     auto-complete
                     undo-tree
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
		     dash
		     hi2
		     ghc
		     scala-mode2
		     sbt-mode
		     php-mode
                     vimrc-mode
		     ;; Themes
		     hipster-theme
		     moe-theme
		     zenburn-theme
		     ujelly-theme
		     tronesque-theme
		     tangotango-theme
		     color-theme-sanityinc-tomorrow
		     cyberpunk-theme
		     js2-mode
		     ac-js2
		     solarized-theme
                     monokai-theme
		     ))

(dolist (p mypackages)
  (when (not (package-installed-p p))
    (package-install p)))
