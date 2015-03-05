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
		     haskell-mode
		     inf-ruby
		     neotree
		     rainbow-mode
<<<<<<< HEAD
		     cider
		     clojure-mode
		     clojure-mode-extra-font-locking
		     dash
		     rainbow-delimiters
		     cl-lib
		     paredit
		     ))
=======
		     yaml-mode
		    ))
>>>>>>> 210aad8c4a08c61ff9eb341c627fe5e2605bdfa1

(dolist (p mypackages)
  (when (not (package-installed-p p))
    (package-install p)))
