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
		    ))

(dolist (p mypackages)
  (when (not (package-installed-p p))
    (package-install p)))
