;; Load packages

(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mypackages '(monokai-theme ;; Add required packages you need here
		    ))

(dolist (p mypackages)
  (when (not (package-installed-p p))
    (package-install p)))
