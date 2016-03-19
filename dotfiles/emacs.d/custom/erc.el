(defun erc-mode-stuff ()
  (require 'erc)

  (setq erc-echo-notices-in-minibuffer-flag t
        erc-server "weber.freenode.net"
        erc-port 6667
        erc-nick "_sjs"
        erc-autojoin-timing 'ident
        erc-autojoin-channels-alist
        '((".*\\.freenode.net" "#emacs" "#racket" "#scheme" "#haskell" "#haskell-beginners" "#lisp" "#stackoverflow" "#programming" "#php" "#startups" "#drupal-support"))
        erc-track-exclude-types
        '("JOIN" "NICK" "PART" "QUIT" "MODE"
        "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  (erc-autojoin-mode t)
  (erc-track-mode t))

(add-hook 'erc-mode-hook 'erc-mode-stuff)
