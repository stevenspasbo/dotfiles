(erc-autojoin-mode t)
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
(erc-track-mode t)

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server erc-server :port erc-port :nick erc-nick))))
