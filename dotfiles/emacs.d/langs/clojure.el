;;;; Sets up clojure-mode

;; Clojure

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

(defun clojure-stuff ()
  (require 'clojure-mode)
  (require 'clojure-mode-extra-font-locking)

  (setq inferior-lisp-program "lein repl")
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))
  (define-clojure-indent (fact 1))
  (define-clojure-indent (facts 1))
  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (subword-mode)
  (enable-paredit-mode)
  (eldoc-mode)
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t))

(add-hook 'clojure-mode-hook 'clojure-stuff)

;;;; Cider
(defun cider-mode-stuff ()
  ;; provides minibuffer documentation for the code you're typing into the repl
  (cider-turn-on-eldoc-mode)
  ;; enable paredit in your REPL
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (setq
   ;; go right to the REPL buffer when it's finished connecting
   cider-repl-pop-to-buffer-on-connect t
   ;; When there's a cider error, show its buffer and switch to it
   cider-show-error-buffer t
   cider-auto-select-error-buffer t
   ;; Where to store the cider history.
   cider-repl-history-file "~/.emacs.d/cider-history"
   ;; Wrap when navigating history.
   cider-repl-wrap-history t))

(add-hook 'cider-mode-hook 'cider-mode-stuff)

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))
