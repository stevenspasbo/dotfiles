;;;; Sets up clojure-mode

;; Clojure
(require 'clojure-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
	    ;; A little more syntax highlighting
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
            (setq nrepl-hide-special-buffers t)))

;;;; Cider
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
