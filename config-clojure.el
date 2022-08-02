(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clj-refactor)
(straight-use-package 'lsp-mode)

(defun clojure-mode-config ()
  (prettify-symbols-mode +1)
  (projectile-mode +1)
  (clj-refactor-mode +1)
  (rainbow-delimiters-mode +1)
  (yas-minor-mode +1)
  (lsp +1)

  ;; style
  (setq tab-width 2)
  (setq clojure-indent-style :align-arguments)

  ;; cider
  (setq cider-repl-history-file (concat user-emacs-directory "cider-history")
	cider-repl-history-size 1000)

  ;; clojure mode alignment
  (setq clojure-align-forms-automatically t)
  (dolist (m '("mlet"
	       "prom/mlet"
	       "if-mlet"
	       "prom/if-mlet"
	       "when-mlet"
	       "prom/when-mlet"))
    (add-to-list 'clojure-align-binding-forms m))

  ;; lambda symbol
  (push '("#" . ?λ) prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate
	      (lambda (begin end match)
                (and (prettify-symbols-default-compose-p begin end match)
                     (or (not (equal match "#")) (eq (char-after end) ?\()))))

  ;; keys
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; wh style (0 = clj align)
  (put-clojure-indent '-> 0)
  (put-clojure-indent '->> 0)
  (put-clojure-indent ':import 0)
  (put-clojure-indent ':require 0)
  (put-clojure-indent ':require-macros 0)
  (put-clojure-indent 'and 0)
  (put-clojure-indent 'as-> 0)
  (put-clojure-indent 'as->> 0)
  (put-clojure-indent 'assoc 0)
  (put-clojure-indent 'asynchronously 0)
  (put-clojure-indent 'cond-> 0)
  (put-clojure-indent 'cond->> 0)
  (put-clojure-indent 'def-query-from-template 0)
  (put-clojure-indent 'def-spec-test 0)
  (put-clojure-indent 'defroutes 0)
  (put-clojure-indent 'fail 0)
  (put-clojure-indent 'hash-map 0)
  (put-clojure-indent 'html/deftemplate 0)
  (put-clojure-indent 'i-util/run-async 1)
  (put-clojure-indent 'if-mlet 1)
  (put-clojure-indent 'mapv 0)
  (put-clojure-indent 'mdo 0)
  (put-clojure-indent 'merge 0)
  (put-clojure-indent 'merge 0)
  (put-clojure-indent 'prom/mlet 1)
  (put-clojure-indent 'prom/if-mlet 1)
  (put-clojure-indent 'prom/when-mlet 1)
  (put-clojure-indent 'reg-event-db 0)
  (put-clojure-indent 'reg-event-fx 0)
  (put-clojure-indent 'reg-fx 0)
  (put-clojure-indent 'reg-sub 0)
  (put-clojure-indent 'reg-sub-raw 0)
  (put-clojure-indent 's/fdef 1)
  (put-clojure-indent 'letsub 1))

;; modes
(add-hook 'clojure-mode-hook
          'clojure-mode-config)

(add-hook 'clojurescript-mode-hook
          'clojure-mode-config)

(add-hook 'clojurec-mode-hook
          'clojure-mode-config)

(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'clojure-mode)
              (clojure-sort-ns))))

(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'clojurescript-mode)
              (clojure-sort-ns))))

(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'clojurec-mode)
              (clojure-sort-ns))))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (when 1 ;; (string-equal system-type "darwin") ; Mac OS X
              (call-process-shell-command "say \"It's time to kick ass and chew bubble gum\" &" nil 0))))

;; set up lsp
(add-hook 'lsp-mode-hook
          (lambda ()
            (dolist (m '(clojure-mode
                         clojurec-mode
                         clojurescript-mode))
              (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
            (setq lsp-enable-indentation nil
                  lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))))

(eval-after-load 'cider
  '(progn
     (define-key cider-mode-map (kbd "C-c ,")   'cider-test-run-test)
     (define-key cider-mode-map (kbd "C-c C-,") 'cider-test-run-project-tests)
     (define-key cider-mode-map (kbd "C-c C-q") 'cider-quit)
     (push '"def-spec-test" cider-test-defining-forms)))

(eval-after-load 'cider-repl
  '(progn
     (define-key cider-repl-mode-map (kbd "C-c #")   'cider-repl-clear-buffer)
     (define-key cider-repl-mode-map (kbd "C-c M-p") 'cider-repl-reset)))


