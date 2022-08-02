;; custom test notation - needs to be after cider hook?
;; ((nil . ((cider-test-defining-forms . ("defspec" "deftest" "def-spec-test")))))

;;
(setq wh-project-root "/home/acron/projects/wh/works-hub")

;; fns
(defun wh-start-executor-cmd ()
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (insert "(do (require 'wh.executor.core) (wh.executor.core/start))")
    (execute-kbd-macro (kbd "<return>"))))

(defun wh-start-app-cmd ()
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (insert "(do (require 'wh.dev) (wh.dev/go))")
    (execute-kbd-macro (kbd "<return>"))))

(defun wh-clear-subs-cmd ()
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (insert "(do (require 're-frame.subs) (re-frame.subs/clear-subscription-cache!))")
    (execute-kbd-macro (kbd "<return>"))))

(defun wh-prt-fn (prefix)
  (interactive "sString for prefix: ")
  (insert "(defn prt [x] (println \"" prefix "\" x) x)")
  (cider-repl-return))

(defun wh-init-eshell ()
  (interactive)
  (insert "cd ")
  (insert wh-project-root)
  (execute-kbd-macro (kbd "<return>"))
  (insert "alias s 'grep -ri --exclude-dir target --exclude-dir .shadow-cljs --include \"*.clj*\" --include \"*.edn\" --include \"*.sass\" $*'")
  (execute-kbd-macro (kbd "<return>")))

(eval-after-load 'cider-repl
  '(progn
     (define-key cider-repl-mode-map (kbd "C-x w s") 'wh-start-app-cmd)
     (define-key cider-repl-mode-map (kbd "C-x w x") 'wh-start-executor-cmd)
     (define-key cider-repl-mode-map (kbd "C-x w c") 'wh-clear-subs-cmd)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-x w e") 'wh-init-eshell)))

;; done
(message "wh is loaded.")

