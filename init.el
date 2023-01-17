;; Disable package.el in favor of straight.el; emacs >= 27
(setq package-enable-at-startup nil)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; packages
;; - melpa etc
(straight-use-package 'selectrum)
(straight-use-package 'ctrlf)
(straight-use-package 'doom-themes)
(straight-use-package 'aggressive-indent)
(straight-use-package 'nyan-mode)
(straight-use-package 'auto-dim-other-buffers)
(straight-use-package 'consult)
(straight-use-package 'magit)
(straight-use-package 'smartparens)
(straight-use-package 'desktop)
(straight-use-package 'undo-tree)
(straight-use-package 'projectile)
(straight-use-package 'rainbow-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'idle-highlight-mode)
(straight-use-package 'company)
(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'switch-window)
(straight-use-package 'deadgrep)
(straight-use-package 'super-save)
(straight-use-package 'ob-mongo)
(straight-use-package 'easy-kill)
(straight-use-package 'helm)

;; - github
(straight-use-package
 '(emacs-treeview :type git :host github :repo "tilmanrassy/emacs-treeview"))
(straight-use-package
 '(emacs-dir-treeview :type git :host github :repo "tilmanrassy/emacs-dir-treeview"))
(straight-use-package
 '(beacon :type git :host github :repo "Malabarba/beacon"))

;; initialisations
(recentf-mode +1)
(selectrum-mode +1)
(ctrlf-mode +1)
(aggressive-indent-global-mode +1)
(nyan-mode +1)
(auto-dim-other-buffers-mode +1)
(smartparens-global-strict-mode +1)
(global-undo-tree-mode)
(rainbow-mode +1)
(global-idle-highlight-mode +1)
(delete-selection-mode +1)
(beacon-mode +1)
(desktop-save-mode +1)

;; other config
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq desktop-base-file-name ".desktop")
(setq desktop-base-lock-name ".desktop.lock")
(setq desktop-restore-eager 8)

;; load other files
(load "~/.emacs.d/config-functions.el")
(load "~/.emacs.d/config-clojure.el")
(load "~/.emacs.d/config-workshub.el")
;;
(load "~/.emacs.d/lib/ob-mongo.el")
(load "~/.emacs.d/lib/helm-swoop.el")
(require 'helm-swoop)

;; theme config
(load-theme 'doom-dracula +1)
(load-theme 'dir-treeview-pleasant +1)
;;
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; font config
(cond 
 ;; Microsoft Windows
 ((string-equal system-type "windows-nt")
  (progn
    (message "Microsoft Windows")))
 ;; Mac OS X
 ((string-equal system-type "darwin")
  (progn
    (message "Mac OS X")
    (set-frame-font "Inconsolata-13")))
 ;; Linux
 ((string-equal system-type "gnu/linux")
  (progn
    (message "Linux")
    (set-frame-font "Inconsolata-11"))))

;; late config / hooks
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; None of this seems to work
	    ;; (set-face-attribute  'hl-line-face nil :underline t)
            ;; (set-face-background 'hl-line-face "#426")
	    ;; (set-face-background 'highlight "#222")
	    ;; (set-face-foreground 'highlight nil)
	    ;; (set-face-underline-p 'highlight t)
	    ))

(add-hook 'elisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters +1)))

(add-hook 'dired-mode-hook
          'all-the-icons-dired-mode)

;; supposed to remove the flymake errors...
;; DOESN'T WORK
(remove-hook 'flymake-diagnostic-functions
	     'flymake-proc-legacy-flymake)

;; keybindings
;; - global
(global-set-key (kbd "C-x m")   'eshell)
(global-set-key (kbd "C-c f")   'consult-recent-file)
(global-set-key (kbd "C-c d")   'toggle-window-dedicated)
(global-set-key (kbd "C-x p")   'window-swap-states)
(global-set-key (kbd "C-x .")   'lsp-find-references)
(global-set-key (kbd "C-x 2")   'split-window-below-and-switch)
(global-set-key (kbd "C-x 3")   'split-window-right-and-switch)
(global-set-key (kbd "C-x o")   'switch-window)
(global-set-key (kbd "C-x f")   'consult-imenu)
(global-set-key (kbd "C-M-SPC") 'easy-mark)
(global-set-key (kbd "C-x /")   'helm-swoop)

(define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<left>")  'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<up>")    'sp-raise-sexp)
(define-key smartparens-mode-map (kbd "M-r")         'sp-raise-sexp)
(define-key smartparens-mode-map (kbd "M-s")         'sp-unwrap-sexp)

;; fix the risky variable eternal query
;; https://emacs.stackexchange.com/questions/10983/remember-permission-to-execute-risky-local-variables
(defun risky-local-variable-p (sym &optional _ignored) nil)

;; on startup
(desktop-read)
;; open tree view
;; (dir-treeview-open "~/projects")
;; (toggle-window-dedicated)
;; (split-window-right 30)o
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" default))
 '(safe-local-variable-values
   '((cider-test-defining-forms "defspec" "deftest" "def-spec-test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
