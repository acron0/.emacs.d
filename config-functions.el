;; -*- lexical-binding: t; -*-

;; Platform-aware shell configuration
(when (eq system-type 'windows-nt)
  (let ((git-bash "C:/Program Files/Git/bin/bash.exe"))
    (when (file-executable-p git-bash)
      (setq shell-file-name git-bash)
      (setq shell-command-switch "-c")
      (setq explicit-shell-file-name git-bash)
      (setenv "SHELL" git-bash))))

(defvar notes-directory "~/notes"
  "Root directory for notes. Used by `open-daily', `show-todos', `search-notes', etc.")

;; https://stackoverflow.com/a/25471300/254190
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
				; set-window-dedicated-p returns FLAG that was passed as
				; second argument, thus can be used as COND for if:
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

;; https://github.com/rakanalh/dotemacs/blob/master/config/config-functions.el
(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun open-daily ()
  "Open today's daily note in ~/notes/Dailies/<YYYY-MM-DD>.org."
  (interactive)
  (let ((filepath (expand-file-name (format-time-string "%Y-%m-%d.org")
                                    (expand-file-name "Dailies" notes-directory))))
    (find-file filepath)
    (when (= (buffer-size) 0)
      (insert (format-time-string "* %Y-%m-%d - Daily Notes\n")))))

(defvar-local dailies--saved-window-config nil
  "Window configuration saved before entering the dailies view.")

(defun dailies--current-date ()
  "Return the date string on the current line, or nil."
  (unless (eobp)
    (string-trim (thing-at-point 'line t))))

(defun dailies--update-preview ()
  "Update *Dailies Preview* with the highlighted date's note."
  (when (string= (buffer-name) "*Dailies*")
    (let* ((date (dailies--current-date))
           (filepath (when date
                       (expand-file-name (concat date ".org")
                                         (expand-file-name "Dailies" notes-directory))))
           (preview-buf (get-buffer-create "*Dailies Preview*")))
      (with-current-buffer preview-buf
        (unless (eq major-mode 'org-mode)
          (org-mode)
          (read-only-mode 1))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (and filepath (file-exists-p filepath))
              (insert-file-contents filepath)
            (insert (format "No note for %s\n" (or date "?")))))))))

(defun dailies--quit ()
  "Quit the dailies view, restoring the previous window configuration."
  (interactive)
  (let ((saved-config dailies--saved-window-config))
    (kill-buffer "*Dailies*")
    (when saved-config
      (set-window-configuration saved-config))))

(defun dailies--open-current ()
  "Open the daily note for the current date, restoring windows first."
  (interactive)
  (when-let ((date (dailies--current-date)))
    (let ((filepath (expand-file-name (concat date ".org")
                                      (expand-file-name "Dailies" notes-directory)))
          (saved-config dailies--saved-window-config))
      (kill-buffer "*Dailies*")
      (when saved-config
        (set-window-configuration saved-config))
      (find-file filepath))))

(defun show-dailies ()
  "Show a navigable list of daily notes with a live preview."
  (interactive)
  (let* ((dailies-dir (expand-file-name "Dailies" notes-directory))
         (dates (when (file-directory-p dailies-dir)
                  (sort (mapcar #'file-name-base
                                (directory-files dailies-dir nil
                                                 "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
                        #'string>)))
         (list-buf (get-buffer-create "*Dailies*")))
    (with-current-buffer list-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if dates
            (dolist (d dates) (insert d "\n"))
          (insert "No daily notes found.\n")))
      (goto-char (point-min))
      (hl-line-mode 1)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "n") #'next-line)
        (define-key map (kbd "p") #'previous-line)
        (define-key map (kbd "j") #'next-line)
        (define-key map (kbd "k") #'previous-line)
        (define-key map (kbd "RET") #'dailies--open-current)
        (define-key map (kbd "q") #'dailies--quit)
        (use-local-map map))
      (add-hook 'post-command-hook #'dailies--update-preview nil t)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when-let ((w (get-buffer-window "*Dailies Preview*")))
                    (delete-window w))
                  (when (get-buffer "*Dailies Preview*")
                    (kill-buffer "*Dailies Preview*")))
                nil t)
      (setq-local dailies--saved-window-config (current-window-configuration))
      (read-only-mode 1))
    (delete-other-windows)
    (switch-to-buffer list-buf)
    (split-window-right 22)
    (set-window-buffer (next-window) (get-buffer-create "*Dailies Preview*"))
    (dailies--update-preview)))

(defun todos--shell-cmd (script paths &optional flags)
  "Run SCRIPT with PATHS expanded and optional FLAGS passed as-is."
  (let* ((expanded-paths (mapcar #'expand-file-name paths))
         (output (shell-command-to-string
                  (concat
                   (shell-quote-argument (expand-file-name script))
                   " "
                   (mapconcat #'shell-quote-argument expanded-paths " ")
                   (if flags (concat " " flags) "")))))
    ;; Git bash strips drive letters from Windows paths (C:/Users/... -> /Users/...).
    ;; Restore them by replacing the shell output prefix with the original path.
    (when (eq system-type 'windows-nt)
      (dolist (p expanded-paths)
        (let ((unix-path (replace-regexp-in-string "^[a-zA-Z]:" "" p)))
          (setq output (replace-regexp-in-string
                        (regexp-quote unix-path) p output t t)))))
    output))

(defun todos--open-at-checkbox ()
  "Open the linked file and move the cursor to the todo's checkbox."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" line)
      (let ((file (match-string 1 line))
            (todo-text (match-string 2 line)))
        (find-file-other-window file)
        (goto-char (point-min))
        (when (search-forward todo-text nil t)
          (beginning-of-line))))))

(defun todos--toggle-in-source ()
  "Toggle the checkbox in the source file for the current todo line."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" line)
      (let ((file (match-string 1 line))
            (todo-text (match-string 2 line))
            (checked (string-match-p "\\[X\\]" line)))
        ;; update source file
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (when (search-forward todo-text nil t)
              (beginning-of-line)
              (if checked
                  (when (re-search-forward "\\[X\\]" (line-end-position) t)
                    (replace-match "[ ]"))
                (when (re-search-forward "\\[ \\]" (line-end-position) t)
                  (replace-match "[X]")))
              (save-buffer))))
        ;; update the todos buffer line
        (let ((inhibit-read-only t))
          (save-excursion
            (beginning-of-line)
            (if checked
                (when (re-search-forward "\\[X\\]" (line-end-position) t)
                  (replace-match "[ ]"))
              (when (re-search-forward "\\[ \\]" (line-end-position) t)
                (replace-match "[X]")))))))))

(defun todos--refresh ()
  "Regenerate the *Todos* buffer contents, preserving cursor position."
  (interactive)
  (when (string= (buffer-name) "*Todos*")
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (todos--shell-cmd
               (expand-file-name "Scripts/generate-todos.sh" notes-directory)
               (list notes-directory) "--org"))
      (goto-char (min pos (point-max))))))

(defun show-todos ()
  "Display todos from notes in a dedicated org buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Todos*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (todos--shell-cmd
                 (expand-file-name "Scripts/generate-todos.sh" notes-directory)
                 (list notes-directory) "--org")))
      (org-mode)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "C-c C-c") 'todos--toggle-in-source)
        (define-key map (kbd "g") 'todos--refresh)
        (define-key map (kbd "RET") 'todos--open-at-checkbox)
        (use-local-map map))
      (add-hook 'window-buffer-change-functions
                (lambda (_) (todos--refresh)) nil t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun new-meeting ()
  "Create a new meeting note from the Meeting template."
  (interactive)
  (let* ((title (read-string "Meeting title: "))
         (date (format-time-string "%Y-%m-%d"))
         (time (format-time-string "%H:%M"))
         (filename (concat date " - " title ".org"))
         (filepath (expand-file-name filename (expand-file-name "Work/Meetings" notes-directory)))
         (template (expand-file-name "Meeting.org" (expand-file-name "Templates" notes-directory))))
    (unless (file-exists-p filepath)
      (copy-file template filepath)
      (with-current-buffer (find-file-noselect filepath)
        (goto-char (point-min))
        (while (search-forward "{{date}}" nil t)
          (replace-match date t t))
        (goto-char (point-min))
        (while (search-forward "{{time}}" nil t)
          (replace-match time t t))
        (save-buffer)))
    (find-file filepath)))

(defun search-notes (query)
  "Search notes with rg, displaying results in a grep-mode buffer.
Searches `notes-directory' by default."
  (interactive "sSearch notes: ")
  (grep (format "rg --no-heading --line-number --color=auto --ignore-case %s %s"
                (shell-quote-argument query)
                (shell-quote-argument (expand-file-name notes-directory)))))

(provide 'config-functions)
