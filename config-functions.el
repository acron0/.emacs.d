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
  (when (string= (buffer-name) "*Todos*")
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (shell-command-to-string (format "%s/Scripts/generate-todos.sh %s --org"
                                          (expand-file-name notes-directory)
                                          (shell-quote-argument (expand-file-name notes-directory)))))
      (goto-char (min pos (point-max))))))

(defun show-todos ()
  "Display todos from notes in a dedicated org buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Todos*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string (format "%s/Scripts/generate-todos.sh %s --org"
                                          (expand-file-name notes-directory)
                                          (shell-quote-argument (expand-file-name notes-directory))))))
      (org-mode)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "C-c C-c") 'todos--toggle-in-source)
        (define-key map (kbd "g") 'todos--refresh)
        (use-local-map map))
      (add-hook 'window-buffer-change-functions
                (lambda (_) (todos--refresh)) nil t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun open-meeting ()
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
  (grep (format "rg --no-heading --line-number --color=auto %s %s"
                (shell-quote-argument query)
                (shell-quote-argument (expand-file-name notes-directory)))))

(provide 'config-functions)
