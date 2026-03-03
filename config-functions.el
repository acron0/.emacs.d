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
  "Open today's daily note in ~/notes/Dailies/<YYYY-MM-DD>.md."
  (interactive)
  (find-file (expand-file-name (format-time-string "%Y-%m-%d.org")
                               "~/notes/Dailies")))

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

(defun show-todos ()
  "Display todos from notes in a dedicated org buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Todos*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string "~/notes/Scripts/generate-todos.sh ~/notes --org")))
      (org-mode)
      (local-set-key (kbd "C-c C-c") 'todos--toggle-in-source)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(provide 'config-functions)
