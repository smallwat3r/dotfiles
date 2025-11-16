;;; $DOOMDIR/autoload/terminal.el -*- lexical-binding: t; -*-

(defun my/terminal-here--default-directory ()
  "Directory where the terminal should start."
  (or (when buffer-file-name
        (file-name-directory buffer-file-name))
      (expand-file-name "~")))

(defun my/terminal-here--pick-terminal ()
  "Pick which terminal to use for this system."
  (cond
   ((featurep :system 'macos) "alacritty")
   ((string-match-p
     "fedora"
     (downcase (or (doom-system-distro-version) "")))
    "foot")
   (t "st")))

(defun my/terminal-here--command ()
  "Build the shell command to launch the chosen terminal here."
  (let* ((term (my/terminal-here--pick-terminal))
         (dir  (my/terminal-here--default-directory)))
    (unless (executable-find term)
      (error "Executable '%s' not found in PATH" term))
    (format "sh -lc 'cd %s && INSIDE_EMACS=%s %s' >/dev/null 2>&1"
            (shell-quote-argument dir) term term)))

;;;###autoload
(defun my/terminal-here ()
  "Open a terminal window in the current directory."
  (interactive "@")
  (start-process-shell-command
   "terminal-here" nil
   (my/terminal-here--command))
  (message "Terminal is ready!"))

;;;###autoload
(defun my/vterm/toggle-current-buffer ()
  "Toggles a vterm buffer from the current directory."
  (interactive)
  (+vterm/toggle t))

;;;###autoload
(defun my/vterm/here-current-buffer ()
  "Open a vterm buffer from the current directory."
  (interactive)
  (+vterm/here t))

(defun my/ssh-config-hosts ()
  "Return a list of SSH host aliases from ~/.ssh/config."
  (let* ((file (expand-file-name "~/.ssh/config"))
         (hosts '()))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                ;; Match: Host foo bar baz
                "^[Hh]ost[ \t]+\\(.+\\)$" nil t)
          (let ((raw (match-string 1)))
            ;; Split on whitespace
            (dolist (h (split-string raw "[ \t]+" t))
              ;; Ignore wildcards (* ?)
              (unless (string-match-p "[*?]" h)
                (push h hosts)))))))
    (delete-dups hosts)))

(defun my/terminal-ssh--command (host)
  "Build the shell command to launch the chosen terminal and SSH to HOST."
  (let* ((term (my/terminal-here--pick-terminal))
         (extra-flags (if (string= term "foot") "-t xterm-256color" ""))
         (ssh-cmd (format "ssh %s" (shell-quote-argument host))))
    (unless (executable-find term)
      (error "Executable '%s' not found in PATH" term))
    (format "INSIDE_EMACS=1 %s %s -e sh -lc %s"
            term
            extra-flags
            (shell-quote-argument ssh-cmd))))

;;;###autoload
(defun my/ssh-external (host)
  "Open an external terminal and SSH to HOST."
  (interactive
   (list (completing-read "SSH target: "
                          (my/ssh-config-hosts)
                          nil nil)))
  (let ((cmd (my/terminal-ssh--command host)))
    (message "my/ssh-external running: %s" cmd)
    (start-process-shell-command
     "my-ssh-external" nil cmd)))
