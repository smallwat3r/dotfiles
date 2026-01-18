;;; smallwat3r/vterm-ext/autoload.el -*- lexical-binding: t; -*-

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
  "Return a list of SSH host aliases from the files in `my-ssh-config-files`."
  (let ((hosts '()))
    (dolist (file my-ssh-config-files)
      (setq file (expand-file-name file))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^[Hh]ost[ \t]+\\(.+\\)$" nil t)
            (let ((raw (match-string 1)))
              (dolist (h (split-string raw "[ \t]+" t))
                (unless (string-match-p "[*?]" h) ; skip wildcards
                  (push h hosts))))))))
    (delete-dups hosts)))

(defun my/terminal-ssh--command (host)
  "Build shell command to open an external terminal and SSH to HOST.
Sets INSIDE_EMACS so the remote shell knows it was launched from Emacs."
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

(defun my/zsh-history-candidates (&optional limit)
  "Return recent unique zsh history lines (most recent first)."
  (let* ((histfile (expand-file-name (or (getenv "HISTFILE") "~/.zsh_history")))
         (limit (or limit 10000))  ; hard limit
         ;; Linux: tac, fallback for macOS: tail -r
         ;; strip zsh timestamps, dedup keeping first (latest) occurrence
         (cmd (format
               "H=%s; [ -r \"$H\" ] || exit 0; \
(tac -- \"$H\" 2>/dev/null || tail -r -- \"$H\") \
| awk -F';' '{sub(/^: [0-9]+:[0-9]+;/, \"\"); if (length($0) && !seen[$0]++) print}' \
| head -n %d"
               (shell-quote-argument histfile) limit)))
    (split-string (shell-command-to-string cmd) "\n" t)))

;;;###autoload
(defun my/vterm-zsh-history-pick ()
  "Prompt from zsh history and insert into vterm (recency preserved)."
  (interactive)
  (let* ((history (my/zsh-history-candidates))
         ;; tell Emacs to keep given order
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata
                             (display-sort-function . identity)
                             (cycle-sort-function . identity))
                         (complete-with-action action history string pred))))
         (initial (or (thing-at-point 'symbol t) "")))
    (let ((choice (completing-read "zsh history: " collection nil nil initial)))
      (when (and (fboundp 'vterm-send-meta-backspace)
                 (thing-at-point 'symbol))
        (vterm-send-meta-backspace))
      (vterm-send-string choice))))

;;;###autoload
(defun my/open-remote-conn ()
  "Open remote SSH connection with Tramp."
  (interactive)
  (let ((tramp-path "/ssh:")
        (prompt "Pick target: "))
    (find-file (read-file-name prompt tramp-path))))
