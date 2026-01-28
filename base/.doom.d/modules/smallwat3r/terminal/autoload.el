;;; smallwat3r/terminal/autoload.el -*- lexical-binding: t; -*-

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
(defun my/open-remote-conn ()
  "Open remote SSH connection with Tramp."
  (interactive)
  (let ((tramp-path "/ssh:")
        (prompt "Pick target: "))
    (find-file (read-file-name prompt tramp-path))))

;;
;;; eat

(defun my/eat--project-root ()
  "Return the project root directory or `default-directory'.
For remote directories, returns the remote default-directory."
  (if (file-remote-p default-directory)
      default-directory
    (or (vc-root-dir)
        (and (fboundp 'projectile-project-root) (projectile-project-root))
        default-directory)))

(defun my/eat--buffer-for-dir (dir)
  "Return buffer name for eat in DIR."
  (if (file-remote-p dir)
      (format "*eat@%s*" (file-remote-p dir))
    (format "*eat:%s*" (abbreviate-file-name dir))))

(defun my/eat--get-buffer (dir)
  "Get or create an eat buffer for DIR."
  (require 'eat)
  (let ((buf-name (my/eat--buffer-for-dir dir)))
    (or (get-buffer buf-name)
        (my/eat--new-buffer dir buf-name))))

(defun my/eat--new-buffer (dir &optional name)
  "Create a new eat buffer for DIR with optional NAME."
  (require 'eat)
  (let* ((default-directory dir)
         (buf-name (or name (generate-new-buffer-name (my/eat--buffer-for-dir dir))))
         (program (funcall eat-default-shell-function))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (unless (and (bound-and-true-p eat-terminal)
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buffer buf-name "/usr/bin/env" nil
                  (list "sh" "-c" program))))
    buffer))

;;;###autoload
(defun my/eat/here (&optional here)
  "Open a new eat buffer at the project root, replacing the current buffer.
If HERE is non-nil, open at current buffer's directory.
For remote directories, opens a shell on the remote host."
  (interactive "P")
  (require 'eat)
  (let* ((dir (if here
                  (or (and buffer-file-name (file-name-directory buffer-file-name))
                      default-directory)
                (my/eat--project-root)))
         (buf (my/eat--new-buffer dir)))
    (switch-to-buffer buf)))

;;;###autoload
(defun my/eat/toggle (&optional here)
  "Toggle eat buffer visibility.
If HERE is non-nil, use buffer-specific directory.
For remote directories, opens a shell on the remote host."
  (interactive "P")
  (require 'eat)
  (let* ((dir (if here
                  (or (and buffer-file-name (file-name-directory buffer-file-name))
                      default-directory)
                (my/eat--project-root)))
         (buf-name (my/eat--buffer-for-dir dir))
         (buf (get-buffer buf-name)))
    (if-let ((win (and buf (get-buffer-window buf))))
        (delete-window win)
      (pop-to-buffer (my/eat--get-buffer dir)))))

;;;###autoload
(defun my/eat/here-current-buffer ()
  "Open an eat buffer from the current directory."
  (interactive)
  (my/eat/here t))

;;;###autoload
(defun my/eat/toggle-current-buffer ()
  "Toggle an eat buffer from the current directory."
  (interactive)
  (my/eat/toggle t))

;;;###autoload
(defun my/eat-zsh-history-pick ()
  "Prompt from zsh history and insert into eat (recency preserved)."
  (interactive)
  (require 'eat)
  (unless (bound-and-true-p eat-terminal)
    (user-error "No eat process in current buffer"))
  (let* ((history (my/zsh-history-candidates))
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata
                             (display-sort-function . identity)
                             (cycle-sort-function . identity))
                         (complete-with-action action history string pred))))
         (initial (or (thing-at-point 'symbol t) ""))
         (choice (completing-read "zsh history: " collection nil nil initial)))
    (when (thing-at-point 'symbol)
      (eat-term-send-string eat-terminal "\C-w"))
    (eat-term-send-string eat-terminal choice)))

;;;###autoload
(defun my/eat-interrupt ()
  "Send interrupt (C-c) to the eat terminal."
  (interactive)
  (require 'eat)
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\C-c")))
