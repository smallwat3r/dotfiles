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
    (if (string= term "alacritty")
        ;; Alacritty has a native working-directory flag
        (format "INSIDE_EMACS=%s %s --working-directory %S >/dev/null 2>&1"
                term term dir)
      ;; foot / st (and similar) just run inside a shell
      (format "sh -lc 'cd %s && INSIDE_EMACS=%s %s' >/dev/null 2>&1"
              (shell-quote-argument dir) term term))))

;;;###autoload
(defun my/terminal-here ()
  "Open a terminal window in the current directory."
  (interactive "@")
  (shell-command (my/terminal-here--command))
  (message "Terminal is ready!"))
