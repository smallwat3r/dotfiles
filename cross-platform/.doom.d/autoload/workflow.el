;;; $DOOMDIR/autoload/workflow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/scroll-up ()
  "Scroll up by 3 lines."
  (interactive)
  (evil-scroll-line-up 3))

;;;###autoload
(defun my/scroll-down ()
  "Scroll down by 3 lines."
  (interactive)
  (evil-scroll-line-down 3))

;;;###autoload
(defun my/enlarge-window-horizontally ()
  "Enlarge window horizontally by 5 chars."
  (interactive)
  (enlarge-window-horizontally 5))

;;;###autoload
(defun my/shrink-window-horizontally ()
  "Shrink window horizontally by 5 chars."
  (interactive)
  (shrink-window-horizontally 5))

;;;###autoload
(defun my/enlarge-window ()
  "Enlarge window by 5 chars."
  (interactive)
  (enlarge-window 5))

;;;###autoload
(defun my/shrink-window ()
  "Shrink window by 5 chars."
  (interactive)
  (shrink-window 5))

;;;###autoload
(defun my/find-file-in-dotfiles ()
  "Find file in my dotfiles."
  (interactive)
  (doom-project-find-file my-dotfiles-dir))

;;;###autoload
(defun my/where-am-i ()
  "Echo where I'm at."
  (interactive)
  (message (if (buffer-file-name)
               (buffer-file-name)
             (concat "buffer-name=" (buffer-name)))))

(defun alacritty-terminal-command ()
  (format "INSIDE_EMACS=alacritty alacritty --working-directory %S >/dev/null 2>&1 & disown"
          (if (buffer-file-name)
              (file-name-directory (buffer-file-name))
            "$HOME")))

(defun st-terminal-command ()
  (format "sh -c 'cd %S' ; INSIDE_EMACS=st st >/dev/null 2>&1 & disown"
          (if (buffer-file-name)
              (file-name-directory (buffer-file-name))
            "$HOME")))

;;;###autoload
(defun my/terminal-here ()
  "Open a terminal window in the current directory."
  (interactive "@")
  ;; Prefer st (Suckless Terminal) in Linux, else default to alacritty.
  (shell-command
   (if IS-LINUX
       (st-terminal-command)
     (alacritty-terminal-command))
   (message "Terminal is ready!"))

;;;###autoload
(defun my/vertico-search-project-symbol-at-point (&optional arg)
  "Performs a live project search from the project root for the thing at point."
  (interactive)
  (+vertico/project-search arg (thing-at-point 'symbol)))
