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
  "Show where I'm at."
  (interactive)
  (message (kill-new (if (buffer-file-name)
                         (buffer-file-name)
                       (buffer-name)))))

;;;###autoload
(defun my/alacritty-here ()
  "Open alacritty from the current directory."
  (interactive "@")
  (shell-command
   (format "INSIDE_EMACS=alacritty alacritty --working-directory %S >/dev/null 2>&1 & disown"
           (if (buffer-file-name)
               (file-name-directory (buffer-file-name))
             "$HOME")))
  (message "Alacritty is ready!"))

;;;###autoload
(defun my/vertico-search-project-symbol-at-point (&optional arg)
  "Performs a live project search from the project root for the thing at point."
  (interactive)
  (+vertico/project-search arg (thing-at-point 'symbol)))
