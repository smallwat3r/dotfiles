;;; $DOOMDIR/autoload/smallwat3r.el -*- lexical-binding: t; -*-

;;
;;; Python

(defun my/venv--locate-python-path ()
  "Look for the closest Python virtual environments in the workspace."
  (when-let (venv-base-directory (locate-dominating-file default-directory "env/"))
    (concat venv-base-directory "env")))

(defun my/venv--locate-python-executable ()
  "Look for a Python executable from the closest virtual environment."
  (when-let (venv-path (my/venv--locate-python-path))
    (executable-find (f-expand "bin/python" venv-path))))

;;;###autoload
(defun my/deactivate-python-venv ()
  "Deactivate Python virtual environment."
  (interactive)
  (pyvenv-deactivate)
  (message "Venv deactivated"))

;;;###autoload
(defun my/activate-closest-python-venv ()
  "Activate the closest Python virtual environment."
  (interactive)
  (if-let (venv-path-python (my/venv--locate-python-path))
      (let ((venv-path venv-path-python))
        (pyvenv-activate venv-path)
        (message "Activated venv `%s'" venv-path))
    (message "Couldn't find any available venv")))

;;;###autoload
(defun my/open-python-repl ()
  "Open Python repl from the closest virtual environment or default to local install."
  (interactive)
  (require 'python)
  (pop-to-buffer
   (process-buffer
    (let ((python-shell-interpreter
           (or (my/venv--locate-python-executable)
               "python3")))
      (message "Python repl has been loaded from `%s'" python-shell-interpreter)
      (run-python nil nil t)))))


;;
;;; Navigation

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


;;
;;; Buffers

;;;###autoload
(defun my/save-buffer ()
  "Save current buffer with confirmation."
  (interactive)
  (save-buffer)
  (message "Saved `%s'" (buffer-name)))

;;;###autoload
(defun my/save-and-close-buffer ()
  "Save and close current buffer with confirmation."
  (interactive)
  (save-buffer)
  (message "Closed and saved `%s'" (buffer-name))
  (kill-this-buffer))

;;;###autoload
(defun my/count-buffers ()
  "Display the current number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (message "%d buffers in total" buf-count)))

;;;###autoload
(defun my/add-scratch-buffer-header (text)
  "Open scratch buffer with a TEXT header."
  (when scratch-buffer
    (save-excursion
      (goto-char (point-min))
      (insert text)
      (newline 2))
    (goto-char (point-max))))

;;;###autoload
(defun my/scratch-rest-mode ()
  "Open a scratch buffer with restclient."
  (interactive)
  (scratch 'restclient-mode))


;;
;;; Misc

;;;###autoload
(defun my/find-file-in-dotfiles ()
  "Find file in my dotfiles."
  (interactive)
  (doom-project-find-file my-dotfiles-dir))

;;;###autoload
(defun my/where-am-i ()
  "Show where I'm at."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))

;;;###autoload
(defun my/vterm/toggle-current-buffer ()
  "Toggles a terminal popup window to the directory of the current buffer."
  (interactive)
  (+vterm/toggle t))

;;;###autoload
(defun my/vterm/here-current-buffer ()
  "Open a terminal buffer in the current window to the directory of the current buffer."
  (interactive)
  (+vterm/here t))

;;;###autoload
(defun my/alacritty-here ()
  "Open alacritty from the current directory."
  (interactive "@")
  (shell-command
   (format "alacritty --working-directory %S >/dev/null 2>&1 & disown"
           default-directory)))
