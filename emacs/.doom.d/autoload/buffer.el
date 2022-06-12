;;; $DOOMDIR/autoload/buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/save-buffer ()
  "Save current buffer with confirmation."
  (interactive)
  (save-buffer)
  (message "Saved `%s'" (buffer-name)))

;;;###autoload
(defun my/kill-buffer ()
  "Kill current buffer."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    ;; If vterm is the major mode, disable confirmation for processes
    ;; and buffer modified.
    (set-buffer-modified-p nil)
    ;; Annoyingly every time I try to kill a vterm buffer it asks me
    ;; for confirmation as it has a running process. This allows me to
    ;; bypass this and kill it regardless.
    ;; NOTE: Doom is supposed to have a hook for this, setting
    ;; `confirm-kill-processes' to nil, but it doesn't seems to work
    ;; properly for some reason.
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (kill-this-buffer))

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
