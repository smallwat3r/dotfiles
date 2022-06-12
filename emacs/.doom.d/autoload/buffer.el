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
  ;; On specific major modes, disable confirmation of checking
  ;; running for processes and modified buffers.
  (when (derived-mode-p 'vterm-mode 'term-mode 'eshell-mode)
    (set-buffer-modified-p nil)
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
