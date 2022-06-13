;;; $DOOMDIR/autoload/buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/save-buffer ()
  "Save current buffer and display a confirmation message."
  (interactive)
  (save-buffer)
  (message "Saved `%s'" (buffer-name)))

;;;###autoload
(defun my/save-and-close-buffer ()
  "Save, close current buffer and display a confirmation message."
  (interactive)
  (save-buffer)
  (message "Closed and saved `%s'" (buffer-name))
  (kill-this-buffer))

;;;###autoload
(defun my/kill-buffer ()
  "Kill current buffer."
  (interactive)
  ;; On some specific major modes, when killing a buffer, disable prompt
  ;; confirmation of checking running for processes and modified buffers.
  (when (derived-mode-p 'vterm-mode 'term-mode 'eshell-mode)
    (set-buffer-modified-p nil)
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
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
  "Toggles a vterm buffer from the current directory."
  (interactive)
  (+vterm/toggle t))

;;;###autoload
(defun my/vterm/here-current-buffer ()
  "Open a vterm buffer from the current directory."
  (interactive)
  (+vterm/here t))
