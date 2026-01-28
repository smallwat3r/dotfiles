;;; $DOOMDIR/autoload/buffer.el -*- lexical-binding: t; -*-
;;
;; Buffer management helpers.

;;;###autoload
(defun my/save-and-close-buffer ()
  "Save, close current buffer and display a confirmation message."
  (interactive)
  (save-buffer)
  (message "Closed and saved: %s" (buffer-name))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun my/kill-buffer (&optional buffer)
  "Kill current buffer or BUFFER without prompts for term/eat/eshell.
Also closes the window if it was a split."
  (interactive
   (list (when current-prefix-arg
           (read-buffer "Kill buffer: " (current-buffer) t))))
  (let* ((buf (or buffer (current-buffer)))
         (bufname (buffer-name buf))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (when (derived-mode-p 'eat-mode 'term-mode 'eshell-mode)
        (set-buffer-modified-p nil)
        (when-let ((proc (get-buffer-process buf)))
          (set-process-query-on-exit-flag proc nil))))
    (kill-buffer buf)
    (when (and win (not (one-window-p)))
      (delete-window win))
    (message "Killed buffer: %s" bufname)))

;;;###autoload
(defun my/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'my/kill-buffer (buffer-list))
  (message "Killed all buffers"))

;;;###autoload
(defun my/kill-all-buffers-except-current ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'my/kill-buffer (delete (current-buffer) (buffer-list)))
  (message "Killed all buffers except: %s" (current-buffer)))
