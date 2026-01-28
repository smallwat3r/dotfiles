;;; smallwat3r/claude/config.el -*- lexical-binding: t; -*-

(defun my/claude-notify (title message)
  "Display a Linux notification using notify-send."
  (when (and (featurep :system 'linux)
             (executable-find "notify-send"))
    (call-process "notify-send" nil nil nil title message)))

(defun my/claude-display-buffer-full-frame (buffer)
  "Display claude-code BUFFER in full frame."
  (display-buffer buffer '((display-buffer-full-frame))))

(use-package! claude-code
  :config
  (setq claude-code-terminal-backend 'eat
        claude-code-notification-function #'my/claude-notify
        claude-code-toggle-auto-select t
        claude-code-display-window-fn #'my/claude-display-buffer-full-frame)

  (defun my/claude-code-toggle ()
    "Show or hide the Claude window in full frame."
    (interactive)
    (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if claude-code-buffer
          (if (get-buffer-window claude-code-buffer)
              (delete-window (get-buffer-window claude-code-buffer))
            (let ((window (my/claude-display-buffer-full-frame claude-code-buffer)))
              (set-window-parameter window 'no-delete-other-windows
                                    claude-code-no-delete-other-windows)
              (when claude-code-toggle-auto-select
                (select-window window))))
        (claude-code--show-not-running-message))))

  (advice-add 'claude-code-toggle :override #'my/claude-code-toggle))
