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
        claude-code-display-window-fn #'my/claude-display-buffer-full-frame))
