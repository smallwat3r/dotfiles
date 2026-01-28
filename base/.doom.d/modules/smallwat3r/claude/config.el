;;; smallwat3r/claude/config.el -*- lexical-binding: t; -*-

(defun my/claude-notify (title message)
  "Display a Linux notification using notify-send."
  (when (and (featurep :system 'linux)
             (executable-find "notify-send"))
    (call-process "notify-send" nil nil nil title message)))

(use-package! claude-code
  :config
  (setq claude-code-terminal-backend 'eat
        claude-code-notification-function #'my/claude-notify))
