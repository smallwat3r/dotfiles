;;; smallwat3r/debug/config.el -*- lexical-binding: t; -*-
;;
;; Debug utilities for Emacs development.

(defvar my-debug-ignored-commands
  '(self-insert-command
    next-line
    previous-line
    forward-char
    backward-char
    evil-forward-char
    evil-backward-char
    evil-next-line
    evil-previous-line
    evil-insert
    evil-normal-state
    mwheel-scroll
    ignore
    mouse-set-point)
  "Commands to ignore when echoing in debug mode.
These are typically high-frequency commands that would flood the echo area.")

(defun my/echo-command-name ()
  "Echo the current command name unless it's in the ignore list."
  (unless (memq this-command my-debug-ignored-commands)
    (message "%s" this-command)))

(define-minor-mode my-debug-mode
  "Global minor mode to echo executed command names.
Useful for discovering command names when creating keybindings or debugging."
  :init-value nil
  :lighter " Debug"
  :global t
  (if my-debug-mode
      (add-hook 'post-command-hook #'my/echo-command-name)
    (remove-hook 'post-command-hook #'my/echo-command-name)))
