;;; smallwat3r/claude/autoload.el -*- lexical-binding: t; -*-

(defvar my-claude-cli-program "claude"
  "Claude CLI command.")

(defvar my-claude--buffer-name "*Claude*"
  "Name of the Claude buffer.")

;;;###autoload
(defun my/claude-chat ()
  "Open a Claude chat buffer in vterm."
  (interactive)
  (unless (executable-find "claude")
    (error "Claude CLI not found. Install it and run `claude login`"))
  (require 'vterm)
  (let ((buf (get-buffer my-claude--buffer-name)))
    (if (and buf (get-buffer-process buf))
        (pop-to-buffer buf)
      (let ((vterm-buffer-name my-claude--buffer-name)
            (vterm-shell my-claude-cli-program))
        (vterm)))))

;;;###autoload
(defun my/claude-send-region (start end)
  "Send the region from START to END to Claude."
  (interactive "r")
  (let ((buf (get-buffer my-claude--buffer-name)))
    (unless (and buf (get-buffer-process buf))
      (user-error "No active Claude session. Run `my/claude-chat' first"))
    (let ((text (buffer-substring-no-properties start end)))
      (with-current-buffer buf
        (vterm-send-string text))
      (pop-to-buffer buf))))
