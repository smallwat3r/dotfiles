;;; smallwat3r/claude/autoload.el -*- lexical-binding: t; -*-

(defgroup my-claude nil
  "Claude CLI integration."
  :group 'tools
  :prefix "my-claude-")

(defcustom my-claude-cli-program "claude"
  "Claude CLI command."
  :type 'string
  :group 'my-claude)

(defcustom my-claude-buffer-prefix "*Claude"
  "Prefix for Claude buffer names."
  :type 'string
  :group 'my-claude)

(defvar my-claude--current-buffer nil
  "The current active Claude buffer.")

(defcustom my-claude-prompt-prefixes
  '("Explain this code"
    "Review this code"
    "Find bugs in this code"
    "Refactor this code"
    "Add tests for this code"
    "Document this code")
  "List of prompt prefixes for `my/claude-send-region-with-prompt'."
  :type '(repeat string)
  :group 'my-claude)

(defun my-claude--project-root ()
  "Return the project root directory or `default-directory'."
  (or (vc-root-dir)
      (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun my-claude--buffer-list ()
  "Return list of active Claude buffers."
  (cl-remove-if-not
   (lambda (buf)
     (and (string-prefix-p my-claude-buffer-prefix (buffer-name buf))
          (get-buffer-process buf)))
   (buffer-list)))

(defun my-claude--active-session-p ()
  "Return non-nil if there is an active Claude session."
  (and my-claude--current-buffer
       (buffer-live-p my-claude--current-buffer)
       (get-buffer-process my-claude--current-buffer)))

(defun my-claude--select-buffer (prompt)
  "Select a Claude buffer with PROMPT, returning the buffer or nil if none exist."
  (let ((buffers (my-claude--buffer-list)))
    (cond
     ((null buffers) nil)
     ((= (length buffers) 1) (car buffers))
     (t (get-buffer (completing-read prompt (mapcar #'buffer-name buffers) nil t))))))

(defun my-claude--ensure-session ()
  "Ensure a Claude session exists, prompting for selection if multiple exist."
  (let ((buffers (my-claude--buffer-list)))
    (cond
     ((null buffers)
      (user-error "No active Claude session. Run `my/claude-chat' first"))
     ((= (length buffers) 1)
      (setq my-claude--current-buffer (car buffers)))
     (t
      (setq my-claude--current-buffer
            (get-buffer
             (completing-read "Send to Claude session: "
                              (mapcar #'buffer-name buffers) nil t)))))))

(defun my-claude--next-buffer-number ()
  "Find the lowest available buffer number."
  (let ((used-numbers
         (save-match-data
           (cl-loop for buf in (buffer-list)
                    for name = (buffer-name buf)
                    when (string-match
                          (format "^%s-\\([0-9]+\\)\\*$"
                                  (regexp-quote my-claude-buffer-prefix))
                          name)
                    collect (string-to-number (match-string 1 name))))))
    (cl-loop for n from 1
             unless (memq n used-numbers)
             return n)))

(defun my-claude--generate-buffer-name (&optional name)
  "Generate a unique buffer name for a new Claude chat.
If NAME is provided, use it instead of a number."
  (if name
      (format "%s:%s*" my-claude-buffer-prefix name)
    (format "%s-%d*" my-claude-buffer-prefix (my-claude--next-buffer-number))))

(defun my-claude--start-chat (dir &optional new-session name)
  "Start a Claude chat session in DIR.
If NEW-SESSION is non-nil, always create a new session.
If NAME is provided, use it as the buffer name."
  (unless (executable-find my-claude-cli-program)
    (error "Claude CLI not found. Install it and run `claude login`"))
  (require 'vterm)
  (if (and (not new-session) (my-claude--active-session-p))
      (pop-to-buffer my-claude--current-buffer)
    (let* ((buf-name (my-claude--generate-buffer-name name))
           (default-directory dir)
           (vterm-buffer-name buf-name)
           (vterm-shell my-claude-cli-program))
      (vterm)
      (setq my-claude--current-buffer (get-buffer buf-name)))))

(defun my-claude--send (text)
  "Send TEXT to the current Claude session and switch to it."
  (my-claude--ensure-session)
  (discard-input)
  (with-current-buffer my-claude--current-buffer
    (vterm-send-string text))
  (pop-to-buffer my-claude--current-buffer))

;;;###autoload
(defun my/claude-chat ()
  "Open a Claude chat buffer in vterm from current directory."
  (interactive)
  (my-claude--start-chat default-directory))

;;;###autoload
(defun my/claude-chat-project-root ()
  "Open a Claude chat buffer in vterm from project root."
  (interactive)
  (my-claude--start-chat (my-claude--project-root)))

;;;###autoload
(defun my/claude-new-chat ()
  "Create a new Claude chat session in current directory."
  (interactive)
  (my-claude--start-chat default-directory t))

;;;###autoload
(defun my/claude-new-chat-project-root ()
  "Create a new Claude chat session in project root."
  (interactive)
  (my-claude--start-chat (my-claude--project-root) t))

;;;###autoload
(defun my/claude-new-chat-named (name)
  "Create a new named Claude chat session in current directory."
  (interactive "sSession name: ")
  (my-claude--start-chat default-directory t name))

;;;###autoload
(defun my/claude-new-chat-named-project-root (name)
  "Create a new named Claude chat session in project root."
  (interactive "sSession name: ")
  (my-claude--start-chat (my-claude--project-root) t name))

;;;###autoload
(defun my/claude-rename-chat (name)
  "Rename the current Claude chat session."
  (interactive "sNew session name: ")
  (my-claude--ensure-session)
  (with-current-buffer my-claude--current-buffer
    (rename-buffer (my-claude--generate-buffer-name name) t)))

;;;###autoload
(defun my/claude-switch-chat ()
  "Switch between active Claude chat buffers."
  (interactive)
  (let ((buf (my-claude--select-buffer "Switch to Claude chat: ")))
    (unless buf
      (user-error "No active Claude sessions"))
    (setq my-claude--current-buffer buf)
    (pop-to-buffer buf)))

;;;###autoload
(defun my/claude-toggle ()
  "Toggle Claude buffer visibility."
  (interactive)
  (when-let ((win (and my-claude--current-buffer
                       (buffer-live-p my-claude--current-buffer)
                       (get-buffer-window my-claude--current-buffer))))
    (delete-window win)
    (cl-return-from my/claude-toggle))
  (when (my-claude--active-session-p)
    (pop-to-buffer my-claude--current-buffer)
    (cl-return-from my/claude-toggle))
  (let ((buf (my-claude--select-buffer "Open Claude chat: ")))
    (if buf
        (progn
          (setq my-claude--current-buffer buf)
          (pop-to-buffer buf))
      (my/claude-chat))))

;;;###autoload
(defun my/claude-send-region (start end)
  "Send the region from START to END to Claude."
  (interactive "r")
  (my-claude--send (buffer-substring-no-properties start end)))

;;;###autoload
(defun my/claude-send-buffer ()
  "Send the entire buffer to Claude."
  (interactive)
  (my/claude-send-region (point-min) (point-max)))

;;;###autoload
(defun my/claude-send-region-with-context (start end)
  "Send region from START to END with file context."
  (interactive "r")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (ext (or (file-name-extension file) ""))
         (line (line-number-at-pos start))
         (text (buffer-substring-no-properties start end)))
    (my-claude--send (format "From `%s` (line %d):\n```%s\n%s\n```"
                             (file-name-nondirectory file) line ext text))))

;;;###autoload
(defun my/claude-send-region-with-prompt (start end)
  "Send region from START to END with a prompt prefix."
  (interactive "r")
  (let* ((prompt (completing-read "Prompt: " my-claude-prompt-prefixes nil nil))
         (file (or (buffer-file-name) (buffer-name)))
         (ext (or (file-name-extension file) ""))
         (text (buffer-substring-no-properties start end)))
    (my-claude--send (format "%s:\n```%s\n%s\n```" prompt ext text))))
