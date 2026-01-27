;;; smallwat3r/claude/autoload.el -*- lexical-binding: t; -*-

(defgroup my-claude nil
  "Claude CLI integration."
  :group 'tools
  :prefix "my-claude-")

(defcustom my-claude-cli-program "claude"
  "Claude CLI command."
  :type 'string
  :group 'my-claude)

(defcustom my-claude-buffer-prefix "Claude"
  "Prefix for Claude buffer names."
  :type 'string
  :group 'my-claude)

(defvar my-claude--current-buffers (make-hash-table :test 'equal)
  "Hash table mapping project roots to their current Claude buffer.")

(defvar-local my-claude--project-root nil
  "The project root this Claude buffer belongs to.")

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

(defun my-claude--buffer-list (&optional all-projects)
  "Return list of active Claude buffers for the current project.
If ALL-PROJECTS is non-nil, return buffers from all projects."
  (let ((project (my-claude--project-root))
        (prefix (format "*%s" my-claude-buffer-prefix)))
    (cl-remove-if-not
     (lambda (buf)
       (and (string-prefix-p prefix (buffer-name buf))
            (get-buffer-process buf)
            (or all-projects
                (equal project (buffer-local-value 'my-claude--project-root buf)))))
     (buffer-list))))

(defun my-claude--get-current-buffer ()
  "Get the current Claude buffer for the current project."
  (gethash (my-claude--project-root) my-claude--current-buffers))

(defun my-claude--set-current-buffer (buf)
  "Set BUF as the current Claude buffer for the current project."
  (puthash (my-claude--project-root) buf my-claude--current-buffers))

(defun my-claude--active-session-p ()
  "Return non-nil if there is an active Claude session for the current project."
  (let ((buf (my-claude--get-current-buffer)))
    (and buf
         (buffer-live-p buf)
         (get-buffer-process buf))))

(defun my-claude--select-buffer (prompt)
  "Select a Claude buffer with PROMPT, returning the buffer or nil if none exist."
  (let ((buffers (my-claude--buffer-list)))
    (cond
     ((null buffers) nil)
     ((= (length buffers) 1) (car buffers))
     (t (get-buffer (completing-read prompt (mapcar #'buffer-name buffers) nil t))))))

(defun my-claude--ensure-session ()
  "Ensure a Claude session exists for the current project.
Prompts for selection if multiple exist."
  (let ((buffers (my-claude--buffer-list)))
    (cond
     ((null buffers)
      (user-error "No active Claude session for this project. Run `my/claude-chat' first"))
     ((= (length buffers) 1)
      (my-claude--set-current-buffer (car buffers)))
     (t
      (my-claude--set-current-buffer
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
                          (format "^\\*%s-\\([0-9]+\\)\\*$"
                                  (regexp-quote my-claude-buffer-prefix))
                          name)
                    collect (string-to-number (match-string 1 name))))))
    (cl-loop for n from 1
             unless (memq n used-numbers)
             return n)))

(defun my-claude--generate-buffer-name (&optional name)
  "Generate a unique buffer name for a new Claude chat.
If NAME is provided, use it instead of a number.
Note: `ansi-term' wraps this in *...* automatically."
  (if name
      (format "%s:%s" my-claude-buffer-prefix name)
    (format "%s-%d" my-claude-buffer-prefix (my-claude--next-buffer-number))))

(defun my-claude--start-chat (dir &optional new-session name)
  "Start a Claude chat session in DIR.
If NEW-SESSION is non-nil, always create a new session.
If NAME is provided, use it as the buffer name."
  (unless (executable-find my-claude-cli-program)
    (error "Claude CLI not found. Install it and run `claude login`"))
  (require 'term)
  (if (and (not new-session) (my-claude--active-session-p))
      (pop-to-buffer (my-claude--get-current-buffer))
    (let* ((project-root (my-claude--project-root))
           (buf-name (my-claude--generate-buffer-name name))
           (default-directory dir))
      (ansi-term my-claude-cli-program buf-name)
      (let ((buf (get-buffer (format "*%s*" buf-name))))
        (with-current-buffer buf
          (setq my-claude--project-root project-root))
        (my-claude--set-current-buffer buf)))))

(defun my-claude--send (text)
  "Send TEXT to the current Claude session and switch to it."
  (my-claude--ensure-session)
  (let ((buf (my-claude--get-current-buffer)))
    (discard-input)
    (with-current-buffer buf
      (term-send-string (get-buffer-process buf) text))
    (pop-to-buffer buf)))

;;;###autoload
(defun my/claude-chat ()
  "Open a Claude chat buffer from current directory."
  (interactive)
  (my-claude--start-chat default-directory))

;;;###autoload
(defun my/claude-chat-project-root ()
  "Open a Claude chat buffer from project root."
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
  (with-current-buffer (my-claude--get-current-buffer)
    (rename-buffer (my-claude--generate-buffer-name name) t)))

;;;###autoload
(defun my/claude-switch-chat ()
  "Switch between active Claude chat buffers for the current project."
  (interactive)
  (let ((buf (my-claude--select-buffer "Switch to Claude chat: ")))
    (unless buf
      (user-error "No active Claude sessions for this project"))
    (my-claude--set-current-buffer buf)
    (pop-to-buffer buf)))

;;;###autoload
(defun my/claude-toggle ()
  "Toggle Claude buffer visibility for the current project."
  (interactive)
  (let ((current-buf (my-claude--get-current-buffer)))
    (when-let ((win (and current-buf
                         (buffer-live-p current-buf)
                         (get-buffer-window current-buf))))
      (delete-window win)
      (cl-return-from my/claude-toggle))
    (when (my-claude--active-session-p)
      (pop-to-buffer current-buf)
      (cl-return-from my/claude-toggle))
    (let ((buf (my-claude--select-buffer "Open Claude chat: ")))
      (if buf
          (progn
            (my-claude--set-current-buffer buf)
            (pop-to-buffer buf))
        (my/claude-chat)))))

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
