;;; smallwat3r/terminal/config.el -*- lexical-binding: t; -*-

(defvar my-ssh-config-files
  '("~/.ssh/config"
    "~/.ssh/work"
    "~/.ssh/private")
  "List of user SSH config files used for TRAMP and ssh helpers.")

(add-to-list 'auto-mode-alist
             '("/\\.ssh/\\(?:work\\|private\\)\\'" . ssh-config-mode))

;; term
(after! term
  ;; Ensure proper UTF-8 encoding for term buffers
  (defun my/term-setup-encoding ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook #'my/term-setup-encoding)

  ;; Use xterm for better compatibility (avoid eterm which has limited support)
  (setq term-term-name "xterm-256color")

  ;; Filter OSC escape sequences (title, notifications) that term doesn't handle
  (defun my/term-filter-osc (orig-fn proc str)
    (funcall orig-fn proc
             (replace-regexp-in-string "\e\\][0-9;]*[^\a]*\a" "" str)))
  (advice-add 'term-emulate-terminal :around #'my/term-filter-osc)

  ;; Keybindings for term-mode (char mode) to match shell behavior
  (defun my/term-send-delete-word-backward ()
    (interactive)
    (term-send-raw-string "\e\C-?"))

  (defun my/term-send-delete-word-forward ()
    (interactive)
    (term-send-raw-string "\ed"))

  (defun my/term-send-forward-word ()
    (interactive)
    (term-send-raw-string "\ef"))

  (defun my/term-send-backward-word ()
    (interactive)
    (term-send-raw-string "\eb"))

  (defun my/term-yank ()
    (interactive)
    (term-send-raw-string (current-kill 0)))

  (define-key term-raw-map (kbd "C-<backspace>") #'my/term-send-delete-word-backward)
  (define-key term-raw-map (kbd "M-<backspace>") #'my/term-send-delete-word-backward)
  (define-key term-raw-map (kbd "M-d") #'my/term-send-delete-word-forward)
  (define-key term-raw-map (kbd "M-f") #'my/term-send-forward-word)
  (define-key term-raw-map (kbd "M-b") #'my/term-send-backward-word)
  (define-key term-raw-map (kbd "C-y") #'my/term-yank))

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.03)

  ;; Auto-enable vterm-copy-mode when entering evil visual state
  (defun my/vterm-visual-enter ()
    "Enable vterm-copy-mode when entering visual state in vterm."
    (when (and (derived-mode-p 'vterm-mode)
               (not vterm-copy-mode))
      (vterm-copy-mode 1)))

  (defun my/vterm-visual-exit ()
    "Disable vterm-copy-mode when exiting visual state in vterm."
    (when (and (derived-mode-p 'vterm-mode)
               vterm-copy-mode)
      (vterm-copy-mode -1)))

  (add-hook 'evil-visual-state-entry-hook #'my/vterm-visual-enter)
  (add-hook 'evil-visual-state-exit-hook #'my/vterm-visual-exit))

;; always display the modeline in vterm (depth 90 ensures it runs last)
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)) 90)

(setq vterm-always-compile-module t)

;; remote file access
(after! tramp
  (tramp-set-completion-function
   "ssh"
   (append
    (mapcar (lambda (f)
              (list 'tramp-parse-sconfig (expand-file-name f)))
            my-ssh-config-files)
    '((tramp-parse-sconfig "/etc/ssh_config")
      (tramp-parse-shosts "/etc/hosts")
      (tramp-parse-shosts "~/.ssh/known_hosts"))))
  ;; reuse SSH ControlMaster connections (requires ControlMaster in ~/.ssh/config)
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; cache remote file properties longer
  (setq remote-file-name-inhibit-cache nil)
  ;; disable version control checks on remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(defun my/vterm-tramp-base-path ()
  "Return the Tramp prefix (e.g., /ssh:user@host) without the directory."
  (let* ((vec (or (car (tramp-list-connections))
                  (when (tramp-tramp-file-p default-directory)
                    (tramp-dissect-file-name default-directory))))
         (method (and vec (tramp-file-name-method vec)))
         (user   (and vec (tramp-file-name-user vec)))
         (host   (and vec (tramp-file-name-host vec))))
    (when (and method host)
      (format "/%s:%s%s"
              method
              (if (and user (not (string-empty-p user))) (concat user "@") "")
              host))))

(defun my/vterm-buffer-hooks-on-tramp ()
  "Set up vterm for remote Tramp connections.
Renames the buffer to include the remote host, and injects an `e'
shell function that opens remote files in a local Emacs buffer
via vterm's OSC 51 escape sequence (e.g., `e .bashrc')."
  (when (and (eq major-mode 'vterm-mode)
             default-directory
             (file-remote-p default-directory))
    (let ((tramp-base-path (my/vterm-tramp-base-path)))
      (rename-buffer (format "*vterm@%s*" tramp-base-path) t)
      ;; Inject `e' function: converts relative paths to absolute, then uses
      ;; OSC 51 (a terminal escape sequence for shell-to-Emacs communication)
      ;; to tell vterm to run find-file with the full Tramp path.
      (vterm-send-string
       (format
        "e() { local f=\"$1\"; [[ \"$f\" != /* ]] && f=\"$PWD/$f\"; \
printf '\\033]51;Efind-file %s:%%s\\007' \"$f\"; }\n"
        tramp-base-path)))
    (vterm-send-string "clear\n")))
(add-hook! 'vterm-mode-hook #'my/vterm-buffer-hooks-on-tramp)

;; provides extra convenience functions for vterm
;; doc: https://github.com/Sbozzolo/vterm-extra
(use-package! vterm-extra
  :after vterm
  :bind (("s-t" . vterm-extra-dispatcher)
         :map vterm-mode-map
         (("C-c C-e" . vterm-extra-edit-command-in-new-buffer))))
