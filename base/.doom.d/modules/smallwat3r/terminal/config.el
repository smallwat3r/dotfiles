;;; smallwat3r/terminal/config.el -*- lexical-binding: t; -*-

(defvar my-ssh-config-files
  '("~/.ssh/config"
    "~/.ssh/work"
    "~/.ssh/private")
  "List of user SSH config files used for TRAMP and ssh helpers.")

(add-to-list 'auto-mode-alist
             '("/\\.ssh/\\(?:work\\|private\\)\\'" . ssh-config-mode))

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.03
        ;; Use eterm-color for better compatibility with some CLI tools
        vterm-term-environment-variable "eterm-color")

  ;; HACK: Fix for copying text from CLI tools that do their own line wrapping
  ;; (e.g., Claude CLI). vterm uses libvterm which stores screen content as a
  ;; 2D grid. When an application outputs pre-wrapped text, vterm pads each
  ;; line with spaces to fill the terminal width. When copying, these padding
  ;; spaces appear instead of newlines.
  ;;
  ;; The built-in `vterm-copy-mode-remove-fake-newlines' only works when vterm
  ;; itself wraps lines (tracking them as "fake" newlines). It doesn't help
  ;; when the application handles wrapping internally.
  ;;
  ;; This workaround replaces runs of 30+ consecutive spaces with newlines.
  ;; The threshold of 30 is high enough to preserve intentional spacing (code
  ;; indentation, table alignment) while catching the padding (typically 50-100+
  ;; spaces depending on terminal width and content).

  (defun my/vterm-clean-fake-newlines (text)
    "Replace vterm line-padding spaces with newlines.
Targets runs of 30+ consecutive spaces, which are almost certainly
padding rather than intentional formatting."
    (string-trim-right
     (replace-regexp-in-string " \\{30,\\}" "\n" text)))

  (defun my/vterm-clean-kill-new (orig-fn string &rest args)
    "Advice around `kill-new' to clean vterm padding when copying.
Only applies when copying from a vterm buffer."
    (apply orig-fn
           (if (derived-mode-p 'vterm-mode)
               (my/vterm-clean-fake-newlines string)
             string)
           args))

  (advice-add 'kill-new :around #'my/vterm-clean-kill-new))

;; always display the modeline in vterm (depth 90 ensures it runs last)
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)) 90)

(setq vterm-always-compile-module t
      ;; Remove fake newlines/spaces when copying from vterm (set early)
      vterm-copy-mode-remove-fake-newlines t)

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
