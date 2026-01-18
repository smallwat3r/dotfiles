;;; smallwat3r/vterm-ext/config.el -*- lexical-binding: t; -*-

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
        vterm-timer-delay 0.01)

  (map! :map vterm-mode-map
        :n "B" #'vterm-beginning-of-line  ; beg of command
        :n "<return>" #'evil-insert-resume
        [remap delete-forward-char] #'vterm-send-delete
        :in "<M-backspace>" #'vterm-send-meta-backspace
        :n "<M-backspace>" #'vterm-send-meta-backspace
        :in "C-k" #'vterm-send-up
        :in "C-j" #'vterm-send-down
        :n "dd" (cmd! (vterm-send-C-c))
        "C-;" #'my/vterm-zsh-history-pick))

(setq vterm-always-compile-module t)

;; always display the modeline in vterm
(remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

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

(map! :leader
      :prefix "o"
      :desc "Terminal"               "1" #'my/terminal-here
      :desc "Vterm at root"          "T" #'+vterm/here
      :desc "Toggle vterm at root"   "t" #'+vterm/toggle
      :desc "Vterm at buffer"        "V" #'my/vterm/here-current-buffer
      :desc "Toggle vterm at buffer" "v" #'my/vterm/toggle-current-buffer
      :desc "Tramp SSH conn"         "." #'my/open-remote-conn
      :desc "Term SSH conn"          "s" #'my/ssh-external)
