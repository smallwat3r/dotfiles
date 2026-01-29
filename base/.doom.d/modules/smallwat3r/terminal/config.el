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

;; eat - Emulate A Terminal
;; doc: https://codeberg.org/akib/emacs-eat
(use-package! eat
  :config
  ;; Terminal colors optimized for light backgrounds
  (custom-set-faces!
    '(eat-term-color-0 :foreground "#000000")   ; black
    '(eat-term-color-1 :foreground "#aa0000")   ; red
    '(eat-term-color-2 :foreground "#00aa00")   ; green
    '(eat-term-color-3 :foreground "#aa5500")   ; yellow/brown
    '(eat-term-color-4 :foreground "#0000aa")   ; blue
    '(eat-term-color-5 :foreground "#aa00aa")   ; magenta
    '(eat-term-color-6 :foreground "#00aaaa")   ; cyan
    '(eat-term-color-7 :foreground "#555555")   ; white (dimmed for light bg)
    '(eat-term-color-8 :foreground "#444444")   ; bright black
    '(eat-term-color-9 :foreground "#cc0000")   ; bright red
    '(eat-term-color-10 :foreground "#00cc00")  ; bright green
    '(eat-term-color-11 :foreground "#cc7700")  ; bright yellow
    '(eat-term-color-12 :foreground "#0000cc")  ; bright blue
    '(eat-term-color-13 :foreground "#cc00cc")  ; bright magenta
    '(eat-term-color-14 :foreground "#00cccc")  ; bright cyan
    '(eat-term-color-15 :foreground "#333333")) ; bright white

  (setq eat-kill-buffer-on-exit t
        eat-enable-mouse t
        eat-tramp-shells '(("ssh" . "/bin/bash")
                           ("scp" . "/bin/bash")
                           ("sshx" . "/bin/bash")
                           ("docker" . "/bin/sh"))
        ;; Point to the correct integration directory (repos, not build)
        eat-term-shell-integration-directory
        (expand-file-name "straight/repos/eat/integration" doom-local-dir)
        ;; Disable left margin prompt annotation (removes left padding)
        eat-enable-shell-prompt-annotation nil)

  ;; Evil integration: switch eat modes based on evil state
  ;; - Insert state: semi-char mode (keys go to terminal)
  ;; - Normal/visual state: emacs mode (keys go to Emacs for navigation)
  (defun my/eat-evil-insert-enter ()
    "Switch to semi-char mode when entering insert state in eat."
    (when (and (derived-mode-p 'eat-mode)
               (boundp 'eat--input-mode)
               (not (eq eat--input-mode 'semi-char)))
      (eat-semi-char-mode)))

  (defun my/eat-evil-insert-exit ()
    "Switch to emacs mode when exiting insert state in eat."
    (when (and (derived-mode-p 'eat-mode)
               (boundp 'eat--input-mode)
               (not (eq eat--input-mode 'emacs)))
      (eat-emacs-mode)))

  (add-hook 'evil-insert-state-entry-hook #'my/eat-evil-insert-enter)
  (add-hook 'evil-insert-state-exit-hook #'my/eat-evil-insert-exit)

  ;; Start in insert state (semi-char mode) when opening eat
  (add-hook 'eat-mode-hook #'evil-insert-state)

  ;; Always display the modeline in eat
  (defun my/eat-show-modeline ()
    "Ensure modeline is visible in eat buffers."
    (hide-mode-line-mode -1)
    (setq-local mode-line-format (default-value 'mode-line-format)))
  (add-hook 'eat-mode-hook #'my/eat-show-modeline 100)

  ;; Yank from kill ring into terminal
  (defun my/eat-yank ()
    "Yank the last killed text into eat terminal."
    (interactive)
    (when eat-terminal
      (eat-term-send-string eat-terminal (current-kill 0))))

  (defun my/eat-backward-kill-word ()
    "Send backward-kill-word (M-backspace / ESC DEL) to terminal."
    (interactive)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\e\C-?")))

  ;; Keybindings for semi-char mode
  (define-key eat-semi-char-mode-map (kbd "<escape>") #'evil-normal-state)
  (define-key eat-semi-char-mode-map (kbd "C-<backspace>") #'my/eat-backward-kill-word)
  (define-key eat-semi-char-mode-map (kbd "M-<backspace>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-d") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-f") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-b") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-<left>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-<right>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-k") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-j") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-y") #'my/eat-yank)
  (define-key eat-semi-char-mode-map (kbd "C-,") #'my/eat-zsh-history-pick)

  ;; Tramp integration for remote connections
  (defun my/eat--tramp-base-path ()
    "Return the Tramp prefix (e.g., /ssh:user@host:) without the directory."
    (let* ((vec (when (tramp-tramp-file-p default-directory)
                  (tramp-dissect-file-name default-directory)))
           (method (and vec (tramp-file-name-method vec)))
           (user   (and vec (tramp-file-name-user vec)))
           (host   (and vec (tramp-file-name-host vec))))
      (when (and method host)
        (format "/%s:%s%s:"
                method
                (if (and user (not (string-empty-p user))) (concat user "@") "")
                host))))

  ;; Message handler for opening files from remote shells
  (defun my/eat-find-file-handler (path)
    "Open PATH in Emacs in another window."
    (when (and path (not (string-empty-p path)))
      (find-file-other-window path)))

  (add-to-list 'eat-message-handler-alist '("find-file" . my/eat-find-file-handler))

  (defun my/eat-setup-tramp (proc)
    "Set up eat for remote Tramp connections.

Renames the buffer to include the remote host, and injects an `e' shell
function that opens remote files in a local Emacs buffer.

The `e' function works by sending an escape sequence that eat interprets:
  - User runs: e myfile.txt
  - Shell resolves to absolute path: /home/user/myfile.txt
  - Shell sends OSC 51 escape sequence with base64-encoded message
  - Eat receives and decodes the message
  - Eat calls the registered `find-file' handler with the full tramp path
  - Emacs opens: /ssh:user@host:/home/user/myfile.txt

Escape sequence format (eat message protocol):
  \\033]51;e;M;<base64:handler>;<base64:path>\\033\\\\

Example for `e myfile.txt' on ssh:pi@myhost in /home/pi:
  - handler: \"find-file\" -> base64 -> \"ZmluZC1maWxl\"
  - path: \"/ssh:pi@myhost:/home/pi/myfile.txt\" -> base64 -> \"L3NzaDpwaUB...\"
  - full sequence: \\033]51;e;M;ZmluZC1maWxl;L3NzaDpwaUB...\\033\\\\

Eat decodes both base64 strings, looks up \"find-file\" in
`eat-message-handler-alist', and calls the handler with the decoded path.

PROC is the process started by eat."
    (when-let* ((buf (process-buffer proc))
                (_ (buffer-live-p buf)))
      (with-current-buffer buf
        (when (file-remote-p default-directory)
          (let ((tramp-base-path (my/eat--tramp-base-path)))
            (when tramp-base-path
              (rename-buffer (format "*eat@%s*" tramp-base-path) t)
              ;; Delay injection to let the shell initialize
              (run-at-time
               0.5 nil
               (lambda (process tramp-path)
                 (when (and (process-live-p process)
                            (buffer-live-p (process-buffer process)))
                   (with-current-buffer (process-buffer process)
                     ;; Set TERM to xterm-256color for compatibility (remote
                     ;; machines don't have eat terminfo)
                     (process-send-string process "export TERM=xterm-256color\n")
                     (process-send-string
                      process
                      (concat
                       "e() { "
                       "local f=\"$1\"; "
                       "[[ \"$f\" != /* ]] && f=\"$PWD/$f\"; "
                       "printf '\\033]51;e;M;%s;%s\\033\\\\' "
                       "\"$(printf 'find-file' | base64)\" "
                       "\"$(printf '" tramp-path "%s' \"$f\" | base64)\"; "
                       "}\n"))
                     (process-send-string process "clear\n"))))
               proc tramp-base-path)))))))

  (add-hook 'eat-exec-hook #'my/eat-setup-tramp))
