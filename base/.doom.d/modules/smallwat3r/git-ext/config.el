;;; smallwat3r/git-ext/config.el -*- lexical-binding: t; -*-

;; Magit
;; doc: https://github.com/magit/magit
(after! magit
  ;; These bindings are hard to work with when using evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil)

  ;; do not show line numbers in the commit buffer
  (setq-hook! 'git-commit-mode-hook display-line-numbers nil)

  (after! git-commit
    (setq git-commit-summary-max-length 75)
    ;; Kill all COMMIT_EDITMSG buffers after committing. We iterate through
    ;; all buffers to handle duplicates (COMMIT_EDITMSG<2>, etc.) that may
    ;; exist when working with multiple repositories.
    (add-hook! 'git-commit-post-finish-hook
      (dolist (buf (buffer-list))
        (when (string-prefix-p "COMMIT_EDITMSG" (buffer-name buf))
          (kill-buffer buf)))))

  ;; Remap keys to move commits up or down when using interactive rebase.
  (after! git-rebase
    (map! :map git-rebase-mode-map
          "K" #'git-rebase-move-line-up
          "J" #'git-rebase-move-line-down
          "N" #'git-rebase-move-line-up   ; custom layout
          "A" #'git-rebase-move-line-down)))

;; Custom SSH options for Git commands from Emacs to improve reliability:
;; -4: force IPv4 (avoids IPv6 connectivity issues)
;; ConnectTimeout: fail quickly if host is unreachable
;; ServerAliveInterval/CountMax: detect dropped connections
;; TCPKeepAlive: prevent firewall from closing idle connections
;; GSSAPIAuthentication=no: skip Kerberos (faster for non-Kerberos hosts)
;; ControlMaster=no: avoid conflicts with user's ControlMaster settings
(setenv "GIT_SSH_COMMAND" "ssh -4 \
  -o ConnectTimeout=10 \
  -o ServerAliveInterval=20 \
  -o ServerAliveCountMax=3 \
  -o TCPKeepAlive=yes \
  -o GSSAPIAuthentication=no \
  -o ControlMaster=no")

;; git-timemachine
;; doc: https://github.com/emacsmirror/git-timemachine
(after! git-timemachine
  ;; custom layout support
  (map! :map git-timemachine-mode-map
        :n "C-n" #'git-timemachine-show-previous-revision
        :n "C-a" #'git-timemachine-show-next-revision))
