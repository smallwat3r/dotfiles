;;; smallwat3r/tools/config.el -*- lexical-binding: t; -*-

;; Imenu list
;; doc: https://github.com/bmag/imenu-list
(use-package! imenu-list
  :defer t
  :commands (imenu-list-smart-toggle))

;; Bitwarden
;; doc: https://github.com/seanfarley/emacs-bitwarden
(use-package! bitwarden
  :custom
  (bitwarden-user user-mail-address)
  (bitwarden-automatic-unlock
   (lambda () (auth-source-pass-get 'secret "bitwarden/password"))))

;; Copy the active region and add it to the kill ring formatted for
;; Github, Slack etc...
;; doc: https://github.com/sshaw/copy-as-format
(use-package! copy-as-format
  :config
  (global-set-key (kbd "C-c y s") 'copy-as-format-slack)
  (global-set-key (kbd "C-c y g") 'copy-as-format-github)
  (global-set-key (kbd "C-c y l") 'copy-as-format-gitlab)
  (global-set-key (kbd "C-c y m") 'copy-as-format-markdown)
  (global-set-key (kbd "C-c y o") 'copy-as-format-org-mode))

;; GPG pinentry
(use-package! pinentry
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

;; Display London TfL tube status
;; doc: https://github.com/smallwat3r/tubestatus.el
(use-package! tubestatus
  :commands (tubestatus))

;; Untappd feed
;; doc: https://github.com/smallwat3r/untappd.el
(use-package! untappd
  :commands (untappd-feed)
  :custom (untappd-access-token (auth-source-pass-get 'secret "untappd/token")))
