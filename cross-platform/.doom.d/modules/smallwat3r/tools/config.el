;;; smallwat3r/tools/config.el -*- lexical-binding: t; -*-

;; Imenu list
;; doc: https://github.com/bmag/imenu-list
(use-package! imenu-list
  :commands (imenu-list-smart-toggle))

;; Bitwarden
;; doc: https://github.com/seanfarley/emacs-bitwarden
(use-package! bitwarden
  :custom
  (bitwarden-user user-mail-address)
  (bitwarden-automatic-unlock
   (lambda () (auth-source-pass-get 'secret "bitwarden/password")))
  :init
  (map! :leader
        :prefix "P"
        :desc "Bitwarden login"    "l" #'bitwarden-login
        :desc "Bitwarden unlock"   "u" #'bitwarden-unlock
        :desc "Bitwarden lock"     "L" #'bitwarden-lock
        :desc "Bitwarden list all" "b" #'bitwarden-list-all))

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
