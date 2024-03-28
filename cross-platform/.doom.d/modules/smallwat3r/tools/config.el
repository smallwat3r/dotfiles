;;; smallwat3r/tools/config.el -*- lexical-binding: t; -*-

;; Imenu list
;; doc: https://github.com/bmag/imenu-list
(use-package! imenu-list
  :commands (imenu-list-smart-toggle))

;; Insert random text
;; doc: https://github.com/jschaf/emacs-lorem-ipsum
(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list)
  :init
  (map! :leader
        :prefix "i"
        (:prefix ("l" . "lorem")
         :desc "Insert paragraphs" "p" #'lorem-ipsum-insert-paragraphs
         :desc "Insert sentences"  "s" #'lorem-ipsum-insert-sentences
         :desc "Insert list"       "l" #'lorem-ipsum-insert-list)))

;; Clipboard history, interact with Flycut.app
;; doc: https://github.com/redguardtoo/cliphist
(use-package! cliphist
  :commands (cliphist-select-item cliphist-paste-item)
  :init
  (map! :leader
        :prefix ("C" . "clipboard")
        :desc "Select item" "s" #'cliphist-select-item
        :desc "Paste item"  "p" #'cliphist-paste-item))

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
