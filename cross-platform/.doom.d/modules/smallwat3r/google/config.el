;;; smallwat3r/google/config.el -*- lexical-binding: t; -*-

;; google-translate
;; doc: https://github.com/atykhonov/google-translate
(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-query-translate
             google-translate-buffer)
  :init
  (after! google-translate-backend
    (setq google-translate-backend-method 'curl))

  (map! :leader
        :prefix ("T" . "translate")
        :desc "Translate query"    "q" #'google-translate-query-translate
        :desc "Translate at point" "t" #'google-translate-at-point
        :desc "Translate buffer"   "b" #'google-translate-buffer))

;; google-this
;; doc: https://github.com/Malabarba/emacs-google-this
(use-package! google-this
  :commands (google-this google-this-word google-this-line)
  :init
  (map! :leader
        :prefix ("G" . "google")
        :desc "Query google"     "q" #'google-this
        :desc "Google this word" "w" #'google-this-word
        :desc "Google this line" "l" #'google-this-line))
