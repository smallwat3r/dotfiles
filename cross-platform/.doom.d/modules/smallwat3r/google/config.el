;;; smallwat3r/google/config.el -*- lexical-binding: t; -*-

;; google-translate
;; doc: https://github.com/atykhonov/google-translate
(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-query-translate
             google-translate-buffer)
  :init
  (set-popup-rule! "*Google Translate*" :size 0.4 :side 'bottom :select t :modeline t)

  (after! google-translate-backend
    (setq google-translate-backend-method 'curl))

  ;; HACK: resolve ttk to be able to communicate with google translate.
  (after! google-translate-tk
    (advice-add #'google-translate--search-tkk
                :override (lambda () "Search TKK fix." (list 430675 2721866130))))

  (map! :leader
        :prefix-map ("T" . "translate")
        :desc "Translate query"    "q" #'google-translate-query-translate
        :desc "Translate at point" "t" #'google-translate-at-point
        :desc "Translate buffer"   "b" #'google-translate-buffer))

;; google-this
;; doc: https://github.com/Malabarba/emacs-google-this
(use-package! google-this
  :commands (google-this google-this-word google-this-line)
  :init
  (map! :leader
        :prefix-map ("G" . "google")
        :desc "Query google"     "q" #'google-this
        :desc "Google this word" "w" #'google-this-word
        :desc "Google this line" "l" #'google-this-line))
