;;; smallwat3r/google/config.el -*- lexical-binding: t; -*-

;; google-translate
;; doc: https://github.com/atykhonov/google-translate
(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-query-translate
             google-translate-buffer)
  :init
  (after! google-translate-backend
    (setq google-translate-backend-method 'curl)))

;; google-this
;; doc: https://github.com/Malabarba/emacs-google-this
(use-package! google-this
  :commands (google-this google-this-word google-this-line))
