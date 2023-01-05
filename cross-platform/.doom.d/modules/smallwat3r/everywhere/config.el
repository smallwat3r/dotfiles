;;; smallwat3r/everywhere/config.el -*- lexical-binding: t; -*-

;; Pop up a new Emacs frame, everywhere.
;; doc: https://github.com/tecosaur/emacs-everywhere
(use-package! emacs-everywhere
  :defer t
  :config
  (setq emacs-everywhere-init-hooks
        '(emacs-everywhere-set-frame-name
          emacs-everywhere-set-frame-position
          emacs-everywhere-major-mode-function
          emacs-everywhere-remove-trailing-whitespace))
  (setq emacs-everywhere-final-hooks
        '(emacs-everywhere-remove-trailing-whitespace))
  (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
          (width . 110)
          (height . 30))))
