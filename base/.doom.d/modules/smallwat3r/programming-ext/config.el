;;; smallwat3r/programming-ext/config.el -*- lexical-binding: t; -*-

;; LSP via Eglot
(after! eglot
  (map! :map eglot-mode-map
        :leader
        :prefix "r"
        :desc "Reconnect Eglot" "w" #'eglot-reconnect))

(add-hook! 'eglot-managed-mode-hook (eglot-inlay-hints-mode -1))

(set-eglot-client! 'python-mode '("basedpyright-langserver" "--stdio"))

;; Flycheck
(after! flycheck
  (setq flycheck-disabled-checkers '(python-flake8 python-pylint))
  (map! :map flycheck-mode-map
        :leader
        :localleader
        :desc "Flycheck list errors" "l" #'flycheck-list-errors))

;; Only activate flycheck on demand
(after! flycheck
  (global-flycheck-mode -1))

(after! flycheck-eglot
  (global-flycheck-eglot-mode -1))

(after! flycheck-popup-tip
  (setq flycheck-popup-tip-error-prefix "(!) "))

;; Spell-fu
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

;; Disable spell-fu by default (enable explicitly when needed)
(remove-hook! (text-mode) #'spell-fu-mode)

;; Shell scripts
(after! sh-mode
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2"   ; spaces for indentation
      "-ci"      ; indent switch cases
      "-bn")     ; binary ops may start a line
    :modes '(sh-mode)))

(setq-hook! 'sh-mode-hook
  flycheck-checker 'sh-shellcheck
  flycheck-shellcheck-excluded-warnings '("SC1091")
  sh-basic-offset 2
  indent-tabs-mode nil)

;; JavaScript
(after! js2-mode
  (set-formatter! 'prettier
    '("prettier"
      "--print-width" "120"
      ("--stdin-filepath" "%s" buffer-file-name))
    :modes '(js2-mode)))

(setq-hook! 'js2-mode-hook js2-basic-offset 2)

;; JSON
(setq-hook! 'json-mode-hook tab-width 2)

;; YAML - invert colors for creamy theme
(add-hook! 'yaml-mode-hook
  (when (eq doom-theme 'creamy)
    (face-remap-add-relative
     'font-lock-variable-name-face
     '(:inherit font-lock-keyword-face))
    (buffer-face-mode -1)))

;; Restclient
(setq-hook! 'restclient-mode-hook tab-width 4)

;; Go
(setq-hook! 'go-mode-hook indent-tabs-mode t)

;; Web mode
(defun my/web-mode-configs ()
  (setq-local tab-width 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-script-padding 2
              web-mode-style-padding 2))

(add-hook! 'web-mode-hook #'my/web-mode-configs)

(setq-hook! '(html-mode-hook web-mode-hook)
  +format-with :none)
