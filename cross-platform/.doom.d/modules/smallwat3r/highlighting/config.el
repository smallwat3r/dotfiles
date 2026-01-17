;;; smallwat3r/highlighting/config.el -*- lexical-binding: t; -*-

;; Highlight numbers
;; doc: https://github.com/Fanael/highlight-numbers
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; Rainbow parentheses in some major modes
;; doc: https://github.com/Fanael/rainbow-delimiters
(use-package! rainbow-delimiters
  :hook ((c-mode-common emacs-lisp-mode lisp-mode typescript-mode typescript-tsx-mode)
         . rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 4))

;; Overlay keywords
;; doc: https://github.com/wolray/symbol-overlay
(use-package! symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-remove-all)
  :init
  (map! :leader
        :prefix "c"
        :desc "Add overlay"     "h" #'symbol-overlay-put
        :desc "Remove overlays" "H" #'symbol-overlay-remove-all)
  :config
  ;; deactivate bindings I do not use that conflict with other commands
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "w") nil)
  (define-key symbol-overlay-map (kbd "t") nil)
  (define-key symbol-overlay-map (kbd "i") nil))
