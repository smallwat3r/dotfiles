;;; smallwat3r/highlighting/config.el -*- lexical-binding: t; -*-

;; Disable globally highlighting the current line the cursor is on.
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; When hl-line is available, do not override the color of rainbow-mode.
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

;; todos
;; doc: https://github.com/tarsius/hl-todo
(after! hl-todo
  (add-to-list 'hl-todo-keyword-faces '("HACK" . "VioletRed1")))

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
  :config
  ;; deactivate bindings I do not use that conflict with other commands
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "w") nil)
  (define-key symbol-overlay-map (kbd "t") nil)
  (define-key symbol-overlay-map (kbd "i") nil))

;; Line numbers colorization and tick indicators
;; Colorize every 5th line number as a visual indicator, useful when using
;; relative line numbers to quickly estimate jump distances.
(after! display-line-numbers
  (setq display-line-numbers-type nil
        display-line-numbers-minor-tick 5
        display-line-numbers-major-tick 5))

(custom-set-faces!
  ;; base: no background, subtle gray
  '(line-number :background unspecified :foreground "gray50")
  ;; every 5th line: orange and bold
  '(line-number-minor-tick :inherit line-number :foreground "orange" :weight bold)
  '(line-number-major-tick :inherit line-number-minor-tick)
  ;; current line: distinct orange red
  '(line-number-current-line :inherit line-number :foreground "orange red" :weight bold))
