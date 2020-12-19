;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Frame settings (GUI)
(when (display-graphic-p)
  ;; Title
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Size
  (add-to-list 'default-frame-alist '(width  . 106))
  (add-to-list 'default-frame-alist '(height . 64)))

;; Don't show line numbers by default
(setq display-line-numbers-type nil)

;; Frame title
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (replace-regexp-in-string
              ".*/[0-9]*-?" " "
              (subst-char-in-string ?_ ? buffer-file-name)) "%b"))
        (:eval
         (if (buffer-modified-p) " (+)"))))

;; Hide icon from frame
(setq ns-use-proxy-icon nil)

;; Terminal line wrap
(set-display-table-slot standard-display-table 'truncation ?›)
(set-display-table-slot standard-display-table 'wrap ?↵)

;; Emacs theme
(use-package! modus-vivendi-theme  ; dark theme (default)
  :config
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-intense-hl-line nil
        modus-vivendi-theme-subtle-diffs t
        modus-vivendi-theme-intense-paren-match 'intense-bold
        modus-vivendi-theme-org-blocks 'rainbow
        modus-vivendi-theme-completions 'opinionated
        modus-vivendi-theme-faint-syntax t)
  (load-theme 'modus-vivendi t))

(use-package! modus-operandi-theme  ; light theme
  :config
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-intense-hl-line nil
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-intense-paren-match 'intense-bold
        modus-operandi-theme-org-blocks 'rainbow
        modus-operandi-theme-completions 'opinionated))

(delq! t custom-theme-load-path)

;; Font settings
(setq
 doom-font (font-spec :family "Fantasque Sans Mono" :size 13 :weight 'regular)
 doom-serif-font (font-spec :family "Courier New")
 doom-variable-pitch-font (font-spec :family "Verdana")
 doom-themes-treemacs-enable-variable-pitch nil)

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Verdana" :height 130))
  (buffer-face-mode))

(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'markdown-mode-hook 'my-buffer-face-mode-variable)

;; Line spacing
(setq-default line-spacing 0)

;; Overwrite theme stuff
(custom-set-faces
 ;; '(default ((t (:background "#000000"))))  ; force black bg
 '(mode-line ((t (:background nil :box nil :overline nil :underline nil))))
 '(hl-line ((t (:background nil))))
 '(fringe ((t (:foreground "#111111"))))
 '(org-ellipsis ((t (:foreground "#00afaf"))))
 '(font-lock-comment-face ((t (:slant italic)))))  ; force italics on comments

(setq visual-line-fringe-indicators
      '(nil right-curly-arrow))  ; show right continuation indicator
                                 ;
;; Show indicator for empty lines
(setq-default indicate-empty-lines t)

;; Enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Whitespace mode
(global-whitespace-mode +1)

(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(setq whitespace-display-mappings
      '((newline-mark 10 [?◦ 10])))  ; eol character

(eval-after-load 'whitespace
  (lambda ()
    (set-face-attribute 'whitespace-newline nil :foreground "#383838" :background nil)))

;; Git gutter fringe
(after! git-gutter-fringe
  (set-face-foreground 'git-gutter-fr:modified "#5f5fff")
  (set-face-foreground 'git-gutter-fr:added    "#87ff87")
  (set-face-foreground 'git-gutter-fr:deleted  "#ff005f"))

;; Evil vim modes
(setq
 evil-normal-state-tag   (propertize "N" 'face '((:foreground "DarkGoldenrod2")))
 evil-emacs-state-tag    (propertize "E" 'face '((:foreground "SkyBlue2")))
 evil-insert-state-tag   (propertize "I" 'face '((:foreground "Chartreuse1")))
 evil-replace-state-tag  (propertize "R" 'face '((:foreground "chocolate")))
 evil-motion-state-tag   (propertize "M" 'face '((:foreground "plum3")))
 evil-visual-state-tag   (propertize "V" 'face '((:foreground "red")))
 evil-operator-state-tag (propertize "O" 'face '((:foreground "sandy brown"))))

;; Mini-modeline (merge modeline with the mini-buffer)
(use-package! mini-modeline
  :config
  (setq mini-modeline-enhance-visual nil)
  (setq mini-modeline-display-gui-line nil)
  (setq mini-modeline-r-format
        (list
         '(:eval (propertize                ; Current filename
                  " %b"
                  'help-echo (buffer-file-name)))
         '(vc-mode vc-mode)                 ; Current git branch
         " "
         (propertize "%02l,%02c "           ; Current line and column
                     'help-echo "Line and column index")
         '(:eval (propertize                ; Major Mode
                  "%m"
                  'help-echo "Buffer major mode"))
         '(:eval (when (buffer-modified-p)  ; Modified?
                   (propertize
                    " [Mod]"
                    'help-echo "Buffer has been modified"
                    'face 'font-lock-warning-face)))
         '(:eval (when buffer-read-only     ; Read only?
                   (propertize
                    " [RO]"
                    'help-echo "Buffer is read-only"
                    'face 'font-lock-type-face)))
         '(:eval (propertize                ; Time
                  (format-time-string " %H:%M ")
                  'help-echo (concat (format-time-string "%c; ")
                                     (emacs-uptime "Uptime: %hh"))))
         '(:eval evil-mode-line-tag)))      ; Evil mode

  (mini-modeline-mode t))
