;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Default frame settings
(when (display-graphic-p)
  ;; Bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Size
  (add-to-list 'default-frame-alist '(width  . 106))
  (add-to-list 'default-frame-alist '(height . 64)))

;; Disable line numbers by default
(setq display-line-numbers-type nil)

;; Set window dividers width
(defvar global-window-divider-width 2
  "Default global width size of a window divider.")

(setq window-divider-default-right-width global-window-divider-width
      window-divider-default-bottom-width global-window-divider-width)

;; Do not change the divider border width when using writeroom
(setq +zen-window-divider-size global-window-divider-width)

;; Writeroom font scaling
(setq +zen-text-scale 1)

;; ;; Auto-activate writeroom on text-mode
;; (add-hook! 'text-mode-hook writeroom-mode)

;; Set up frame title. It shows the title of the current file and an
;; indicator if the file has been modified eg. (+)
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (replace-regexp-in-string
              ".*/[0-9]*-?" " "
              (subst-char-in-string ?_ ? buffer-file-name)) "%b"))
        (:eval
         (if (buffer-modified-p) " (+)"))))

;; Hide file icon from frame window
(setq ns-use-proxy-icon nil)

;; Themes setup
(use-package! modus-vivendi-theme  ; dark theme
  :init
  (setq
   modus-vivendi-theme-slanted-constructs nil
   modus-vivendi-theme-bold-constructs t
   modus-vivendi-theme-intense-hl-line nil
   modus-vivendi-theme-subtle-diffs t
   modus-vivendi-theme-intense-paren-match 'intense-bold
   modus-vivendi-theme-org-blocks 'rainbow
   modus-vivendi-theme-completions 'opinionated
   modus-vivendi-theme-faint-syntax t)
  :config
  ;; Override colors
  (modus-vivendi-theme-with-color-variables
    (custom-theme-set-faces! 'modus-vivendi
      `(hl-line :background "#000000")
      ))
  )

(use-package! modus-operandi-theme  ; light theme
  :init
  (setq
   modus-operandi-theme-slanted-constructs nil
   modus-operandi-theme-bold-constructs t
   modus-operandi-theme-intense-hl-line nil
   modus-operandi-theme-subtle-diffs t
   modus-operandi-theme-intense-paren-match 'intense-bold
   modus-operandi-theme-org-blocks 'rainbow
   modus-operandi-theme-completions 'opinionated)
  :config
  ;; Override colors
  (modus-operandi-theme-with-color-variables
    (custom-theme-set-faces! 'modus-operandi
      `(default :background "#efefd8")
      `(term :background "#efefd8")
      `(hl-line :background "#efefd8")
      ))

  ;; HACK: Change default background color when using vterm within modus-operandi.
  ;; Changing it by setting vterm-color-default above doesn't seems to work anymore.
  (add-hook 'vterm-mode-hook
            (lambda()
              (when (string= doom-theme "modus-operandi")
                (set (make-local-variable 'buffer-face-mode-face)
                     '(:background "#e3e3c5"))
                (buffer-face-mode t))))
  )

;; Do not show unwanted themes
(delq! t custom-theme-load-path)

;; Set up our default theme
(setq doom-theme 'modus-operandi)

;; Minimal dashboard menu
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

(setq +doom-dashboard-menu-sections
      '(("Open project"
         :action projectile-switch-project)
        ("Recently opened files"
         :action recentf-open-files)
        ("Reload last session"
         :when (cond ((require 'persp-mode nil t)
                      (file-exists-p
                       (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("Open private configuration"
         :when (file-directory-p doom-private-dir)
         :action doom/open-private-config)
        ("Open documentation"
         :action doom/help)))

;; Font faces
(setq
 doom-font (font-spec :family "Monaco Nerd Font" :size 13)
 ;; doom-font (font-spec :family "Recursive Monospace Casual" :size 13)
 doom-variable-pitch-font (font-spec :family "Verdana"))

;; Steps used to increment fonts (default is 2)
(setq doom-font-increment 1)

;; Increment of 3 points in big-font-mode (default is 4)
(setq doom-big-font-increment 3)

;; Use default emacs font for treemacs
(setq doom-themes-treemacs-enable-variable-pitch nil)

(defun zz/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Verdana"))
  (buffer-face-mode))

;; Change default frame font in some specific modes
(add-hook 'org-mode-hook 'zz/buffer-face-mode-variable)
(add-hook 'markdown-mode-hook 'zz/buffer-face-mode-variable)
(add-hook 'notmuch-show-mode-hook 'zz/buffer-face-mode-variable)
(add-hook 'notmuch-message-mode-hook 'zz/buffer-face-mode-variable)

;; No extra line spacing
(setq-default line-spacing nil)

;; Disable hl-line-mode
(remove-hook! (prog-mode text-mode conf-mode special-mode) #'hl-line-mode)

;; Do not override the color of rainbow-mode with hl-line-mode.
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

;; Overwrite some global theme stuff
(custom-set-faces!
  ;; We use mini-modeline (merge modeline in minibuffer) so we want to keep
  ;; our modeline as invisible and clean as possible.
  '(mode-line :background nil :box nil :overline nil :underline nil)

  ;; Line numbers (when on)
  '(line-number :background nil :foreground "#3b3b3b" :height 100)
  '(line-number-current-line :background nil :height 100)

  ;; Whitespace newline symbol
  '(whitespace-newline :background nil :inherit font-lock-comment-face)

  ;; Comments and docstrings font face
  ;; '(font-lock-comment-face :inherit variable-pitch)
  ;; '(font-lock-doc-face :inherit variable-pitch)
  )

;; Show visual indicators for line continuation in fringes
;; FIXME: Doesn't seems to work anymore for some reason...
;; (setq visual-line-fringe-indicators
;;       '(nil right-curly-arrow))  ; show in right fringe only

;; Show indicator for empty lines (eg. the tildes in vim after eof)
;; (setq-default indicate-empty-lines t)

;; Enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Disable global by default word-wrap in a few modes
(add-to-list '+word-wrap-disabled-modes 'vterm-mode)
(add-to-list '+word-wrap-disabled-modes 'notmuch-search-mode)

;; Terminal line wrap symbols
(set-display-table-slot standard-display-table 'truncation ?›)
(set-display-table-slot standard-display-table 'wrap ?↵)

;; Whitespace mode
;; (global-whitespace-mode +1)  ; turns on whitespace mode globally
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(setq whitespace-display-mappings
      '((newline-mark 10 [?◦ 10])))  ; eol character

;; Git gutter fringe.
(after! git-gutter-fringe
  ;; Set default fringe colors based on action
  (set-face-foreground 'git-gutter-fr:modified "yellow")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red"))

;; Evil vim modes faces text representation and colors
(setq
 evil-normal-state-tag   (propertize "N" 'face '((:foreground "DarkGoldenrod2")))
 evil-emacs-state-tag    (propertize "E" 'face '((:foreground "SkyBlue2")))
 evil-insert-state-tag   (propertize "I" 'face '((:foreground "green")))
 evil-replace-state-tag  (propertize "R" 'face '((:foreground "chocolate")))
 evil-motion-state-tag   (propertize "M" 'face '((:foreground "plum3")))
 evil-visual-state-tag   (propertize "V" 'face '((:foreground "red")))
 evil-operator-state-tag (propertize "O" 'face '((:foreground "sandy brown"))))

;; Mini-modeline (merge modeline with the mini-buffer)
(use-package! mini-modeline
  :init
  ;; Turn off some default settings, like to keep it as clean as possible.
  (setq mini-modeline-enhance-visual nil)
  (setq mini-modeline-display-gui-line nil)

  ;; Keep modeline information on the right side of the mini-buffer so it still
  ;; has enough space to display useful information on the left side (eg. commands
  ;; information, echos, documentation etc).
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
                  (format-time-string " %a %b %d %H:%M ")
                  'help-echo (concat (format-time-string "%c; week %V; ")
                                     (emacs-uptime "Uptime: %hh"))))
         '(:eval evil-mode-line-tag)))      ; Evil mode
  :config
  ;; Activate mini-modeline
  (mini-modeline-mode t))
