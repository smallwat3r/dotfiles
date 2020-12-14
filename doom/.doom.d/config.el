;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Frame settings (GUI)
(when (display-graphic-p)
  ;; Title
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Modus vivendi theme
  (use-package! modus-vivendi-theme
    :init
    (delq! t custom-theme-load-path)  ; do not show the default themes
    :config
    (setq modus-vivendi-theme-slanted-constructs t
          ;; modus-vivendi-theme-bold-constructs t
          modus-vivendi-theme-completions 'opinionated
          modus-vivendi-theme-faint-syntax t)
    (load-theme 'modus-vivendi t))

  ;; Size
  (add-to-list 'default-frame-alist '(width  . 106))
  (add-to-list 'default-frame-alist '(height . 64)))

;; Don't show line numbers by default
(setq display-line-numbers-type nil)

;; UK keyboard hash key support
(define-key key-translation-map (kbd "M-3") (kbd "#"))

;; Personnal info
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

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

;; Font settings
(setq
 doom-font (font-spec :family "Monaco Nerd Font" :size 11)
 doom-serif-font (font-spec :family "Courier New")
 doom-variable-pitch-font (font-spec :family "Verdana")
 doom-themes-treemacs-enable-variable-pitch nil)

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Verdana"))
  (buffer-face-mode))

(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'markdown-mode-hook 'my-buffer-face-mode-variable)

;; Line spacing
(setq-default line-spacing 0)

;; Overwrite theme stuff
(custom-set-faces
 ;; '(default ((t (:background "#000000"))))  ; force black bg
 '(mode-line ((t (:background nil :box nil :overline nil :underline nil))))
 ;; '(hl-line ((t (:background nil))))
 '(hl-line ((t (:background "#000000"))))
 '(fringe ((t (:background nil))))
 '(org-ellipsis ((t (:foreground "#00afaf"))))
 '(font-lock-comment-face ((t (:slant italic)))))  ; force italics on comments

;; Load bindings
(load! "+bindings")

;; Choose buffer when splitting the window
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; My abbreviations
(setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Custom file
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Some general settings
(setq
 default-directory "~/"
 undo-limit 80000000
 evil-want-fine-undo t             ; fine grained undo history
 inhibit-compacting-font-caches t  ; improve general perfs
 scroll-margin 7                   ; top and bottom margins to trigger scroll
 which-key-idle-delay 0.5)         ; delay to show key bindings menu

;; Show indicator for empty lines
(setq-default indicate-empty-lines t)

;; Enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Disable global word-wrap in vterm-mode
(add-to-list '+word-wrap-disabled-modes 'vterm-mode)

(setq visual-line-fringe-indicators
      '(nil right-curly-arrow))  ; show right continuation indicator

;; Delete all whitespace on save, except on markdown
(add-hook! 'before-save-hook
  (lambda ()
    (unless (eq major-mode 'markdown-mode)
      (delete-trailing-whitespace))))

;; Git gutter fringe
(after! git-gutter-fringe
  (set-face-foreground 'git-gutter-fr:modified "#5f5fff")
  (set-face-foreground 'git-gutter-fr:added    "#87ff87")
  (set-face-foreground 'git-gutter-fr:deleted  "#ff005f"))

;; Set up default projects folders
(after! projectile
  (setq projectile-ignored-projects
        '("~/" "/tmp" "~/Downloads"))
  (setq projectile-sort-order 'recentf)
  (setq projectile-project-search-path
        '("~/dotfiles/" "~/Projects/" "~/Code/" "~/Github/")))

;; Writeroom font scaling
(add-hook! 'writeroom-mode-hook
  (text-scale-set (if writeroom-mode 1 0)))

;; ;; Auto-activate writeroom on text-mode
;; (add-hook! 'text-mode-hook writeroom-mode)

;; OS executables
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l"))  ; disable annoying warning
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Function to command names in minibuffer as they are being used, hide obvious commands
(defun my-echo-command-name-hook()
  (unless (or (eq this-command 'self-insert-command)
              (eq this-command 'evil-backward-char)
              (eq this-command 'evil-forward-char)
              (eq this-command 'scroll-up-line)
              (eq this-command 'scroll-down-line)
              (eq this-command 'previous-line)
              (eq this-command 'next-line))
    (message "%s" this-command)))

;; (add-hook! 'post-command-hook 'my-echo-command-name-hook)

;; Completion stuff
(after! company
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2)

  (add-hook! 'evil-normal-state-entry-hook #'company-abort)

  ;; Use tab to go through the choices
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

  (set-company-backend! '(text-mode markdown-mode gfm-mode)
    '(:seperate company-ispell company-files company-yasnippet)))

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        +ivy-buffer-preview t))

;; Dired file explorer
(after! dired
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-lat"))  ; sort by date

(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map :n "/" 'dired-narrow-fuzzy))

;; Vterm
(after! vterm
  ;; Auto-quit when exit
  (setq vterm-kill-buffer-on-exit t)

  ;; ;; Terminal font settings
  ;; (add-hook 'vterm-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch-serif)
  ;;             (buffer-face-mode t)))

  ;; Cursor behaviour when hitting ESC in evil mode
  (defun evil-collection-vterm-escape-stay ()
    (setq-local evil-move-cursor-back nil))

  ;; Scrolling
  (evil-define-key 'insert vterm-mode-map (kbd "C-j") 'scroll-up-line)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k") 'scroll-down-line)
  (evil-define-key 'normal vterm-mode-map (kbd "C-j") 'scroll-up-line)
  (evil-define-key 'normal vterm-mode-map (kbd "C-k") 'scroll-down-line)

  ;; Insert mode
  (evil-define-key 'normal vterm-mode-map (kbd "i")        'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") 'evil-insert-resume)

  ;; Delete the previous word
  (define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))

  ;; Terminal pop up settings
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.60
    :vslot -4
    :select t
    :quit nil
    :ttl 0
    :side 'bottom))

;; Python stuff
(add-hook! python-mode
  (setq python-shell-interpreter
        "/usr/local/opt/python@3.8/bin/python3.8"))

;; Static code analysis
(after! flycheck
  (add-hook! 'python-mode-hook  ; python stuff
    (lambda ()
      (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
      (setq flycheck-pylintrc "~/.config/pylintrc"))))

;; Elisp shell aliases
(after! eshell
  (set-eshell-alias!
   "d" "dired $1"
   "c" "clear"
   "sl" "ls"
   "emacs" "find-file $1"
   "e" "find-file $1"
   "qq" "exit"))

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

;; Kubernetes integration
(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

;; Org settings
(setq
 org-directory "~/org/"
 org-ellipsis " ↴ "
 org-hide-emphasis-markers t  ; hide mark up symbols (eg. *bold*)
 org-adapt-indentation nil)   ; indentation of text below headlines

;; Org-journal
(use-package! org-journal
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))

;; Abilitiy to use `ciq' `yiq' etc in normal mode (literally "Inside Quotes")
;; (Credits to @Flo from the doom emacs discord channel)
(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote))

;; Insert hex color
;; (https://emacs.stackexchange.com/a/5583)
(defun zz/insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

With prefix argument \\[universal-argument] insert the 48-bit value."
  (interactive "*P")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
                (interactive)
                (quit-window)
                (with-current-buffer ,buf
                  (insert (apply #'color-rgb-to-hex
                                 (nconc (color-name-to-rgb name)
                                        (unless (consp ',arg)
                                          (list (or ,arg 2)))))))))))
