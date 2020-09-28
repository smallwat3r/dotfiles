;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Frame title
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Change default UI stuff
(custom-set-faces
 '(default ((t (:background "#080808"))))
 '(hl-line ((t (:background nil))))
 '(fringe ((t (:foreground "magenta"))))
 '(font-lock-comment-face ((t (:slant italic)))))

;; Initialise frame size at start-up
(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (width . 106)
            (height . 60)
            (left . 50)
            (top . 50))))

;; Load bindings
(load! "+bindings")

;; Personnal info
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

;; Some global settings
(setq
 doom-font (font-spec :family "Luculent 12" :size 12 :weight 'Regular)
 doom-variable-pitch-font (font-spec :family "Go Mono" :size 12)
 doom-theme 'doom-laserwave
 doom-themes-enable-bold t
 doom-themes-enable-italic t)

;; Activate blinking cursor
(blink-cursor-mode 1)

;; My abbreviations
(setq abbrev-file-name "~/.doom.d/abbrev.el")
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Custom file
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Some other general settings
(setq
 default-directory "~/"
 display-line-numbers-type nil
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 undo-limit 80000000
 evil-want-fine-undo t
 inhibit-compacting-font-caches t
 truncate-string-ellipsis "…")

;; Enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Fix annoying lsp pop up error
(setq lsp-restart 'ignore)

;; Delete all whitespace on save
(add-hook! 'before-save-hook 'delete-trailing-whitespace)

;; Set up default projects folders
(after! projectile
  (setq projectile-sort-order 'recentf)
  (setq projectile-project-search-path
        '("~/dotfiles/" "~/Projects/" "~/Github" "~/Code")))

;; Make jj to trigger ESC in insert mode, with a time delay in case I
;; do need to type in jj ...
(use-package! key-chord
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

;; OS executables
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Completion stuff related
(after! company
  (setq company-idle-delay 0
        company-tooltip-limit 10
        company-minimum-prefix-length 1)

  (add-hook! 'evil-normal-state-entry-hook #'company-abort)

  ;; Use tab to go through the choices
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

  (set-company-backend! '(text-mode markdown-mode gfm-mode)
    '(:seperate company-ispell company-files company-yasnippet))
  )

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        +ivy-buffer-preview t))

;; Vterm
(after! vterm
  (setq vterm-kill-buffer-on-exit t)

  ;; Terminal font
  (add-hook! 'vterm-mode-hook
    (lambda ()
      (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
      (buffer-face-mode t)))

  ;; Behaviour when hitting ESC in evil mode
  (defun evil-collection-vterm-escape-stay ()
    (setq-local evil-move-cursor-back nil))

  (add-hook! 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

  ;; Use of C-c is needed in the terminal, so prioritise it
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)

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
    :side 'bottom)
  )

;; Python stuff
(add-hook! python-mode
  (setq python-shell-interpreter
        "/usr/local/opt/python@3.8/bin/python3.8"))

;; Static code analysis
(after! flycheck
  (add-hook! 'python-mode-hook
    (lambda ()
      (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
      (setq flycheck-pylintrc "~/.config/pylintrc")
      )))

;; Elisp shell
(after! eshell
  (set-eshell-alias!
   "d" "dired $1"
   "c" "clear"
   "sl" "ls"
   "emacs" "find-file $1"
   "qq" "exit"
   ))

;; Merge modeline with the mini-buffer
(use-package! mini-modeline
  :init
  ;; Initialise modeline default background color
  (custom-set-faces
   '(mode-line ((t (:background "#080808")))))
  :config
  (mini-modeline-mode t)
  ;; Modeline formatting
  (setq-default
   mini-modeline-r-format
   (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                        'help-echo (buffer-file-name)))

    ;; show current Git branch
    '(vc-mode vc-mode) " "

    ;; line and column
    ;; '%02' to set to 2 chars at least; prevents flickering
    (propertize "%02l" 'face 'font-lock-type-face) ","
    (propertize "%02c" 'face 'font-lock-type-face) " "

    ;; the current major mode for the buffer.
    '(:eval (propertize "%m"
                        'face 'font-lock-string-face
                        'help-echo buffer-file-coding-system)) " "

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","
                      (propertize "Mod"
                                  'face 'font-lock-warning-face
                                  'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","
                      (propertize "RO"
                                  'face 'font-lock-type-face
                                  'help-echo "Buffer is read-only")))) " "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
                        'help-echo
                        (concat (format-time-string "%c; ")
                                (emacs-uptime "Uptime:%hh")))) " --"
    ))
  )

;; Kubernetes integration
(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(use-package! org-bullets
  :init (add-hook! 'org-mode-hook 'org-bullets-mode))

;; Org settings
(setq org-ellipsis "⤵")
(setq org-hide-emphasis-markers t)

(setq org-directory "~/org/")
(setq org-adapt-indentation nil)

;; Current time and date bindings and functions
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key (kbd "C-c d") 'insert-current-date-time)
(global-set-key (kbd "C-c t") 'insert-current-time)
