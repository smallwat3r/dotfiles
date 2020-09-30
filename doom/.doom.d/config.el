;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Frame settings
(when (display-graphic-p)
  ;; Title
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Size
  (add-to-list 'default-frame-alist '(width  . 106))
  (add-to-list 'default-frame-alist '(height . 64)))

;; Personnal info
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

;; Some global settings
(setq
 doom-font (font-spec :family "Courier Prime Code" :size 14)
 doom-variable-pitch-font (font-spec :family "Courier Prime" :size 14)
 doom-theme 'doom-dark+
 doom-themes-enable-bold t
 doom-themes-enable-italic t)

;; Line spacing
(setq-default line-spacing 2)

;; Change default UI stuff
(custom-set-faces
 '(hl-line ((t (:background nil))))
 '(default ((t (:background "#000000"))))
 '(fringe ((t (:foreground "magenta"))))
 '(font-lock-comment-face ((t (:slant italic)))))

;; Load bindings
(load! "+bindings")

;; Activate blinking cursor
(blink-cursor-mode 1)

;; My abbreviations
(setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
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
 truncate-string-ellipsis "…"
 scroll-margin 7)

;; Enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Delete all whitespace on save
(add-hook! 'before-save-hook 'delete-trailing-whitespace)

;; Set up default projects folders
(after! projectile
  (setq projectile-sort-order 'recentf)
  (setq projectile-project-search-path
        '("~/dotfiles/" "~/Projects/" "~/Github" "~/Code")))

;; Writeroom font scaling
(add-hook! 'writeroom-mode-hook
  (text-scale-set (if writeroom-mode 1 0)))

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
    '(:seperate company-ispell company-files company-yasnippet))
  )

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        +ivy-buffer-preview t))

;; Dired file explorer
(after! dired
  ;; Sort by date
  (setq dired-listing-switches "-lat"))

(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n  "/" 'dired-narrow-fuzzy))

;; Vterm
(after! vterm
  ;; Auto-quit when exit
  (setq vterm-kill-buffer-on-exit t)

  ;; Terminal font settings
  (add-hook! 'vterm-mode-hook
    (lambda ()
      (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
      (buffer-face-mode t)))

  ;; Cursor behaviour when hitting ESC in evil mode
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
  ;; Python settings
  (add-hook! 'python-mode-hook
    (lambda ()
      (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
      (setq flycheck-pylintrc "~/.config/pylintrc")
      )))

;; Elisp shell
(after! eshell
  ;; Aliases
  (set-eshell-alias!
   "d" "dired $1"
   "c" "clear"
   "sl" "ls"
   "emacs" "find-file $1"
   "qq" "exit"
   ))

;; Evil vim modes
(setq
 evil-normal-state-tag   (propertize "N/" 'face '((:foreground "DarkGoldenrod2")))
 evil-emacs-state-tag    (propertize "E/" 'face '((:foreground "SkyBlue2")))
 evil-insert-state-tag   (propertize "I/" 'face '((:foreground "Chartreuse1")))
 evil-replace-state-tag  (propertize "R/" 'face '((:foreground "chocolate")))
 evil-motion-state-tag   (propertize "M/" 'face '((:foreground "plum3")))
 evil-visual-state-tag   (propertize "V/" 'face '((:foreground "red")))
 evil-operator-state-tag (propertize "O/" 'face '((:foreground "sandy brown"))))

;; Mini-modeline
;; Merge modeline with the mini-buffer
(use-package! mini-modeline
  :init
  ;; Default background color
  (custom-set-faces '(mode-line ((t (:background "#000000")))))
  :config
  ;; Activate mini-modeline
  (mini-modeline-mode t)
  ;; Modeline formatting
  (setq mini-modeline-r-format
        (list
         ;; Show the evil vim mode line
         '(:eval evil-mode-line-tag) " "

         ;; The buffer name; the file name as a tool tip
         '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                             'help-echo (buffer-file-name)))

         ;; Show current Git branch
         '(vc-mode vc-mode) " "

         ;; Line and column
         ;; '%02' to set to 2 chars at least; prevents flickering
         (propertize "%02l" 'face 'font-lock-type-face) ","
         (propertize "%02c" 'face 'font-lock-type-face) " "

         ;; The current major mode for the buffer.
         '(:eval (propertize "%m"
                             'face 'font-lock-string-face
                             'help-echo buffer-file-coding-system)) " "

         ;; Was this buffer modified since the last save?
         '(:eval (when (buffer-modified-p)
                   (concat ","
                           (propertize "Mod"
                                       'face 'font-lock-warning-face
                                       'help-echo "Buffer has been modified"))))

         ;; Is this buffer read-only?
         '(:eval (when buffer-read-only
                   (concat ","
                           (propertize "RO"
                                       'face 'font-lock-type-face
                                       'help-echo "Buffer is read-only")))) " "

         ;; Add the current time, show the complete date and emacs uptime in the tooltip
         '(:eval (propertize (format-time-string "%H:%M")
                             'help-echo
                             (concat (format-time-string "%c; ")
                                     (emacs-uptime "Uptime:%hh"))))
         ))
  )

;; Kubernetes integration
(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

;; Org settings
(setq
 org-ellipsis "↴"
 org-hide-emphasis-markers t
 org-directory "~/org/"
 org-adapt-indentation nil)

;; Use org bullets
(use-package! org-bullets
  :init
  (add-hook! 'org-mode-hook 'org-bullets-mode))

;; Org-journal
(use-package! org-journal
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))
