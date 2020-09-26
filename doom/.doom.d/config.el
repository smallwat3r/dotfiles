;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 144) (height . 33)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

(setq doom-font (font-spec :family "Menlo" :size 12 :weight 'Regular)
      doom-theme 'doom-outrun-electric
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

(map!  "C-j"   #'scroll-up-line
       "C-k"   #'scroll-down-line

       (:map override
        "M-h"  #'windmove-left
        "M-l"  #'windmove-right
        "M-k"  #'windmove-up
        "M-j"  #'windmove-down
        "M-3"  "#")  ;; macOS Uk keyboard hack

       (:map evil-normal-state-map
        ";w"   #'evil-write
        ";q"   #'evil-save-and-close
        ";x"   #'evil-save-and-close
        ";vs"  #'split-window-horizontally
        ";sp"  #'split-window-vertically))

(custom-set-faces
 '(default ((t (:background "black"))))
 '(font-lock-comment-face ((t (:slant italic)))))

(add-hook! python-mode
  (setq python-shell-interpreter
        "/usr/local/opt/python@3.8/bin/python3.8"))

(setq abbrev-file-name
      "~/.doom.d/abbrev.el")
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq default-directory "~/"
      display-line-numbers-type nil
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      undo-limit 80000000
      evil-want-fine-undo t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

(global-visual-line-mode t)

(add-hook! 'before-save-hook 'delete-trailing-whitespace)

(after! projectile
  (setq projectile-sort-order 'recentf)
  (setq projectile-project-search-path
        '("~/dotfiles/" "~/Projects/" "~/Github" "~/Code")))

(use-package! key-chord
  :config
  (setq key-chord-two-keys-delay 0.5)
  ;; Make jj to work as ESC in insert mode
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package! format-all
  :config
  :bind ((:map evil-normal-state-map
          (";f" . 'format-all-buffer))))

(after! company
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2)

  (add-hook! 'evil-normal-state-entry-hook #'company-abort)

  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

  (set-company-backend! '(text-mode markdown-mode gfm-mode)
    '(:seperate company-ispell company-files company-yasnippet))
  )

(after! vterm
  (setq vterm-kill-buffer-on-exit t)

  (add-hook! 'vterm-mode-hook
    (lambda ()
      (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
      (buffer-face-mode t)))

  (defun evil-collection-vterm-escape-stay ()
    (setq-local evil-move-cursor-back nil))

  (add-hook! 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)

  (define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))

  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.60
    :vslot -4
    :select t
    :quit nil
    :ttl 0
    :side 'bottom))

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        +ivy-buffer-preview t))

(after! flycheck
  (add-hook! 'python-mode-hook
    (lambda ()
      (setq flycheck-python-pylint-executable
            "/usr/local/bin/pylint")
      (setq flycheck-pylintrc
            "~/.config/pylintrc")))
  )

(after! eshell
  ;; remember to run eshell-read-aliases-list from the eshell to reload cache
  ;; in case the alias file path has changed
  (setq eshell-aliases-file
        "~/.doom.d/eshell/aliases"))

(use-package! mini-modeline
  :init
  (custom-set-faces
   '(mode-line ((t (:background "black")))))
  :config
  (mini-modeline-mode t)
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

    ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                        'face 'font-lock-preprocessor-face
                        'help-echo (concat "Buffer is in "
                                           (if overwrite-mode "overwrite" "insert") " mode")))

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

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(use-package! org-bullets
  :init
  (add-hook! 'org-mode-hook 'org-bullets-mode))

(setq org-ellipsis "⤵")
(setq org-hide-emphasis-markers t)

(setq org-directory "~/org/")
(setq org-adapt-indentation nil)

;; Current time and date bindings
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
