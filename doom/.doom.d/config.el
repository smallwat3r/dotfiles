;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 144) (height . 33)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com"

      doom-font (font-spec :family "Monaco" :size 12 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "Arial")
      doom-serif-font (font-spec :family "Consolas")

      doom-theme 'doom-outrun-electric
      doom-themes-enable-bold t
      doom-themes-enable-italic t

      default-directory "~/"
      display-line-numbers-type nil
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

(custom-set-faces
 '(font-lock-comment-face ((t (:slant italic :inherit 'fixed-pitch-serif))))
 '(default ((t (:background "black")))))

(global-visual-line-mode 1)

(setq org-directory "~/org/"
      org-adapt-indentation nil)

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package! projectile
  :config
  (projectile-add-known-project "~/dotfiles")
  (projectile-add-known-project "~/Projects")
  (projectile-add-known-project "~/Github")
  (projectile-add-known-project "~/Code"))

;; general settings
(setq-default delete-by-moving-to-trash t
              tab-width 4
              uniquify-buffer-name-style 'forward
              x-stretch-cursor t)

(setq undo-limit 80000000
      evil-want-fine-undo t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

(setq python-shell-interpreter "/usr/local/opt/python@3.8/bin/python3.8")

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(map!
 "M-3" "#"  ;; macOS Uk keyboard fix

 "M-h" #'windmove-left
 "M-l" #'windmove-right
 "M-k" #'windmove-up
 "M-j" #'windmove-down

 "C-j" #'scroll-up-line
 "C-k" #'scroll-down-line)

(use-package! evil
  :demand t
  :custom
  (evil-esc-delay 0.001 "avoid ESC/meta mixups")
  (evil-shift-width 4)
  (evil-search-module 'evil-search)
  :bind ((:map evil-normal-state-map
          (";w" . 'evil-write)
          (";q" . 'evil-save-and-close)
          (";x" . 'evil-save-and-close)
          (";vs" . 'split-window-horizontally)
          (";sp" . 'split-window-vertically)))
  :config
  ;; jj to work as esc
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package! format-all
  :after evil
  :bind ((:map evil-normal-state-map
          (";f" . 'format-all-buffer))))

(use-package! company
  :after evil
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)

  (add-hook 'evil-normal-state-entry-hook #'company-abort)

  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

  (set-company-backend! '(text-mode markdown-mode gfm-mode)
    '(:seperate company-ispell company-files company-yasnippet)))

(use-package! vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch-serif)
              (buffer-face-mode t))))

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.60
    :vslot -4
    :select t
    :quit nil
    :ttl 0
    :side 'bottom))

(use-package! ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (setq +ivy-buffer-preview t))

(use-package! flycheck
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
              (setq flycheck-pylintrc "~/.config/pylintrc"))))

(use-package! eshell
  :config
  ;; remember to run eshell-read-aliases-list from the eshell to reload cache
  ;; in case the alias file path has changed
  (setq eshell-aliases-file "~/.doom.d/eshell/aliases"))

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
    )))
