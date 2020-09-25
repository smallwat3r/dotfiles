;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ensure env variables inside Emacs are the same than in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; initial window size when Emacs starts
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 144) (height . 33)))

;; projectile
(projectile-add-known-project "~/dotfiles")
(projectile-add-known-project "~/Projects")
(projectile-add-known-project "~/Github")
(projectile-add-known-project "~/Code")

;; personal stuff
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

(setq default-directory "~/")
(setq org-directory "~/org/")

;; ui stuff
(setq doom-font (font-spec :family "Hack" :size 12 :weight 'Regular)
      doom-theme 'doom-outrun-electric
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; show line wrap indicators
(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; do not show line numbers
(setq display-line-numbers-type nil)

;; comments and global bg
(custom-set-faces
  '(font-lock-comment-face ((t (:slant italic :inherit 'fixed-pitch-serif))))
  '(default ((t (:background "black")))))

;; general settings
(setq undo-limit 80000000
      evil-want-fine-undo t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

;; display battery in modeline
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

;; display time in modeline
(display-time-mode 1)

;; telephone-line
(setq telephone-line-height 15
      telephone-line-evil-use-short-tag t)

(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

(telephone-line-mode 1)

;; fix issue on macos uk keyboard for # char
(global-set-key (kbd "M-3") "#")

;; delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; scroll
(global-set-key (kbd "C-j") 'scroll-up-line)
(global-set-key (kbd "C-k") 'scroll-down-line)

;; switch windows
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

;; jj to work as esc
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;; vim-like shortcuts
(define-key evil-normal-state-map ";w" 'evil-write)
(define-key evil-normal-state-map ";q" 'evil-save-and-close)
(define-key evil-normal-state-map ";x" 'evil-save-and-close)

;; split windows
(define-key evil-normal-state-map ";vs" 'split-window-horizontally)
(define-key evil-normal-state-map ";sp" 'split-window-vertically)

;; resize window
(global-set-key (kbd "S-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-j") 'shrink-window)
(global-set-key (kbd "S-C-k") 'enlarge-window)

;; company
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
(add-hook 'evil-normal-state-entry-hook #'company-abort))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(set-company-backend! '(text-mode markdown-mode gfm-mode)
  '(:seperate company-ispell company-files company-yasnippet))

;; python
(setq python-shell-interpreter "/usr/local/opt/python@3.8/bin/python3.8")

;; vterm
(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch-serif)
                 (buffer-face-mode t)))

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.60 :vslot -4 :select t :quit nil :ttl 0 :side 'bottom))

;; ivy
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")

;; auto-format
(define-key evil-normal-state-map ";f" 'format-all-buffer)
