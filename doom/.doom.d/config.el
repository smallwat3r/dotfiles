;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Ensure env variables inside Emacs are the same than in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Initial window size when Emacs starts
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 144) (height . 33)))

;; Projects
(projectile-add-known-project "~/dotfiles")
(projectile-add-known-project "~/Projects")
(projectile-add-known-project "~/Github")
(projectile-add-known-project "~/Code")

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

(setq default-directory "~/")
(setq org-directory "~/org/")

;; ui stuff
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'Regular)
      doom-theme 'doom-outrun-electric
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq display-line-numbers-type nil)

(custom-set-faces
  '(font-lock-comment-face ((t (:slant italic :inherit 'fixed-pitch-serif))))
  '(default ((t (:background "black"))))
  '(mode-line ((t (:inherit 'fixed-pitch-serif ))))
  '(mode-line-inactive ((t (:inherit 'fixed-pitch-serif)))))

(setq undo-limit 80000000
      evil-want-fine-undo t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(display-time-mode 1)

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
