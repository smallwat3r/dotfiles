;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Ensure env variables inside Emacs are the same than in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Initial window size when Emacs starts
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 144) (height . 33)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

(setq default-directory "~/")
(setq org-directory "~/org/")

;; ui stuff
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'Regular)
      doom-theme 'doom-outrun-electric
      doom-themes-enable-bold nil)

(setq display-line-numbers-type nil)

(custom-set-faces
  '(font-lock-comment-face ((t (:inherit 'fixed-pitch-serif))))
  '(default ((t (:background "black"))))
  '(mode-line ((t (:inherit 'fixed-pitch-serif ))))
  '(mode-line-inactive ((t (:inherit 'fixed-pitch-serif)))))

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
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

;; python
(setq python-shell-interpreter "/usr/local/opt/python@3.8/bin/python3.8")

;; vterm
(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch-serif)
                 (buffer-face-mode t)))

;; ivy
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")
