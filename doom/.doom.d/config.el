;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+bindings")
(load! "+functions")

;; Disable confirmation when exiting Emacs
(setq confirm-kill-emacs nil)

;; Hack for hash key support, on UK macOS keyboard, M-3 wouldn't print a hash (#)
(define-key key-translation-map (kbd "M-3") (kbd "#"))

;; Personal info
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

;; Email stuff. We are using msmtp to send emails from Emacs
(setq mail-user-agent 'message-user-agent)
(setq mail-specify-envelope-from t)
(setq sendmail-program "/usr/local/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; My abbreviations. These are stored in a file named abbrev.el
(setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Custom file. File used by Emacs to cache some data relative to the config
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Some general settings
(setq
 evil-vsplit-window-right t
 evil-split-window-below t
 default-directory "~/"
 undo-limit 80000000
 evil-want-fine-undo t             ; fine grained undo history
 inhibit-compacting-font-caches t  ; improve general perfs
 scroll-margin 7                   ; top and bottom margins to trigger scroll
 which-key-idle-delay 0.5)         ; delay to show key bindings menu

;; Delete all whitespace on save, except on markdown-mode
(add-hook! 'before-save-hook
  (lambda ()
    (unless (eq major-mode 'markdown-mode)
      (delete-trailing-whitespace))))

;; Projectile. Setup default project folders.
(after! projectile
  (setq projectile-sort-order 'recentf)
  (setq projectile-ignored-projects
        '("~/" "/tmp" "~/Downloads"))
  (setq projectile-project-search-path
        '("~/dotfiles/" "~/Projects/" "~/Code/" "~/Github/")))

;; Writeroom font scaling
(add-hook! 'writeroom-mode-hook
  (text-scale-set (if writeroom-mode 1 0)))

;; ;; Auto-activate writeroom on text-mode
;; (add-hook! 'text-mode-hook writeroom-mode)

;; Adds binaries to PATH, so we can use them from Emacs as it works from the shell
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
;; Uncomment the below line to use the function above
;; (add-hook! 'post-command-hook 'my-echo-command-name-hook)

;; Completion
(after! company
  (setq company-idle-delay 0.1            ; Add minimal delay
        company-tooltip-limit 10          ; Dropdown of 10 lines long
        company-minimum-prefix-length 2)  ; Needs >2 chars before showing

  (add-hook! 'evil-normal-state-entry-hook #'company-abort)  ; Hack

  ;; Deactivate on sh-mode, as it seems to slow things down drastically when writing
  ;; bash scripts. FIXME: Investigate what is causing this? Is it really needed?
  (setq company-global-modes '(not sh-mode))

  ;; Use tab to go through the choices
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

  ;; Set specific backends for text stuff
  (set-company-backend! '(text-mode markdown-mode gfm-mode)
    '(:seperate company-ispell company-files company-yasnippet)))

;; Ivy
(after! ivy
  ;; Popup to choose buffer when splitting the window
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (+ivy/switch-buffer))

  ;; Default ivy settings
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        +ivy-buffer-preview t))

;; Use a specific window to pop-up Ivy
;; (after! ivy-posframe
;;   (setq ivy-posframe-border-width 5)
;;   (setq ivy-posframe-display-functions-alist
;;         '((t . ivy-posframe-display-at-frame-center))))

;; Dired file explorer
(after! dired
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-lat"))  ; sort by date

;; Adds narrow fuzzy search functionality to Dired
(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map :n "/" 'dired-narrow-fuzzy))

;; Vterm. The default shell I used in Emacs
(after! vterm
  (setq vterm-kill-buffer-on-exit t)  ; kill buffer on exiting the term

  ;; Terminal font settings
  ;; (add-hook 'vterm-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch-serif)
  ;;             (buffer-face-mode t)))

  ;; Cursor behaviour when hitting ESC in evil mode
  (defun evil-collection-vterm-escape-stay ()
    (setq-local evil-move-cursor-back nil))

  ;; Scrolling
  (evil-define-key 'insert vterm-mode-map (kbd "C-j") #'scroll-up-line)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k") #'scroll-down-line)
  (evil-define-key 'normal vterm-mode-map (kbd "C-j") #'scroll-up-line)
  (evil-define-key 'normal vterm-mode-map (kbd "C-k") #'scroll-down-line)

  ;; Ctrl-C behaviour. Stop current command automatically
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)

  ;; Enter in insert mode
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)

  ;; Delete the previous word
  (define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))

  ;; Terminal pop-up settings
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.60
    :vslot -4
    :select t
    :quit nil
    :ttl 0
    :side 'bottom))

;; Python configuration stuff
(add-hook! python-mode
  (setq python-shell-interpreter
        "/usr/local/opt/python@3.8/bin/python3.8"))

;; Static code analysis
(after! flycheck
  ;; Python stuff. Use Pylint by default.
  (add-hook! 'python-mode-hook
    (lambda ()
      (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
      (setq flycheck-pylintrc "~/.config/pylintrc"))))

;; Spell checker
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default delay is 0.25

;; Bash formatter settings (shfmt)
(set-formatter! 'shfmt "shfmt -i 2 -ci")

;; Elisp shell aliases
(after! eshell
  (set-eshell-alias!
   "d" "dired $1"
   "c" "clear"
   "sl" "ls"
   "emacs" "find-file $1"
   "e" "find-file $1"
   "qq" "exit"))

;; Kubernetes integration
(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

;; Email configuration. We use Notmuch to manage emails in Emacs
(after! notmuch
  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))
  ;; Use mbsync to synchronise emails
  (setq +notmuch-sync-backend 'mbsync)
  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        '(("mpetiteau.pro@gmail.com" . "personal/sent -inbox +sent -unread")
          ("matthieu@smallwatersolutions.com" . "sws/sent -inbox +sent -unread"))))

;; Add emoji support
(use-package! emojify
  :hook (after-init . global-emojify-mode))

;; Deft (notes)
(after! deft
  (setq deft-recursive t  ; Search recursively from the deft-directory
        deft-directory "~/org"))

;; Org settings
(setq
 org-directory "~/org/"
 org-ellipsis " … "
 org-hide-emphasis-markers t  ; hide mark up symbols (eg. *bold*)
 org-adapt-indentation nil)   ; indentation of text below headlines

;; Org-journal
(use-package! org-journal
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y%m%d.org"))

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
