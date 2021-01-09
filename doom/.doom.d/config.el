;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load configs
(load! "+ui")
(load! "+bindings")
(load! "+functions")

;; Disable confirmation when exiting Emacs
(setq confirm-kill-emacs nil)

;; Echo the command names in minibuffer as they are being used
;; (add-hook! 'post-command-hook 'zz/echo-command-name)

;; Personal info
(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com")

;; Some general settings
(setq
 evil-vsplit-window-right t
 evil-split-window-below t
 default-directory "~/"
 undo-limit 80000000
 evil-want-fine-undo t             ; fine grained undo history
 inhibit-compacting-font-caches t  ; improve general perfs
 scroll-margin 7                   ; top and bottom margins to trigger scroll
 which-key-idle-delay 0.5          ; delay to show key bindings menu
 )

;; My abbreviations. These are stored in a file named abbrev.el
(setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Custom file. File used by Emacs to cache some data relative to the config
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; Adds binaries to PATH, so we can use them from Emacs as it works from the shell
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments '("-l"))  ; disable annoying warning
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  :config
  (exec-path-from-shell-initialize))

;; Auto add headers on scratch buffers in specific modes
(add-hook! 'org-mode-hook (zz/add-scratch-buffer-header "#+TITLE: Scratch file"))
(add-hook! 'sh-mode-hook (zz/add-scratch-buffer-header "#!/usr/bin/env bash"))

;; Completion
(after! company
  (setq +lsp-company-backends
        '(:separate company-yasnippet company-capf))

  (setq company-idle-delay 0.1            ; Add minimal delay
        company-tooltip-limit 10          ; Dropdown of 10 lines long
        company-minimum-prefix-length 2)  ; Needs >2 chars before showing

  (add-hook! 'evil-normal-state-entry-hook #'company-abort)  ; Make aborting less annoying

  ;; Use tab and shift-tab to go through the choices
  (map! :map company-active-map "TAB"       #'company-complete-common-or-cycle)
  (map! :map company-active-map "<tab>"     #'company-complete-common-or-cycle)
  (map! :map company-active-map "S-TAB"     #'company-select-previous)
  (map! :map company-active-map "<backtab>" #'company-select-previous)
  )

;; Python company backend
(after! python-mode
  (set-company-backend! 'python-mode
    '(company-capf :separate company-yasnippet)))

;; Javascript company backend
(after! js2-modea
  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

;; Bash company backend
(after! sh-script
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet)))

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
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat"  ; sort by date
        ))

;; Adds narrow fuzzy search functionality to Dired
(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map :n "/" #'dired-narrow-fuzzy))

;; Vterm. The default shell I use in Emacs
(after! vterm
  ;; 6000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 6000)

  ;; Scrolling
  (map! :map vterm-mode-map :i "C-j" #'scroll-up-line)
  (map! :map vterm-mode-map :i "C-k" #'scroll-down-line)
  (map! :map vterm-mode-map :n "C-j" #'scroll-up-line)
  (map! :map vterm-mode-map :n "C-k" #'scroll-down-line)

  ;; Ctrl-C behaviour. Stop current command automatically
  (map! :map vterm-mode-map :i "C-c" #'vterm--self-insert)

  ;; Automatically enter in insert mode
  (map! :map vterm-mode-map :n "i"        #'evil-insert-resume)
  (map! :map vterm-mode-map :n "o"        #'evil-insert-resume)
  (map! :map vterm-mode-map :n "<return>" #'evil-insert-resume)

  ;; Delete the previous word
  (map! :map vterm-mode-map "<C-backspace>"
        (lambda ()
          (interactive) (vterm-send-key (kbd "C-w"))))
  )

;; Python configuration stuff
(add-hook! python-mode
  (setq python-shell-interpreter
        "/usr/local/opt/python@3.9/bin/python3.9"))

;; Static code analysis
(after! flycheck
  ;; Pylint configs
  (setq flycheck-python-pylint-executable "/usr/local/bin/pylint"
        flycheck-pylintrc "~/.config/pylintrc")
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-checker 'python-pylint)))
  ;; Shellcheck configs
  (setq flycheck-shellcheck-excluded-warnings '("SC1091"))
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq flycheck-checker 'sh-shellcheck)))
  )

;; Spell checker
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default delay is 0.25

;; spell-fu needs to be turn on manually
(remove-hook! (text-mode) #'spell-fu-mode)

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

;; Email config stuff. It's using msmtp to send emails
(setq mail-user-agent 'message-user-agent
      sendmail-program "/usr/local/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; We use Notmuch to manage emails in Emacs
(after! notmuch
  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))
  ;; Command to fetch for new emails
  (setq +notmuch-sync-backend 'custom)  ; so we can run our custom command
  (setq +notmuch-sync-command "mbsync -a && notmuch new")
  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        '(("mpetiteau.pro@gmail.com" . "personal/sent -inbox +sent -unread")
          ("matthieu@smallwatersolutions.com" . "sws/sent -inbox +sent -unread"))))

(defvar my-notes-directory "~/org"
  "Where I'm storing all my notes stuff.")

;; Deft (notes)
(after! deft
  (setq deft-directory my-notes-directory
        deft-recursive t  ; Search recursively from the deft-directory
        ))

;; Org settings
(after! org
  (setq org-directory my-notes-directory
        org-hide-emphasis-markers t  ; Hide mark symbols such as *bold* or ~code~
        ))

;; Org-journal
(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" my-notes-directory)
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "journal-%Y%m%d.org"
        ))

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
