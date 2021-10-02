;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; Frame

(setq default-frame-alist
      (append default-frame-alist
              '((width . 105)
                (height . 50)
                (drag-internal-border . 1)
                (internal-border-width . 0)
                (inhibit-double-buffering . t))))

(setq frame-title-format '("Emacs " emacs-version))
(setq ns-use-proxy-icon nil)  ; hide file icon from titlebar


;;
;;; General

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com"
      user-mail-address-2 "matthieu@smallwatersolutions.com")

(defvar my-dotfiles-dir "~/dotfiles"
  "Dotfiles directory.")

(setq confirm-kill-emacs nil
      load-prefer-newer t               ; always load newer bytes compiled files
      evil-vsplit-window-right t
      evil-split-window-below t
      default-directory "~/"
      undo-limit 80000000
      evil-want-fine-undo t             ; fine grained undo history
      inhibit-compacting-font-caches t  ; improve general perfs
      scroll-margin 7                   ; top and bottom margins to trigger scroll
      which-key-idle-delay 0.2)         ; delay to show key bindings menu

;; Abbreviations
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-file-name (expand-file-name "abbrev_defs" doom-private-dir))

;; Custom File, used by Emacs to cache some data relative to the config
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))


;;
;;; Fonts

(setq ns-use-thin-smoothing t)

(defvar my-monospace-font "Monaco"
  "Monospace font")

(defvar my-sans-serif-font "Lucida Grande"
  "Sans serif font")

(setq doom-font (font-spec :family my-monospace-font :size 13)
      doom-variable-pitch-font (font-spec :family my-sans-serif-font :size 14))

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default line-spacing 1)
(setq-default tab-width 8)


;;
;;; Themes

(load! "+custom-faces")


;;
;;; Bindings

(load! "+bindings")


;;
;;; Editor

(setq-default with-editor-emacsclient-executable "emacsclient")
(setq display-line-numbers-type nil)  ; no line number

;; Vertical file explorer
;; doc: https://github.com/Alexander-Miller/treemacs

(after! treemacs
  (setq doom-themes-treemacs-enable-variable-pitch t
        doom-themes-treemacs-line-spacing 0
        doom-themes-treemacs-theme "doom-colors"
        treemacs-width 35)
  (treemacs-resize-icons 14))

;; Git fringe indicator
;; doc: https://github.com/emacsorphanage/git-gutter-fringe

(after! git-gutter-fringe
  (fringe-mode 2))

;; Disable hl-line
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; When hl-line is available, do not override the color of rainbow-mode
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(defvar global-window-divider-width 2
  "Default global width size of a window divider.")

;; Set window dividers width
(setq window-divider-default-right-width global-window-divider-width
      window-divider-default-bottom-width global-window-divider-width)

;; Activate goto-address mode on some major modes
(add-hook! (prog-mode text-mode restclient-mode vterm-mode eshell-mode)
  (goto-address-mode t))

;; Zen mode
;; doc: https://github.com/joostkremers/writeroom-mode

(after! writeroom-mode
  (setq +zen-window-divider-size global-window-divider-width
        +zen-text-scale 0))

;; Evil visual hints
;; doc: https://github.com/edkolev/evil-goggles

(after! evil-goggles
  (setq evil-goggles-duration 0.25)
  (evil-goggles-use-magit-faces))

;;
;;; Custom templates

;; Custom file templates
(setq +file-templates-dir "~/.doom.d/templates"
      +file-templates-default-trigger "_template")

;; Use file templates only for a few specific modes
(setq +file-templates-alist
      '(("/packages\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "_template-doom-packages"
         :mode emacs-lisp-mode)
        ("\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "_template-doom-module"
         :mode emacs-lisp-mode)
        (restclient-mode)
        (sh-mode)))

;;
;;; Dashboard

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

(setq +doom-dashboard-menu-sections
      '(("Open project"
         :action projectile-switch-project
         :face default)
        ("Recently opened files"
         :action recentf-open-files
         :face default)
        ("Reload last session"
         :when (cond ((require 'persp-mode nil t)
                      (file-exists-p
                       (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :action doom/quickload-session
         :face default)
        ("Find file in dotfiles"
         :when (file-directory-p my-dotfiles-dir)
         :action my/find-file-in-dotfiles
         :face default)
        ("Find file in Doom config"
         :when (file-directory-p doom-private-dir)
         :action doom/find-file-in-private-config
         :face default)
        ("Open documentation"
         :action doom/help
         :face default)))


;;
;;; Project space management

;; doc: https://github.com/bbatsov/projectile
;;      https://docs.projectile.mx/projectile/index.html
;;
;; Run `projectile-discover-projects-in-search-path' to autoload all the projects from the
;; `projectile-project-search-path' list.

(after! projectile
  (setq projectile-sort-order 'recentf
        projectile-mode-line-prefix "P"
        projectile-mode-line-function '(lambda () (format " P[%s]" (projectile-project-name)))
        projectile-ignored-projects '("~/" "/tmp" "~/Downloads" "~/backups")
        projectile-project-search-path '("~/dotfiles/" "~/Projects/" "~/Code/" "~/Github/")))


;;
;;; File explorer

;; doc: https://www.emacswiki.org/emacs/DiredMode
;;      https://github.com/Fuco1/dired-hacks

(add-hook! 'dired-mode-hook 'dired-hide-details-mode)

(after! dired
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat"))  ; sort by date

;; Narrowing searchs in dired
(use-package! dired-narrow
  :after dired
  :commands (dired-narrow-fuzzy))

;; Toggle directories with TAB in dired
(use-package! dired-subtree
  :after dired
  :commands (dired-subtree-toggle dired-subtree-cycle))


;;
;;; Completion frameworks

;; Code completion
;; doc: https://www.emacswiki.org/emacs/CompanyMode

(defface my-company-icons-face
  '((t :background unspecified :inherit default))
  "The face used to display the company icons.")

(after! company
  (setq company-idle-delay 0.1
        company-tooltip-limit 10
        company-minimum-prefix-length 1)

  ;; Company icons
  (setq company-format-margin-function #'company-text-icons-margin
        company-text-icons-format " %s "
        company-text-icons-add-background nil)

  (setq company-text-icons-mapping
        '((array "[" my-company-icons-face)
          (boolean "1" my-company-icons-face)
          (class "C" my-company-icons-face)
          (color "#" my-company-icons-face)
          (constant "c" my-company-icons-face)
          (enum-member "e" my-company-icons-face)
          (enum "e" my-company-icons-face)
          (field "f" my-company-icons-face)
          (file "F" my-company-icons-face)
          (folder "D" my-company-icons-face)
          (interface "i" my-company-icons-face)
          (keyword "k" my-company-icons-face)
          (method "m" my-company-icons-face)
          (function "f" my-company-icons-face)
          (module "{" my-company-icons-face)
          (numeric "n" my-company-icons-face)
          (operator "o" my-company-icons-face)
          (parameter "p" my-company-icons-face)
          (property "p" my-company-icons-face)
          (ruler "r" my-company-icons-face)
          (snippet "S" my-company-icons-face)
          (string "s" my-company-icons-face)
          (struct "%" my-company-icons-face)
          (text "w" my-company-icons-face)
          (value "v" my-company-icons-face)
          (variable "v" my-company-icons-face)
          (t "." my-company-icons-face))))


;;
;;; Programmation Language server protocol

;; doc: https://emacs-lsp.github.io/lsp-mode/

(setq +lsp-prompt-to-install-server 'quiet
      +format-with-lsp nil)

(after! lsp-mode
  (setq lsp-enable-file-watchers nil))

;; Git porcelain
;; doc: https://github.com/magit/magit

(after! magit
  (setq git-commit-summary-max-length 70))

;; Interactive code analysis and linting
;; doc: https://www.flycheck.org/en/latest/

(after! flycheck
  ;; Pylint (python)
  (setq flycheck-python-pylint-executable "/usr/local/bin/pylint"
        flycheck-pylintrc "~/.config/pylintrc")
  (setq-hook! 'python-mode-hook flycheck-checker 'python-pylint)

  ;; Shellcheck (bash)
  (setq flycheck-shellcheck-excluded-warnings '("SC1091"))
  (setq-hook! 'sh-mode-hook flycheck-checker 'sh-shellcheck))

;; Check for spelling mistakes
;; doc: https://gitlab.com/ideasman42/emacs-spell-fu

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

;; Force grammar spell checking to be turn on manually
(remove-hook! (text-mode) #'spell-fu-mode)

(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2" ; nb of spaces used for indentation
      "-ci"    ; indent switch cases
      "-bn")   ; binary ops may start a line
    :modes '(sh-mode)))

(add-hook! 'shell-mode-hook (company-mode -1))

(setq-hook! 'sh-mode-hook
  sh-basic-offset 2
  indent-tabs-mode nil)

(after! python
  (setq python-shell-interpreter "python3")
  (set-formatter! 'black
    '("black"
      "--quiet"
      "--line-length" "100"
      "-")  ; apply in file changes
    :modes '(python-mode)))

(set-popup-rule! "^\\*pytest*" :size 0.3)

;; Disable warnings in python repl
(add-hook! 'inferior-python-mode-hook 'python-shell-completion-native-turn-off)

(after! js2-mode
  (set-formatter! 'prettier
    '("prettier"
      "--print-width" "120"
      ("--stdin-filepath" "%s" buffer-file-name))
    :modes '(js2-mode)))

(setq-hook! 'js2-mode js2-basic-offset 2)
(setq-hook! 'json-mode js-indent-level 2)
(setq-hook! 'go-mode indent-tabs-mode t)
(setq-hook! 'web-mode-hook
  tab-width 2
  web-mode-markup-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-script-padding 2
  web-mode-style-padding 2)

(setq-hook! 'html-mode-hook +format-with :none)
(setq-hook! 'web-mode-hook +format-with :none)

;; Detect specific modes

(setq auto-mode-alist
      (append '(("\\.restclient" . restclient-mode)
                ("abbrev_defs" . emacs-lisp-mode))
              auto-mode-alist))

(setq interpreter-mode-alist
      (append '(("osascript" . applescript-mode))
              interpreter-mode-alist))


;;
;;; Vterm

;; doc: https://github.com/akermu/emacs-libvterm

(after! vterm
  (setq vterm-max-scrollback 6000)
  (remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)  ; always display modeline

  (defun my/vterm-delete-word ()
    (interactive)
    (vterm-send-key (kbd "C-w"))))


;;
;;; Eshell

;; doc: https://www.gnu.org/software/emacs/manual/html_node/eshell/index.html

;; Remove the virtual env variable once the env has been deactivated, it will
;; get recreated once we reactivate the env. It's used in the eshell prompt
;; so we need to remove it when not in use.
(add-hook! 'pyvenv-post-deactivate-hooks (lambda () (setenv "VIRTUAL_ENV" nil)))

;; Disable company in eshell
;; (setq-hook! 'eshell-mode-hook company-idle-delay nil)
(add-hook! 'eshell-mode-hook (company-mode -1))

(after! eshell
  (defun my/eshell-current-git-branch ()
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

  (defun my/eshell-prompt ()
    (let ((base/dir (shrink-path-prompt default-directory))
          (base/branch (my/eshell-current-git-branch)))
      (concat
       ;; python venv
       (if (getenv "VIRTUAL_ENV")
           (let ((venv (file-name-nondirectory (getenv "VIRTUAL_ENV"))))
             (propertize (format "(%s) " venv) 'face 'default)))
       ;; directory path
       (propertize (car base/dir) 'face 'font-lock-comment-face)
       (propertize (cdr base/dir) 'face 'default)
       ;; git branch
       (if base/branch
           (propertize (format " (%s)" base/branch) 'face 'default))
       ;; user / super user
       (propertize (if (= (user-uid) 0) " # " " λ ") 'face 'default))))

  (setq eshell-history-size 1000000
        eshell-buffer-maximum-lines 5000
        eshell-modify-global-environment t
        eshell-destroy-buffer-when-process-dies t)

  (remove-hook! 'eshell-mode-hook #'hide-mode-line-mode)  ; always display modeline

  ;; Prompt settings
  (setq eshell-prompt-regexp "^.* [#λ] "
        eshell-prompt-function #'my/eshell-prompt)

  ;; List of eshell aliases
  (set-eshell-alias!
   "d" "dired $1"
   "clear" "clear-scrollback"
   "c" "clear-scrollback"
   "g" "git $*"
   "qq" "exit"
   "gs" "magit-status"
   "gc" "magit-commit"
   "gd" "magit-diff-unstaged"
   "gds" "magit-diff-staged"
   "..." "cd ../.."
   "...." "cd ../../.."
   "....." "cd ../../../.."
   "k" "kubectl $*"
   "kt" "kubetail $*"
   "kgn" "kubectl get namespaces"
   "ls" "my/eshell/ls $*")

  ;; Custom Eshell functions

  (defun eshell/cr ()
    "Go to git repository root."
    (eshell/cd (locate-dominating-file default-directory ".git")))

  (defun eshell/md (dir)
    "mkdir and cd into DIR."
    (eshell/mkdir dir)
    (eshell/cd dir))

  (defun eshell/dots ()
    "cd into my dotfiles directory."
    (eshell/cd "~/dotfiles"))

  (defun my/eshell/ls (&rest args)
    "ls command list hidden files by default."
    (eshell/ls "-a" args))

  (defun eshell/sl (&rest args)
    "Same as ls, used to avoid typos."
    (my/eshell/ls args))

  (defun eshell/o ()
    "Open in finder."
    (+macos/reveal-in-finder))

  (defun eshell/deactivate ()
    "Deactivate a python venv."
    (pyvenv-deactivate))

  (defun eshell/activate (&optional env)
    "Activate a python ENV."
    (if env
        (pyvenv-activate env)
      (pyvenv-activate "env")))

  (defun eshell/e (&rest args)
    "Invoke `find-file' on the file.
\"e +42 foo\" also goes to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (find-file file)
            (forward-line line))
        (find-file (pop args)))))

  (defun eshell/extract (file)
    "Extract archive file."
    (let ((command
           (some (lambda (x)
                   (if (string-match-p (car x) file)
                       (cadr x)))
                 '((".*\.tar.bz2" "tar xjf")
                   (".*\.tar.gz" "tar xzf")
                   (".*\.bz2" "bunzip2")
                   (".*\.rar" "unrar x")
                   (".*\.gz" "gunzip")
                   (".*\.tar" "tar xf")
                   (".*\.tbz2" "tar xjf")
                   (".*\.tgz" "tar xzf")
                   (".*\.zip" "unzip")
                   (".*\.Z" "uncompress")
                   (".*" "echo 'Could not extract the file:'")))))
      (eshell-command-result (concat command " " file)))))


;;
;;; Org

;; doc: https://orgmode.org/manual/

(defvar my-notes-directory "~/org"
  "Where I'm storing my notes.")

(after! org
  (setq org-directory my-notes-directory
        org-hide-emphasis-markers t))

;; (add-hook! 'org-mode-hook 'variable-pitch-mode)

;; Deft
;; doc: https://github.com/jrblevin/deft

(after! deft
  (setq deft-directory my-notes-directory))

;; Make invisible parts of Org elements appear visible
;; doc: https://github.com/awth13/org-appear

(use-package! org-appear
  :after org-mode
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(add-hook! 'org-mode-hook 'org-appear-mode)

;; Journal
;; doc: https://github.com/bastibe/org-journal

(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" my-notes-directory)
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "journal-%Y%m%d.org"))


;;
;;; Mail

;; Emails are sent using msmtp
(setq sendmail-program "/usr/local/bin/msmtp")

(setq mail-user-agent 'message-user-agent
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Email client
;; doc: https://notmuchmail.org/emacstips/

(after! notmuch
  ;; Main buffer sections
  (setq notmuch-show-log nil
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags))

  ;; Remove pop-up rule, so it opens in its own buffer
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)

  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))

  ;; Use a custom command to fetch for new emails with mbsync
  (setq +notmuch-sync-backend "mbsync -a && notmuch new")

  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        '((user-mail-address . "personal/sent -inbox +sent -unread")
          (user-mail-address-2 . "sws/sent -inbox +sent -unread"))))


;;
;;; Misc

;; shrink-path util
(use-package! shrink-path
  :commands (shrink-path-file shrink-path-prompt))

;; Markdown visualiser
;; doc: https://github.com/seagle0128/grip-mode

(after! grip-mode
  (setq grip-github-user "smallwat3r"
        grip-github-password (+pass-get-secret "github/password")))

;; Scratch buffers
;; doc: https://github.com/ieure/scratch-el

(use-package! scratch
  :commands (scratch))

;; Auto add headers on scratch buffers in specific modes
(add-hook! 'org-mode-hook (my/add-scratch-buffer-header "#+TITLE: Scratch file"))
(add-hook! 'sh-mode-hook (my/add-scratch-buffer-header "#!/usr/bin/env bash"))
(add-hook! 'restclient-mode-hook (my/add-scratch-buffer-header "#\n# restclient\n#"))

;; Insert lorem-ipsum text
;; doc: https://github.com/jschaf/emacs-lorem-ipsum

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list))

;; Untappd
;; doc: https://github.com/smallwat3r/untappd.el

(use-package! untappd
  :commands (untappd-feed)
  :config (setq untappd-access-token
                (auth-source-pass-get 'secret "untappd/token")))
