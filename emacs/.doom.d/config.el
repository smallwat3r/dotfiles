;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; Frame

(setq initial-frame-alist
      (append initial-frame-alist
              '((fullscreen . maximized))))

(setq default-frame-alist
      (append default-frame-alist
              '((width . 105)
                (height . 40)
                (left-fringe . 0)
                (right-fringe . 0)
                (drag-internal-border . t)
                (internal-border-width . 3)
                (inhibit-double-buffering . t))))

(setq frame-title-format '("Emacs:" emacs-version))

;; Check if Emacs is displaying the frame in a Nextstep inferface. This is used
;; by macOS (and GNUstep). So the below settings would only be applied on these
;; systems.
(when (eq window-system 'ns)
  (setq ns-use-thin-smoothing t
        ns-use-native-fullscreen nil
        ns-use-proxy-icon nil
        ns-use-fullscreen-animation nil))

;; Emacs everywhere.
;; Ability to pop-up an Emacs buffer anywhere to type some text and send it back
;; to the GUI using C-c.
;; doc: https://github.com/tecosaur/emacs-everywhere
(after! emacs-everywhere
  (setq emacs-everywhere-frame-parameters
        '((name . "emacs-everywhere")
          (width . 110)
          (height . 30))))


;;
;;; General

(setq user-full-name "Matthieu Petiteau"
      user-mail-address "mpetiteau.pro@gmail.com"
      user-mail-address-2 "matthieu@smallwatersolutions.com")

(setq default-directory "~/"
      my-dotfiles-dir (concat default-directory "dotfiles"))

;; Abbreviations
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-file-name (expand-file-name "abbrev_defs" doom-private-dir))

;; Custom File, used by Emacs to cache some data related to its config
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))


;;
;;; Fonts

(let* ((font "Monaco")
       (font-size 13))
  (setq doom-font (font-spec :family font :size font-size)
        doom-serif-font (font-spec :family font :size font-size)
        doom-variable-pitch-font (font-spec :family font :size font-size)))

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default line-spacing 1)
(setq-default tab-width 8)


;;
;;; Themes

(setq doom-theme 'simplicity)

;; Enforce these faces for all the themes. This must comes after the theme gets
;; defined in the config.
(custom-set-faces!
  '(git-commit-summary :foreground "orange1")

  '(magit-section-highlight :background unspecified)
  '(magit-diff-hunk-heading-highlight :background "blue2" :foreground "coral1")
  '(magit-diff-hunk-heading :background "blue2" :foreground "cyan1")

  '((markdown-code-face
     markdown-pre-face
     org-block
     org-inline-src-block)
    :background "gray16" :foreground "gray93" :extend t)

  '((markdown-inline-code-face
     org-code)
    :inherit help-key-binding :foreground "gray93")

  ;; I like to keep my editor clean and simple. Do not activate code
  ;; highlighting on some major code faces like variables or functions, as I
  ;; don't think having lots of colors helps with readability.
  '((font-lock-function-name-face
     font-lock-variable-name-face
     font-lock-constant-face
     font-lock-builtin-face
     font-lock-type-face)
    :foreground unspecified :weight normal))


;;
;;; Bindings

(load! "+bindings")


;;
;;; Editor

(blink-cursor-mode 1)

(setq-default with-editor-emacsclient-executable "emacsclient")

(setq display-line-numbers-type nil ; no line numbers
      scroll-margin 7)              ; top and bottom margins to trigger scroll

(setq confirm-kill-emacs nil            ; quit emacs without confirmation
      load-prefer-newer t               ; always load newer bytes compiled files
      inhibit-compacting-font-caches t) ; improve general perfs

(setq evil-vsplit-window-right t
      evil-split-window-below t
      evil-want-fine-undo t)

;; Prompt to select file after an evil window split action. Press ESC to cancel
;; and the split windown will be for the current file.
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (ido-find-file))

;; Scrolling
(if (boundp 'mac-mouse-wheel-smooth-scroll)
    (setq mac-mouse-wheel-smooth-scroll t))
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode))

;; Magit
;; doc: https://github.com/magit/magit
(after! magit
  ;; These bindings are hard to work with as I'm running evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil))

;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2
        which-key-allow-imprecise-window-fit nil))

;; Git fringe indicator
;; doc: https://github.com/emacsorphanage/git-gutter-fringe
(after! git-gutter-fringe
  ;; `fringe-mode' is not a git-gutter-fringe specific parameter. But it
  ;; specifies the pixel width of the fringe used for git-gutter-fringe.
  (fringe-mode 2))

;; Disable globally hightlighting the current line the cursor is on.
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; When hl-line is available, do not override the color of rainbow-mode.
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(defvar my-global-window-divider-width 2
  "Default global width size (in pixels) of a window divider.")

;; Set window dividers widths.
(setq window-divider-default-right-width my-global-window-divider-width
      window-divider-default-bottom-width my-global-window-divider-width)

;; Activate goto-address mode on some major modes.
;; This mode activates and highlights URLs and email addresses in the current buffer.
(add-hook! (prog-mode text-mode restclient-mode vterm-mode eshell-mode)
  (goto-address-mode t))

;; Zen mode. Implements a distraction free writing mode.
;; doc: https://github.com/joostkremers/writeroom-mode
(after! writeroom-mode
  (setq +zen-window-divider-size global-window-divider-width
        +zen-text-scale 0))

;; Overlay keywords
;; doc: https://github.com/wolray/symbol-overlay
(use-package! symbol-overlay
  :config
  ;; Do not conflict with vim bindings when using overlays.
  ;; Change `symbol-overlay-map-help' to be triggered from 'H' instead of 'h'.
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "H") #'symbol-overlay-map-help))

;; Evil visual hints when yanking, pasting, deleting etc.
;; doc: https://github.com/edkolev/evil-goggles
(after! evil-goggles
  (setq evil-goggles-duration 0.25)
  (evil-goggles-use-magit-faces))

;; Evil escape
;; doc: https://github.com/syl20bnr/evil-escape
(after! evil-escape
  ;; Do not activate evil-escape through the 'jk' escape sequence in the
  ;; following modes. It might be because 'j' and 'k' allow scrolling up or
  ;; down even in insert-mode, or it is just breaking the expected mode
  ;; behaviour.
  (setq evil-escape-excluded-major-modes '(treemacs-mode)))

;; Highlight todos
;; doc: https://github.com/tarsius/hl-todo
(after! hl-todo
  (defface my-todos-face
    '((t :background unspecified :foreground "#f54260" :weight bold))
    "The face used to display todos from hl-todo.")

  (setq hl-todo-keyword-faces
        `(("TODO" . my-todos-face)
          ("DEPRECATED" . my-todos-face)
          ("QUESTION" . my-todos-face)
          ("FIXME" . my-todos-face)
          ("HACK" . my-todos-face)
          ("BUG" . my-todos-face)
          ("NOTE" . my-todos-face)
          ("SECURITY" . my-todos-face))))


;;
;;; Custom templates

(setq +file-templates-dir "~/.doom.d/templates"
      +file-templates-default-trigger "_template")

;; Allow some file templates to be used only in specific modes.
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

;; This is the dashboard displayed when starting Emacs.
;; It displays a menu with useful links to go to. As a personal preference, I
;; like to keep it short and simple.

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

;; Projectile
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
        projectile-project-search-path '("~/dotfiles/" "~/projects/" "~/code/" "~/github/")))


;;
;;; File explorers

;; Dired
;; doc: https://www.emacswiki.org/emacs/DiredMode
;;      https://github.com/Fuco1/dired-hacks
(after! dired
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat")  ; sort by date
  (add-hook! 'dired-mode-hook 'dired-hide-details-mode))

;; Narrowing searchs in dired
(use-package! dired-narrow
  :after dired
  :commands (dired-narrow-fuzzy))

;; Toggle directories with TAB in dired
(use-package! dired-subtree
  :after dired
  :commands (dired-subtree-toggle dired-subtree-cycle))

;; Treemacs
;; doc: https://github.com/Alexander-Miller/treemacs
(after! treemacs
  (setq doom-themes-treemacs-enable-variable-pitch t
        doom-themes-treemacs-line-spacing 0
        doom-themes-treemacs-theme "doom-colors"
        treemacs-width 35)
  (treemacs-resize-icons 14))


;;
;;; Completion frameworks

;; This function gets used for `company-format-margin-function'. The default
;; displayed the company icons without a space between the icons and the text.
;; Which makes it a bit innelegant. This function adds a space between the two
;; values.
(defun my/company-spaced-dark-icons-margin (candidate selected)
  (concat
   (company--render-icons-margin company-vscode-icons-mapping
                                 (expand-file-name "vscode-dark" company-icons-root)
                                 candidate
                                 selected)
   " "))

;; Code completion
;; doc: https://www.emacswiki.org/emacs/CompanyMode
(after! company
  (setq company-idle-delay 0.1
        company-tooltip-limit 10
        company-minimum-prefix-length 1
        company-format-margin-function 'my/company-spaced-dark-icons-margin))


;;
;;; Programming

;; Language Server Protocol
;; doc: https://emacs-lsp.github.io/lsp-mode/
(setq +lsp-prompt-to-install-server 'quiet
      +format-with-lsp nil)

(after! lsp-mode
  ;; I had issues with file watchers enabled in the past as Emacs would freeze
  ;; because it took too much memory. So I just disable it as a default.
  (setq lsp-enable-file-watchers nil))

;; Magit
;; doc: https://github.com/magit/magit
(after! magit
  (setq git-commit-summary-max-length 90))

;; Interactive code analysis and linting
;; doc: https://www.flycheck.org/en/latest/
(after! flycheck
  ;; Pylint (python)
  (setq flycheck-python-pylint-executable "/usr/local/bin/pylint"
        flycheck-pylintrc "~/.config/pylintrc"
        flycheck-python-mypy-config "~/.config/mypy/config")
  (setq-hook! 'python-mode-hook flycheck-checker 'python-pylint)

  ;; Shellcheck (bash)
  (setq flycheck-shellcheck-excluded-warnings '("SC1091"))
  (setq-hook! 'sh-mode-hook flycheck-checker 'sh-shellcheck))

;; Check for spelling mistakes
;; doc: https://gitlab.com/ideasman42/emacs-spell-fu
(after! spell-fu
  (setq spell-fu-idle-delay 0.5)
  ;; spell-fu is by default enabled in text-mode, but I think this quite
  ;; annoying, so force it to be disabled, and we can explicitly enable it
  ;; if we need to use it.
  (remove-hook! (text-mode) #'spell-fu-mode))

(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2" ; nb of spaces used for indentation
      "-ci"    ; indent switch cases
      "-bn")   ; binary ops may start a line
    :modes '(sh-mode)))

;; Disabled company auto completion on shell mode. I experienced some heavy
;; performance issues when it was enabled.
(add-hook! 'shell-mode-hook (company-mode -1))

(after! python
  (setq python-shell-interpreter "python3")
  (set-formatter! 'black
    '("black"
      "--quiet"
      "--line-length" "100"
      "-")  ; apply in file changes
    :modes '(python-mode)))

(set-popup-rule! "^\\*pytest*" :size 0.3)

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

(setq-hook! 'sh-mode-hook
  sh-basic-offset 2
  indent-tabs-mode nil)

;; Disable formatters for html and web modes
(setq-hook! 'html-mode-hook +format-with :none)
(setq-hook! 'web-mode-hook +format-with :none)

(after! sql
  ;; MySQL settings does not provide a default port. Use 3306 as the default as
  ;; this is the most widely used.
  (setq sql-mysql-login-params
        (append sql-mysql-login-params '(port :default 3306))))

;; Activate specific modes from the current file name or extension.
(setq auto-mode-alist
      (append '(("\\.restclient" . restclient-mode)
                ("abbrev_defs" . emacs-lisp-mode)
                ("Makefile.*" . makefile-mode))
              auto-mode-alist))

;; Activate specific modes from the current file shebang.
(setq interpreter-mode-alist
      (append '(("osascript" . applescript-mode))
              interpreter-mode-alist))

;; Debug Adapter Protocol
;; Enables communication between client and a debug server, for powerful
;; interactive debugging.
;; TODO: I need to spend a bit more time setting this up and getting used to it.
;; doc: https://github.com/emacs-lsp/dap-mode
(after! dap-mode
  (setq dap-python-debugger 'debugpy
        dap-python-executable "python3"))


;;
;;; Terminals

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.01
        vterm-always-compile-module t)

  ;; Make sure to always display the modeline when using vterm. I feel like even
  ;; in a terminal, its still useful to see the modeline and its information.
  (remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

  (defun my/vterm-delete-word ()
    "Binding function to delete a word."
    (interactive)
    (vterm-send-key (kbd "C-w")))

  (defun my/set-no-process-query-on-exit ()
    "Disable dialog to confirm killing buffer with running process."
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))

  ;; Annoyingly every time I try to kill a vterm buffer it asks me for confirmation
  ;; as it has a running process. This hook allows me to bypass this and kill it
  ;; regardless.
  ;; NOTE: Doom is supposed to have a hook for this, setting `confirm-kill-processes'
  ;; to nil, but it doesn't seems to work properly for some reason.
  (add-hook! 'vterm-mode-hook #'my/set-no-process-query-on-exit))

;; eshell
;; doc: https://www.gnu.org/software/emacs/manual/html_node/eshell/index.html
(after! eshell
  (defun my/eshell-current-git-branch ()
    "Get current git branch."
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

  (defun my/eshell-prompt ()
    "Build eshell custom prompt."
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
       (propertize " % " 'face 'default))))

  (setq eshell-history-size 1000000
        eshell-buffer-maximum-lines 5000
        eshell-modify-global-environment t
        eshell-destroy-buffer-when-process-dies t)

  ;; Make sure to always display the modeline when using eshell. I feel like even
  ;; in a terminal, its still useful to see the modeline and its information.
  (remove-hook! 'eshell-mode-hook #'hide-mode-line-mode)

  ;; Prompt settings
  (setq eshell-prompt-regexp "^.* [%] "
        eshell-prompt-function #'my/eshell-prompt)

  ;; Remove the virtual env variable once the env has been deactivated, it will
  ;; get recreated once we reactivate the env. It's used in the eshell prompt
  ;; so we need to remove it when not in use.
  (add-hook! 'pyvenv-post-deactivate-hooks (lambda () (setenv "VIRTUAL_ENV" nil)))

  ;; Disable company completion in eshell.
  (add-hook! 'eshell-mode-hook (company-mode -1))

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
  ;; These can be used directly in eshell by omitting the 'eshell/' prefixes.
  ;; For example 'eshell/cr' can be used directly by invoking 'cr' in eshell.

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
      (pyvenv-activate "env"))))


;;
;;; Org

(defvar my-notes-directory "~/org"
  "Where I'm storing my notes.")

;; Org mode
;; doc: https://orgmode.org/manual/
(after! org
  (setq org-directory my-notes-directory
        org-hide-emphasis-markers t))

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

(setq sendmail-program "/usr/local/bin/msmtp"
      mail-user-agent 'message-user-agent
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Email client
;; doc: https://notmuchmail.org/emacstips/
(after! notmuch
  ;; Main buffer sections information.
  (setq notmuch-show-log nil
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags))

  ;; By default, Doom's implementation of Notmuch makes it open in a pop-up
  ;; buffer. But I prefer Notmuch to open in its own buffer window.
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

;; shrink-path provides functions to shrink filepaths to be displayed such as
;; '/foo/bar/file.el' becomes 'f/b/file.el'.
(use-package! shrink-path
  :commands (shrink-path-file shrink-path-prompt))

;; Markdown visualiser
;; doc: https://github.com/seagle0128/grip-mode
(after! grip-mode
  ;; TODO: even by providing credentials, the API rate limitation is really
  ;;       annoying as super limited. I feel like there must be a better
  ;;       solution to this.
  (setq grip-github-user "smallwat3r"
        grip-github-password (+pass-get-secret "github/password")))

;; Scratch buffers
;; doc: https://github.com/ieure/scratch-el
(use-package! scratch
  :commands (scratch))

;; Automatically add headers on scratch buffers in specific modes.
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

;; Look up
(setq +lookup-provider-url-alist
      ;; Searching stuff on sourcegraph is quite useful. It provides lots of
      ;; code implementations and examples.
      (append +lookup-provider-url-alist
              '(("Sourcegraph"
                 "https://sourcegraph.com/search?q=context:global+%s&patternType=literal"))))


;;
;;; Testing...

;; I use the below commands when testing/debugging my Emacs config. I'm sure
;; there are better ways or tools to do most of this, but hey, it works for me!

(defun my-echo-command-name-hook ()
  "Echo live command names."
  (unless (or (eq this-command 'self-insert-command)
              (eq this-command 'next-line))
    (message "%s" this-command)))

(define-minor-mode my-debug-mode
  "Custom debug mode.")

(add-hook! 'post-command-hook
  (if (bound-and-true-p my-debug-mode)
      (my-echo-command-name-hook)))
