;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; Frame

(push '(width . 105) default-frame-alist)
(push '(height . 40) default-frame-alist)
(push '(left-fringe . 2) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(push '(drag-internal-border . t) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(inhibit-double-buffering . t) default-frame-alist)

(when IS-LINUX
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(defvar my-title-emacs-version (concat "Emacs @ " emacs-version)
  "Running Emacs version as a title.")

(setq frame-title-format (concat my-title-emacs-version " - %b"))

(when IS-MAC
  ;; Nextstep inferface settings. This is used by macOS (and GNUstep).
  (setq ns-use-thin-smoothing nil
        ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        ns-antialias-text t))


;;
;;; General

(defvar my-user-alias "smallwat3r"
  "User alias.")

(defvar my-email-addresses-alist
  '(("gmail". "mpetiteau.pro@gmail.com")
    ("sws" . "matthieu@smallwatersolutions.com")
    ("smallwat3r" . "matt@smallwat3r.com"))
  "Alist of my email addresses.")

(setq user-full-name "Matthieu Petiteau"
      user-mail-address (cdr (assoc "gmail" my-email-addresses-alist)))

(setq default-directory "~/")

(defvar my-dotfiles-dir (concat default-directory "dotfiles")
  "Directory containing my dotfiles.")

;; Abbreviations
(use-package! abbrev
  :custom
  (save-abbrevs nil)
  (abbrev-file-name (expand-file-name "abbrev_defs.el" doom-user-dir))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(setq-default abbrev-mode t)

;; Custom File, used by Emacs to cache some data related to its config.
(use-package! cus-edit
  :custom (custom-file (expand-file-name ".custom.el" doom-user-dir))
  :config
  (if (file-exists-p custom-file)
      (load custom-file t)))


;;
;;; Editor

(setq doom-theme 'smallwat3r)

;; Fonts
(if IS-LINUX
    (setq doom-font (font-spec :family "Triplicate A Code" :size 18))
  (setq doom-font (font-spec :family "Triplicate A Code" :size 16)))

(setq doom-variable-pitch-font (font-spec :family "Triplicate A"))

;; Enable proportional fonts for text-mode buffers.
(add-hook! 'text-mode-hook 'variable-pitch-mode)

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default line-spacing 2
              tab-width 8
              with-editor-emacsclient-executable "emacsclient")

(setq fancy-splash-image (concat doom-user-dir "/etc/emacs.svg"))

(defun my-dashboard-message ()
  (insert (+doom-dashboard--center
           +doom-dashboard--width
           (concat "MAIN BUFFER - " my-title-emacs-version))))

;; Dashboard displayed when starting Emacs. As a personal preference, I like to keep
;; it very simple. It is ligther than the default scratch buffer in many cases. But
;; it can also not be killed, hence remembers the working directory of the last open
;; buffer, `find-file' will work from the directory I expect.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner my-dashboard-message))

(setq display-line-numbers-type nil
      scroll-margin 7
      confirm-kill-emacs nil)

;; Doom modeline
;; doc: https://github.com/seagle0128/doom-modeline
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-vcs-max-length 20)

  ;; Prefer no icons on the modeline.
  (setq doom-modeline-icon nil)

  ;; HACK: Make sure the modeline renders thin (default is too wide).
  ;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-507201556
  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (+ (frame-char-height) 2))

  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height))

;; Evil-mode
(after! evil
  ;; Change cursor color and shape based on evil mode.
  (setq evil-emacs-state-cursor '("DarkTurquoise" box)
        evil-normal-state-cursor '("DarkTurquoise" box)
        evil-visual-state-cursor '("SlateGray2" box)
        evil-insert-state-cursor '("SlateGray2" box)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  ;; General evil mode settings.
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t))

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

;; Icons
;; doc: https://github.com/domtronn/all-the-icons.el
(after! all-the-icons
  ;; Keep icons small.
  (setq all-the-icons-scale-factor 0
        all-the-icons-default-adjust 0))

;; Scrolling
(if (boundp 'mac-mouse-wheel-smooth-scroll)
    (setq mac-mouse-wheel-smooth-scroll t))
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode))

;; Magit
;; doc: https://github.com/magit/magit
(after! magit
  (after! git-commit
    (setq git-commit-summary-max-length 74))
  ;; These bindings are hard to work with as I'm running evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil))

;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2)
  ;; Make evil commands less verbose, use small arrow symbols instead.
  ;; Stolen from: https://tecosaur.github.io/emacs-config/config.html#which-key,code--2
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

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

;; Goto-address mode. This mode activates and highlights URLs and email addresses
;; in the current buffer.
(use-package! goto-addr
  :defer t
  :custom
  (goto-address-mail-regexp "\\w+\\(\\.\\w+\\)?\\(\\+\\w+\\)?@\\(\\w\\|\\.\\)+\\.\\w+")
  (goto-address-mail-face 'my-goto-address-mail-face)
  :init
  (defface my-goto-address-mail-face '((t :italic nil :underline t))
    "Face for email address."
    :group 'basic-faces))

(add-hook! 'prog-mode-hook 'goto-address-prog-mode)
(add-hook! '(compilation-mode-hook text-mode-hook restclient-mode-hook vterm-mode-hook)
           'goto-address-mode)

;; todos
;; doc: https://github.com/tarsius/hl-todo
(after! hl-todo
  (add-to-list 'hl-todo-keyword-faces '("HACK" . "VioletRed1")))

;; Zen mode. Implements a distraction free writing mode.
;; doc: https://github.com/joostkremers/writeroom-mode
(after! writeroom-mode
  (setq +zen-window-divider-size my-global-window-divider-width
        +zen-text-scale 0))

;; Overlay keywords
;; doc: https://github.com/wolray/symbol-overlay
(use-package! symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-remove-all)
  :init
  (map! (:leader (:prefix "c"
                  :desc "Add overlay"     "h" #'symbol-overlay-put
                  :desc "Remove overlays" "H" #'symbol-overlay-remove-all)))
  :config
  ;; Deactivate binding for `symbol-overlay-map-help', as it conflicts with evil.
  ;; Remap it to a capital 'H' instead.
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "H") #'symbol-overlay-map-help))

;; Conveniently resize windows
;; doc: https://github.com/roman/golden-ratio.el
(use-package! golden-ratio
  :bind (:map evil-window-map
         ("g" . golden-ratio)
         ("G" . golden-ratio-mode))
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  ace-window))))

;; Flycheck pop-up tooltips
;; doc: https://github.com/flycheck/flycheck-popup-tip
(after! flycheck-popup-tip
  (setq flycheck-popup-tip-error-prefix "* "))

;; Imenu list
;; doc: https://github.com/bmag/imenu-list
(use-package! imenu-list
  :commands (imenu-list-smart-toggle))

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
;;; Project space management

;; Projectile
;; doc: https://github.com/bbatsov/projectile
;;      https://docs.projectile.mx/projectile/index.html
(after! projectile
  (setq projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-mode-line-function '(lambda () (format " P[%s]" (projectile-project-name))))

  (setq projectile-globally-ignored-directories
        '(".npm" ".poetry" "GoogleDrive" ".mypy_cache" "Library" ".git" "__pycache__"
          "node_modules" ".idea" ".vscode" ".svn" ".tox" ".cache"))

  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS" "*.pyc"))

  ;; Make the projectile command use fd with some more sensitive defaults, as I noticed some
  ;; performance issues with the one used by Doom or projectile natively.
  (let ((excludes (mapcar (lambda (val) (format "-E '%s'" val))
                          (append projectile-globally-ignored-files
                                  projectile-globally-ignored-directories))))
    (setq projectile-generic-command
          (format "%s" (cons "fd . -0 -H -c never -t file -t symlink --strip-cwd-prefix"
                             excludes))))

  ;; Ignore all projects from within these directories.
  (setq projectile-ignored-projects
        '("~/" "/tmp" "~/Downloads" "~/backups" "/Applications" "/Volumes/GoogleDrive"))

  ;; Allow projectile to automatically find projects from any of these directories.
  (setq projectile-project-search-path '("~/dotfiles/" "~/projects/" "~/code/" "~/github/")))


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
  :config (map! (:map dired-mode-map :n "/" #'dired-narrow-fuzzy)))

;; Toggle directories with TAB in dired
(use-package! dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom-face
  (dired-subtree-depth-1-face ((t (:background unspecified))))
  (dired-subtree-depth-2-face ((t (:background unspecified))))
  (dired-subtree-depth-3-face ((t (:background unspecified))))
  (dired-subtree-depth-4-face ((t (:background unspecified))))
  (dired-subtree-depth-5-face ((t (:background unspecified))))
  (dired-subtree-depth-6-face ((t (:background unspecified)))))

;; Treemacs
;; doc: https://github.com/Alexander-Miller/treemacs
(after! treemacs
  (setq doom-themes-treemacs-enable-variable-pitch t
        doom-themes-treemacs-line-spacing 0
        doom-themes-treemacs-theme "doom-colors"
        treemacs-width 50)
  (treemacs-resize-icons 14))


;;
;;; Completion frameworks

;; Code completion
;; doc: https://www.emacswiki.org/emacs/CompanyMode
(after! company
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
  (setq company-format-margin-function 'my/company-spaced-dark-icons-margin)

  ;; Make completion feel snappy.
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-tooltip-limit 6)

  (setq company-tooltip-align-annotations nil
        company-tooltip-offset-display 'lines))


;;
;;; Programming

;; Language Server Protocol
;; doc: https://emacs-lsp.github.io/lsp-mode/
(setq +lsp-prompt-to-install-server 'quiet
      +format-with-lsp nil)

(after! lsp-mode
  ;; I had issues with file watchers enabled in the past as Emacs would freeze
  ;; because it took too much memory. So I just disable it as a default.
  (setq lsp-enable-file-watchers nil)
  ;; Ignore asking to restart if server failed to boot.
  (setq lsp-restart 'ignore))

;; Shell scripts (bash, zsh...)
(after! sh-mode
  (setq-hook! 'sh-mode-hook
    flycheck-checker 'sh-shellcheck
    flycheck-shellcheck-excluded-warnings '("SC1091")
    sh-basic-offset 2
    indent-tabs-mode nil)

  ;; Formatter
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2" ; nb of spaces used for indentation
      "-ci"    ; indent switch cases
      "-bn")   ; binary ops may start a line
    :modes '(sh-mode)))

;; Disabled company auto completion on shell mode. I experienced some heavy
;; performance issues when it was enabled.
(add-hook! 'shell-mode-hook (company-mode -1))

;; Check for spelling mistakes
;; doc: https://gitlab.com/ideasman42/emacs-spell-fu
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

;; spell-fu is by default enabled in some modes, but I find this quite
;; annoying, so force it to be disabled, and we can explicitly enable it
;; if we need to use it.
(remove-hook! (text-mode yaml-mode) #'spell-fu-mode)

;; LSP python
(use-package! lsp-pyright
  :after lsp-mode
  :init
  ;; Enforce lsp-pyright to use one session per project. This needs to be set-up
  ;; before initialising lsp-pyright to work.
  (setq lsp-pyright-multi-root nil)
  :preface
  (after! python
    (setq lsp-pyright-python-executable-cmd python-shell-interpreter))
  :config
  (set-lsp-priority! 'pyright 1))

;; Python
(after! python
  (defvar my-default-python-line-length 100
    "Default python line length.")

  (setq python-shell-interpreter "python3")
  ;; Disable annoying warnings about `python-shell-interpreter' readline support.
  (setq python-shell-completion-native-enable nil)

  ;; Isort
  (after! py-isort
    (setq py-isort-options '("--trailing-comma" "--use-parentheses"))
    (add-to-list 'py-isort-options (format "-l %s" my-default-python-line-length)))

  ;; Formatter
  (set-formatter! 'black
    '("black"
      "--quiet"
      "--line-length" (format "%s" my-default-python-line-length)
      "--target-version" "py310"
      "-")  ; apply in file changes
    :modes '(python-mode))

  ;; Debugger
  (after! dap-mode
    (setq dap-python-debugger 'debugpy
          dap-python-executable python-shell-interpreter))

  (setq-hook! 'python-mode-hook
    flycheck-checker 'python-pylint
    flycheck-pylintrc "~/.config/pylintrc"
    flycheck-python-mypy-config "~/.config/mypy/config"
    flycheck-python-mypy-executable "mypy"
    flycheck-python-pyright-executable "pyright"))

;; Pytest
(set-popup-rule! "^\\*pytest*" :size 0.3)

;; Javascript
(after! js2-mode
  (setq-hook! 'js2-mode-hook js2-basic-offset 2)

  ;; Formatter
  (set-formatter! 'prettier
    '("prettier"
      "--print-width" "120"
      ("--stdin-filepath" "%s" buffer-file-name))
    :modes '(js2-mode)))

;; Json
(setq-hook! 'json-mode-hook tab-width 2)

;; Restclient
(setq-hook! 'restclient-mode-hook tab-width 4)

;; Golang
(setq-hook! 'go-mode-hook indent-tabs-mode t)

;; Web mode
(setq-hook! 'web-mode-hook
  tab-width 2
  web-mode-markup-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-script-padding 2
  web-mode-style-padding 2)

;; Disable formatters for html and web modes
(setq-hook! '(html-mode-hook web-mode-hook)
  +format-with :none)

;; SQL
(use-package! sql
  :mode (("\\.\\(m\\|my\\)?sql\\'" . sql-mode))
  :custom
  ;; I use this for local development only. Disable SSL mode to ease connectivity
  ;; using localhost.
  (sql-mysql-options '("--ssl-mode=DISABLED"))
  (sql-mysql-login-params '((user :default "root")
                            password
                            database
                            (server :default "127.0.0.1")
                            (port :default 3306))))

;; Makefile
(use-package! makefile-mode
  :mode ("Makefile.*" . makefile-mode))

;; Yaml mode
;; doc: https://github.com/yoshiki/yaml-mode
(use-package! yaml-mode
  :mode ("\\.\\(yaml\\|yml\\)\\'" . yaml-mode)
  :hook (yaml-mode . my/remap-yaml-faces)
  :config
  (defun my/remap-yaml-faces ()
    (face-remap-add-relative
     'font-lock-variable-name-face :inherit font-lock-keyword-face)
    ;; The above seems to leave `buffer-face-mode' on, disable it.
    (buffer-face-mode -1)))

;; Logs
;; doc: https://github.com/doublep/logview
(use-package! logview
  :mode ("\\.log.*" . logview-mode))

;; Applescript
;; doc: https://github.com/emacsorphanage/applescript-mode
(use-package! applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'" . applescript-mode))
  :interpreter ("osascript" . applescript-mode))

;; Nginx
;; doc: https://github.com/ajc/nginx-mode
(use-package! nginx-mode
  :mode (("/nginx/conf.d/.*" . nginx-mode)
         ("/nginx/.*\\.conf\\'" . nginx-mode)
         ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

;; TOML
(add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . conf-toml-mode))


;;
;;; Terminals

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.01)

  (defun my/vterm-delete-word ()
    "Binding function to delete a word."
    (interactive)
    (vterm-send-key (kbd "C-w"))))

(setq vterm-always-compile-module t)

;; Make sure to always display the modeline when using vterm. I feel like even
;; in a terminal, its still useful to see the modeline and its information.
(remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)


;;
;;; Org

(defvar my-notes-directory "~/org"
  "Where I'm storing my notes.")

;; Org mode
;; doc: https://orgmode.org/manual/
(after! org
  (setq org-directory my-notes-directory
        org-hide-emphasis-markers nil
        org-pretty-entities t
        org-ellipsis "…")

  ;; Do not wrap lines when converting to markdown.
  (setq org-pandoc-options-for-markdown '((wrap . "none"))))

;; Org modern
;; doc: https://github.com/minad/org-modern
(use-package! org-modern
  :hook (org-mode . org-modern-mode))

;; Deft
;; doc: https://github.com/jrblevin/deft
(after! deft
  (setq deft-directory my-notes-directory))

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

  (setq my-user-mail-address-2 (cdr (assoc "sws" my-email-addresses-alist)))

  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        '((user-mail-address . "personal/sent -inbox +sent -unread")
          (my-user-mail-address-2 . "sws/sent -inbox +sent -unread"))))


;;
;;; Misc

;; Insert random text
;; doc: https://github.com/jschaf/emacs-lorem-ipsum
(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list)
  :init
  (map! (:leader ((:prefix ("l" . "lorem")
                   :desc "Insert paragraphs" "p" #'lorem-ipsum-insert-paragraphs
                   :desc "Insert sentences"  "s" #'lorem-ipsum-insert-sentences
                   :desc "Insert list"       "l" #'lorem-ipsum-insert-list)))))

;; Untappd
;; doc: https://github.com/smallwat3r/untappd.el
(use-package! untappd
  :commands (untappd-feed)
  :custom (untappd-access-token (auth-source-pass-get 'secret "untappd/token")))

;; Look up
(setq +lookup-provider-url-alist
      (append +lookup-provider-url-alist
              '(("Python Docs" "https://docs.python.org/3/search.html?q=%s")
                ("Sourcegraph" "https://sourcegraph.com/search?q=context:global+%s&patternType=literal"))))

;; Clipboard history, interact with Flycut.app
;; doc: https://github.com/redguardtoo/cliphist
(use-package! cliphist
  :commands (cliphist-select-item cliphist-paste-item)
  :init
  (map! (:leader ((:prefix ("C" . "clipboard")
                   :desc "Select item" "s" #'cliphist-select-item
                   :desc "Paste item"  "p" #'cliphist-paste-item)))))

;; Bitwarden
;; doc: https://github.com/seanfarley/emacs-bitwarden
(use-package! bitwarden
  :custom
  (bitwarden-user user-mail-address)
  (bitwarden-automatic-unlock (lambda ()
                                (auth-source-pass-get 'secret "bitwarden/password")))
  :init
  (map! (:leader (:prefix "P"
                  :desc "Bitwarden login"    "l" #'bitwarden-login
                  :desc "Bitwarden unlock"   "u" #'bitwarden-unlock
                  :desc "Bitwarden lock"     "L" #'bitwarden-lock
                  :desc "Bitwarden list all" "b" #'bitwarden-list-all))))

;; Elfeed, web feed reader (RSS)
;; doc: https://github.com/skeeto/elfeed
(after! elfeed
  ;; Fetch feeds from a month ago.
  (setq elfeed-search-filter "@1-month-ago")

  ;; Hook on new entries.
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "github.com/smallwat3r.private"
                                :add '(github perso)))

  (defface my-github-elfeed-entry-face '((t :foreground "cyan4"))
    "Face for a Github related Elfeed entry.")

  (defface my-python-elfeed-entry-face '((t :foreground "IndianRed4"))
    "Face for a Python related Elfeed entry.")

  (defface my-emacs-elfeed-entry-face '((t :foreground "purple"))
    "Face for an Emacs related Elfeed entry.")

  ;; Distinguish faces depending on tags for Elfeed entries.
  (push '(github my-github-elfeed-entry-face) elfeed-search-face-alist)
  (push '(python my-python-elfeed-entry-face) elfeed-search-face-alist)
  (push '(emacs my-emacs-elfeed-entry-face) elfeed-search-face-alist)

  ;; Set up feeds.
  (setq elfeed-feeds
        '(("https://www.reddit.com/r/emacs.rss" reddit emacs)
          ("https://github.com/doomemacs/doomemacs/commits/master.atom" emacs doom)
          ("https://realpython.com/atom.xml?format=xml" python)
          ("http://feeds.feedburner.com/PythonInsider" python)))

  ;; Add private Github RSS feed to list of feeds. This needs to fetch my Github RSS token,
  ;; so this is done separately.
  (setq my-github-rss-feed (format "https://github.com/smallwat3r.private.atom?token=%s"
                              (auth-source-pass-get 'secret "github/rss/token")))
  (add-to-list 'elfeed-feeds (list my-github-rss-feed))

  ;; Rename some feeds titles.
  (defadvice elfeed-search-update (before configure-elfeed-search-update activate)
    (let ((github-feed (elfeed-db-get-feed my-github-rss-feed))
          (git-doom-feed (elfeed-db-get-feed "https://github.com/doomemacs/doomemacs/commits/master.atom")))
      (setf (elfeed-feed-title github-feed) "Github feed")
      (setf (elfeed-feed-title git-doom-feed) "Doom Emacs commits"))))

;; Turn this off as images wouldn't render correctly with `line-spacing' set
;; greater than zero.
(setq +rss-enable-sliced-images nil)


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


;;
;;; Bindings

;; Bundle most of my custom bindings for using Doom Emacs. Some bindings are not
;; included here, when they are specific to packages and cannot be bundled outside
;; of the package configuration.
;;
;; It's important for these to be loaded in last so it makes sure these are not
;; getting overriden.

;; Disable bindings, due to other conflicts (specially coming from Hammerspoon).
(map! "M-k" nil
      "M-j" nil)

(map!
 ;; For some reason, Emacs wouldn't print # on ^3, even when configured with
 ;; an English UK keyboard. Re-enable this behaviour.
 (:map key-translation-map
  "M-3" "#")

 (:map evil-insert-state-map
  "C-h" #'evil-backward-char
  "C-l" #'evil-forward-char
  "C-k" #'evil-previous-line
  "C-j" #'evil-next-line)

 (:map evil-visual-state-map
  ";f"  #'+format/region)

 (:map evil-normal-state-map
  "C-2"   #'my/scroll-up
  "C-1"   #'my/scroll-down
  "S-C-h" #'my/enlarge-window-horizontally
  "S-C-l" #'my/shrink-window-horizontally
  "S-C-k" #'my/enlarge-window
  "S-C-j" #'my/shrink-window
  "M-SPC" #'cycle-spacing
  "M-o"   #'delete-blank-lines
  ";d"    #'my/save-and-close-buffer
  ";w"    #'my/save-buffer
  "C-k"   #'join-line
  "B"     #'beginning-of-line-text
  "E"     #'end-of-line)

 (:after python
  (:map python-mode-map
   (:leader
    (:localleader
     :desc "Open Python repl" "r" #'my/open-python-repl
     (:prefix ("e" . "env")
      :desc "Deactivate venv" "d" #'my/deactivate-python-venv
      :desc "Activate venv"   "a" #'my/activate-closest-python-venv)))))

 (:after vterm
  (:map vterm-mode-map
   :n "B"          #'vterm-beginning-of-line
   :n "<return>"   #'evil-insert-resume
   "<C-backspace>" #'my/vterm-delete-word
   :in "C-k"       #'vterm-send-up
   :in "C-j"       #'vterm-send-down))

 (:leader
  "§" #'other-frame
  "1" #'my/where-am-i

  (:prefix "b"
   :desc "Kill buffer" "d" #'my/kill-buffer)

  (:prefix "f"
   :desc "Find file in dotfiles" "." #'my/find-file-in-dotfiles)

  (:prefix "o"
   :desc "Terminal"               "1" #'my/terminal-here
   :desc "Link at point"          "l" #'browse-url-at-point
   :desc "Vterm at root"          "T" #'+vterm/here
   :desc "Toggle vterm at root"   "t" #'+vterm/toggle
   :desc "Vterm at buffer"        "V" #'my/vterm/here-current-buffer
   :desc "Toggle vterm at buffer" "v" #'my/vterm/toggle-current-buffer

   (:prefix ("s" . "Scratch buffer")
    :desc "Current mode" "o" #'scratch
    :desc "Restclient"   "r" #'my/scratch-rest-mode))

  (:prefix "t"
   :desc "Truncate lines" "t" #'toggle-truncate-lines
   :desc "Imenu"          "i" #'imenu-list-smart-toggle)

  (:prefix "p"
   :desc "Run Makefile target" "m" #'+make/run)

  (:prefix ("P" . "password")
   :desc "Open password-store buffer" "p" #'pass)

  (:prefix "s"
   :desc "Search project (at point)" "w" #'my/vertico-search-project-symbol-at-point)))