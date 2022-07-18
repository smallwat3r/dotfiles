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
(use-package! abbrev
  :hook ((text-mode prog-mode) . abbrev-mode)
  :custom
  (save-abbrevs nil)
  (abbrev-file-name (expand-file-name "abbrev_defs" doom-private-dir))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; Custom File, used by Emacs to cache some data related to its config.
(use-package! cus-edit
  :custom (custom-file expand-file-name ".custom.el" doom-private-dir)
  :config
  (if (file-exists-p custom-file)
      (load custom-file t)))


;;
;;; Editor

;; Fonts
(let* ((font "Monaco")
       (font-size 13))
  (setq doom-font (font-spec :family font :size font-size)
        doom-serif-font (font-spec :family font :size font-size)
        doom-variable-pitch-font (font-spec :family font :size font-size)))

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default line-spacing 2)

;; Use a custom minimalistic theme.
(setq doom-theme 'smallwat3r)

(setq-default tab-width 8)
(setq-default with-editor-emacsclient-executable "emacsclient")

(setq display-line-numbers-type nil ; no line numbers
      scroll-margin 7               ; top and bottom margins to trigger scroll
      confirm-kill-emacs nil        ; quit emacs without confirmation
      load-prefer-newer t)          ; always load newer bytes compiled files

(after! evil
  ;; Change cursor color and shape based on evil mode.
  (setq evil-emacs-state-cursor '("DarkMagenta" box)
        evil-normal-state-cursor '("DarkMagenta" box)
        evil-visual-state-cursor '("DarkCyan" box)
        evil-insert-state-cursor '("DarkCyan" box)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  ;; General evil mode settings.
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t)

  ;; Prompt to select file after an evil window split action. Press ESC to cancel
  ;; and the split windown will be for the current file.
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (ido-find-file)))

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
  ;; These bindings are hard to work with as I'm running evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil))

;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2))

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

;; Goto-address mode. This mode activates and highlights URLs and email addresses
;; in the current buffer.
(use-package! goto-addr
  :hook (((compilation-mode
           text-mode
           restclient-mode
           eshell-mode
           vterm-mode
           shell-mode)
          . goto-address-mode)
         (prog-mode . goto-address-prog-mode))
  :custom
  (goto-address-mail-regexp "\\w+\\(\\.\\w+\\)?\\(\\+\\w+\\)?@\\(\\w\\|\\.\\)+\\.\\w+")
  (goto-address-mail-face '((t :background unspecified :inherit default :underline t))))

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
                  :desc "Remove overlays" "H" #'symbol-overlay-remove-all))))


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
  (setq projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-mode-line-function '(lambda () (format " P[%s]" (projectile-project-name))))

  (setq projectile-globally-ignored-directories
        '(".npm" ".poetry" "GoogleDrive" ".mypy_cache" "Library" ".git" "__pycache__"
          "node_modules" ".idea" ".vscode" ".svn" ".tox" ".cache"))

  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS" "*.pyc"))

  ;; Make the projectile command use fd with some more sensitive defaults, as I noticed some
  ;; performance issues with the one used by Doom or projectile.
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
  (setq lsp-enable-file-watchers nil)
  ;; Ignore asking to restart if server failed to boot.
  (setq lsp-restart 'ignore))

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
  (setq spell-fu-idle-delay 0.5))

;; spell-fu is by default enabled in text-mode, but I find this quite
;; annoying, so force it to be disabled, and we can explicitly enable it
;; if we need to use it.
(remove-hook! (text-mode) #'spell-fu-mode)

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
  ;; Disable annoying warnings about `python-shell-interpreter' readline support.
  (setq python-shell-completion-native-enable nil)

  (set-formatter! 'black
    '("black"
      "--quiet"
      "--line-length" "100"
      "-")  ; apply in file changes
    :modes '(python-mode))

  (after! lsp-pyright
    (setq lsp-pyright-python-executable-cmd python-shell-interpreter))

  (after! dap-mode
    (setq dap-python-debugger 'debugpy
          dap-python-executable python-shell-interpreter)))

;; Pytest
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

(use-package! sql
  :mode (("\\.\\(m\\|my\\)?sql\\'" . sql-mode))
  :config
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

;; Yaml mode
;; doc: https://github.com/yoshiki/yaml-mode
(use-package! yaml-mode
  :mode ("\\.\\(yaml\\|yml\\)\\'")
  :hook (yaml-mode . my/remap-yaml-faces)
  :config
  (defun my/remap-yaml-faces ()
    (face-remap-add-relative
     'font-lock-variable-name-face :inherit font-lock-keyword-face)))

;; Logs
;; doc: https://github.com/doublep/logview
(use-package! logview
  :mode ("\\.log.*" . logview-mode))


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
        org-hide-emphasis-markers t))

;; Deft
;; doc: https://github.com/jrblevin/deft
(after! deft
  (setq deft-directory my-notes-directory))

;; Make invisible parts of Org elements appear visible
;; doc: https://github.com/awth13/org-appear
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

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
                ("Sourcegraph" (concat "https://sourcegraph.com/search?"
                                       "q=context:global+%s&patternType=literal")))))

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
     (:prefix "i"
      :desc "Remove unused imports" "R" #'pyimport-remove-unused)
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
   :desc "Alacritty"              "a" #'my/alacritty-here
   :desc "Link at point"          "l" #'browse-url-at-point
   :desc "Vterm at root"          "T" #'+vterm/here
   :desc "Toggle vterm at root"   "t" #'+vterm/toggle
   :desc "Vterm at buffer"        "V" #'my/vterm/here-current-buffer
   :desc "Toggle vterm at buffer" "v" #'my/vterm/toggle-current-buffer
   :desc "Scr. buffer cur. mode"  "x" #'scratch
   :desc "Scr. buffer restclient" "h" #'my/scratch-rest-mode)

  (:prefix "t"
   :desc "Truncate lines" "t" #'toggle-truncate-lines)

  (:prefix "p"
   :desc "Run Makefile target" "m" #'+make/run)

  (:prefix ("P" . "password")
   :desc "Open password-store buffer" "p" #'pass)))
