;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; General

(defconst my-user-alias "smallwat3r"
  "User alias.")

(defvar my-email-addresses-alist
  '(("gmail"      . "mpetiteau.pro@gmail.com")
    ("smallwat3r" . "matt@smallwat3r.com"))
  "Alist of my email addresses.")

(defun my/get-email (name)
  "Helper function to get email address by NAME."
  (cdr (assoc name my-email-addresses-alist)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address (my/get-email "gmail"))

(setq default-directory "~/")

(defvar my-dotfiles-dir (concat default-directory "dotfiles")
  "Directory containing my dotfiles.")

(defconst my-system-os
  (if (executable-find "uname")
      (format "%s" (cdr (doom-call-process "uname" "-sr")))
    "Unknown OS")
  "Operating system name and version.")

(defconst my-system-distro
  (string-replace "\"" "" (doom-system-distro-version))
  "System distro name and version.")

(defconst my-hardware-vendor
  (let ((board-vendor-file "/sys/devices/virtual/dmi/id/board_vendor"))
    (if (file-exists-p board-vendor-file)
        (format "%s" (cdr (doom-call-process "cat" board-vendor-file)))
      ""))
  "Hardware vendor name.")

;; works on GPD 3 and 4, not tested on other versions
(defconst IS-GPD (string= my-hardware-vendor "GPD")
  "Is it running on a GPD?")

(when (featurep :system 'macos)
  ;; stuff installed via homebrew
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; ensure cargo binaries are included
(add-to-list 'exec-path "~/.cargo/bin")

;;
;;; Frame

(setq default-frame-alist
      (append (list
               '(fullscreen . maximized)
               '(drag-internal-border . t)
               '(internal-border-width . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0)
               '(inhibit-double-buffering . t)
               '(vertical-scroll-bars . nil))))

(defvar my-title-emacs-version (concat "Emacs " emacs-version)
  "Running Emacs version as a title.")

(setq frame-title-format (concat my-title-emacs-version " - %b"))

(when (featurep 'ns)
  ;; ns stands for NextStep, it's the interface used with OSX.
  (setq ns-use-thin-smoothing t
        ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        ns-antialias-text t))


;;
;;; Editor

;; Fonts

(defun my/get-font-size-based-on-os ()
  "Return a font size depending on the OS name."
  (let ((os (downcase (or (doom-system-distro-version) ""))))
    (cond
     ((string-match-p "ubuntu" os) 22)
     ((string-match-p "fedora" os) 17)
     (t 16))))

(setq my-font-size (my/get-font-size-based-on-os))
(setq doom-font (font-spec :family "Triplicate A Code"
                           :size my-font-size))
(setq doom-variable-pitch-font doom-font)

;; Enable proportional fonts for text-mode buffers.
(add-hook! 'text-mode-hook 'variable-pitch-mode)

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default tab-width 8
              with-editor-emacsclient-executable "emacsclient")

(setq-default line-spacing 1)
(when (not (= line-spacing 0))
  ;; images would not render correctly if `line-spacing' is not 0
  (setq +rss-enable-sliced-images nil))

;; Theme
(setq doom-theme 'creamy)

;; Remove highlighting on some major programming faces. I think it makes
;; the buffer too busy and difficult to read.
(custom-set-faces!
  '((font-lock-function-name-face
     font-lock-variable-name-face
     font-lock-constant-face
     font-lock-builtin-face
     font-lock-type-face)
    :foreground unspecified :weight normal))

;; colorize line numbers every 5th lines as a visual indicator, this is
;; specially useful when using relative line numbers.
(after! display-line-numbers
  (setq display-line-numbers-type 'relative
        display-line-numbers-minor-tick 5
        display-line-numbers-major-tick 5))
(custom-set-faces!
  ;; base line numbers: no gray background
  '(line-number :background unspecified :foreground "gray50")
  ;; multiples of 5 (minor/major tick)
  '(line-number-minor-tick :inherit line-number :foreground "orange" :weight bold)
  '(line-number-major-tick :inherit line-number-minor-tick)
  ;; current line number: distinct color
  '(line-number-current-line :inherit line-number :foreground "orange red" :weight bold))

;; highlight numbers
;; doc: https://github.com/Fanael/highlight-numbers
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; rainbow parenthesis in some major modes
;; doc: https://github.com/Fanael/rainbow-delimiters
(use-package! rainbow-delimiters
  :hook ((c-mode-common emacs-lisp-mode lisp-mode typescript-mode typescript-tsx-mode)
         . rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 4))

;; Dashboard displayed when starting Emacs. As a personal preference, I like to
;; keep it very simple. It is ligther than the default scratch buffer in many
;; cases. But it can also not be killed, hence remembers the working directory
;; of the last open buffer, `find-file' will work from the directory I expect.
(defun my/dashboard-message ()
  (insert (concat "MAIN BUFFER\n"
                  my-title-emacs-version
                  " on " my-system-distro " (" my-system-os ")\n"
                  "Built for: " system-configuration)))

(setq +doom-dashboard-functions '(my/dashboard-message))

(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    ;; Do not display the cursor on the dashboard.
    (setq-local evil-normal-state-cursor (list nil))))

(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)

(setq scroll-margin 0
      ;; Makes underlines render a bit cleaner.
      x-underline-at-descent-line t
      ;; No confirmation can be annoying as I realised it often happens by
      ;; mistake.
      confirm-kill-emacs 'yes-or-no-p)

;; Evil-mode
(after! evil
  ;; General evil mode settings.
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t)

  (map! :map evil-insert-state-map
        "C-h" #'evil-backward-char
        "C-l" #'evil-forward-char
        "C-k" #'evil-previous-line
        "C-j" #'evil-next-line

        :map evil-visual-state-map
        ";f"  #'+format/region

        :map evil-normal-state-map
        ;; Window scrolling
        "C-;"   #'my/scroll-up
        "C-l"   #'my/scroll-down

        ;; Window resizing
        "S-C-h" #'my/enlarge-window-horizontally
        "S-C-y" #'my/enlarge-window-horizontally  ; custom layout
        "S-C-l" #'my/shrink-window-horizontally
        "S-C-n" #'my/shrink-window-horizontally  ; custom layout
        "S-C-k" #'my/enlarge-window
        "S-C-a" #'my/enlarge-window  ; custom layout
        "S-C-e" #'my/enlarge-window
        "S-C-j" #'my/shrink-window
        "S-C-i" #'my/shrink-window  ; custom layout

        ;; Misc editing
        "M-SPC" #'cycle-spacing
        "M-o"   #'delete-blank-lines
        "C-k"   #'join-line
        "C-a"   #'join-line  ; custom layout
        "B"     #'beginning-of-line-text
        "E"     #'end-of-line
        "M-<delete>" #'kill-word
        "C-n"   #'electric-newline-and-maybe-indent

        ;; Buffer management
        ";d"    #'my/save-and-close-buffer
        ";w"    #'save-buffer
        ";s"    #'save-buffer
        ";q"    #'my/kill-buffer

        :leader
        ;; Window management
        "wy" #'evil-window-left
        "ly" #'evil-window-left  ; custom layout
        "wn" #'evil-window-down
        "ln" #'evil-window-down  ; custom layout
        "wa" #'evil-window-up
        "la" #'evil-window-up    ; custom layout
        "we" #'evil-window-right
        "le" #'evil-window-right ; custom layout
        "ls" #'evil-window-split
        "lv" #'evil-window-vsplit)

  ;; Change the cursor color depending on the evil mode
  (setq evil-default-state-cursor  '(box "cyan3")
        evil-normal-state-cursor   '(box "cyan3")
        evil-insert-state-cursor   '(bar "green3")
        evil-visual-state-cursor   '(box "OrangeRed2")
        evil-replace-state-cursor  '(hbar "red2")
        evil-operator-state-cursor '(box "red2")))

;; Evil visual hints when yanking, pasting, deleting etc.
;; doc: https://github.com/edkolev/evil-goggles
(after! evil-goggles
  (setq evil-goggles-duration 0.15)
  (evil-goggles-use-diff-refine-faces))

;; Evil snipe
;; doc: https://github.com/hlissner/evil-snipe
(after! evil-snipe
  (setq evil-snipe-scope 'visible)
  (map! :map evil-snipe-parent-transient-map
        :g "j" #'evil-snipe-repeat
        :g "k" #'evil-snipe-repeat-reverse
        :g "n" #'evil-snipe-repeat         ; custom layout
        :g "a" #'evil-snipe-repeat-reverse ; custom layout
        ))

;; Icons
;; doc: https://github.com/domtronn/all-the-icons.el
(after! all-the-icons
  ;; Keep icons small.
  (if (featurep :system 'macos)
      (setq all-the-icons-scale-factor 0)
    (setq all-the-icons-scale-factor 0.8))
  (setq all-the-icons-default-adjust 0))

(setq my-browse-url-qutebrowser-arguments nil)

(defun my/browse-url-qutebrowser (url &optional _new-window)
  "Adapted to Qutebrowser from `browse-url-chrome'"
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "qutebrowser" url) nil
           (executable-find "qutebrowser")
           (append
            my-browse-url-qutebrowser-arguments
            (list url)))))

(function-put 'my/browse-url-qutebrowser 'browse-url-browser-kind 'external)

;; Browse stuff in qutebrowser as default when using Linux.
(when (and (featurep :system 'linux) (executable-find "qutebrowser"))
  (setq browse-url-browser-function #'my/browse-url-qutebrowser))

;; Magit
;; doc: https://github.com/magit/magit
(after! magit
  ;; These bindings are hard to work with when using evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil)

  ;; do not show line numbers in the commit buffer
  (setq-hook! 'git-commit-mode-hook display-line-numbers nil)

  (after! git-commit
    (setq git-commit-summary-max-length 75))

  ;; Remap keys to move commits up or down when using interactive rebase.
  (after! git-rebase
    (map! :map git-rebase-mode-map
          "K" #'git-rebase-move-line-up
          "J" #'git-rebase-move-line-down
          "N" #'git-rebase-move-line-up   ; custom layout
          "A" #'git-rebase-move-line-down ; custom layout
          )))

;; all Git SSH commands from Emacs should use this
(setenv "GIT_SSH_COMMAND" "ssh -4 \
  -o ConnectTimeout=10 \
  -o ServerAliveInterval=20 \
  -o ServerAliveCountMax=3 \
  -o TCPKeepAlive=yes \
  -o GSSAPIAuthentication=no \
  -o ControlMaster=no")

;; git-timemachine
;; doc: https://github.com/emacsmirror/git-timemachine
(after! git-timemachine
  ;; custom layout support
  (map! :map git-timemachine-mode-map
        :n "C-n" #'git-timemachine-show-previous-revision
        :n "C-a" #'git-timemachine-show-next-revision))

;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2))

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

;; Goto-address mode. This mode activates and highlights URLs and email
;; addresses in the current buffer.
(use-package! goto-addr
  :hook
  (prog-mode . goto-address-prog-mode)
  ((text-mode vterm-mode restclient-mode compilation-mode) . goto-address-mode)
  :custom
  (goto-address-mail-regexp
   "\\w+\\(\\.\\w+\\)?\\(\\+\\w+\\)?@\\(\\w\\|\\.\\)+\\.\\w+")
  (goto-address-mail-face 'my-goto-address-mail-face)
  :init
  (defface my-goto-address-mail-face '((t :italic nil :underline t))
    "Face for email address."
    :group 'basic-faces))

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
  :commands (symbol-overlay-put
             symbol-overlay-remove-all)
  :init
  (map! :leader
        :prefix "c"
        :desc "Add overlay"     "h" #'symbol-overlay-put
        :desc "Remove overlays" "H" #'symbol-overlay-remove-all)
  :config
  ;; Deactivate binding for `symbol-overlay-map-help', as it conflicts with
  ;; evil. Remap it to a capital 'H' instead.
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "H") #'symbol-overlay-map-help)

  ;; Other overlay bindings I don't use which could conflict with evil
  ;; operations.
  (define-key symbol-overlay-map (kbd "w") nil)
  (define-key symbol-overlay-map (kbd "t") nil)
  (define-key symbol-overlay-map (kbd "i") nil))

;; Abbreviations
(use-package! abbrev
  :custom
  (save-abbrevs nil)
  (abbrev-file-name (expand-file-name "abbrev_defs.el" doom-user-dir))
  :config
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

(setq-default abbrev-mode t)

;; Custom File, used by Emacs to cache some data related to its config.
(use-package! cus-edit
  :custom (custom-file (expand-file-name ".custom.el" doom-user-dir))
  :config
  (when (file-exists-p custom-file)
    (load custom-file t)))

(defun my/post-save-hook ()
  "Show a confirmation message after saving."
  (when buffer-file-name
    (message "Saved: %s" (abbreviate-file-name buffer-file-name))))
(add-hook 'after-save-hook #'my/post-save-hook)

;; add a confirmation message after calling `save-buffer'
(defadvice save-buffer (after my/save-buffer-confirmation activate)
  (message "Saved `%s'" (buffer-name)))

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
        projectile-project-search-path '("~/dotfiles/" "~/code/" "~/work/"))

  (when (featurep :system 'linux)
    (dolist (file (directory-files "~/" t))
      (when (and (file-directory-p file)
                 (string-suffix-p "_ws" file))
        (add-to-list 'projectile-project-search-path file))))

  (pushnew! projectile-globally-ignored-directories
            ".npm" ".poetry" "GoogleDrive" ".mypy_cache"
            "Library" ".git" "__pycache__" "node_modules"
            ".idea" ".vscode" ".svn" ".tox" ".cache")
  (pushnew! projectile-ignored-projects "/tmp" "~/Downloads" "~/backups")
  (when (featurep :system 'macos)
    (pushnew! projectile-ignored-projects "/Applications" "/Volumes/GoogleDrive")))


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
  :config
  (map! :map dired-mode-map
        :n "/" #'dired-narrow-fuzzy))

;; Toggle directories with TAB in dired
(use-package! dired-subtree
  :after dired
  :config
  (map! :map dired-mode-map
        "<tab>" #'dired-subtree-toggle
        "<backtab>" #'dired-subtree-cycle)

  ;; set a transparent background for all levels in dired subtree
  (custom-set-faces!
    `(,(cl-loop for i from 0 to 6 collect
                (intern (format "dired-subtree-depth-%d-face" i)))
      :background unspecified)))

;; Treemacs
;; doc: https://github.com/Alexander-Miller/treemacs
(after! treemacs
  (setq doom-themes-treemacs-enable-variable-pitch t
        doom-themes-treemacs-line-spacing 0
        doom-themes-treemacs-theme "doom-colors"
        treemacs-width 50
        ;; popups have been disabled so switching back and forth with the
        ;; treemacs buffer using the evil bindings does not work. This restores
        ;; the original behaviour.
        treemacs-is-never-other-window nil)
  (treemacs-resize-icons 14))


;;
;;; Completion frameworks

;; Code completion
;; doc: https://github.com/minad/corfu
(after! corfu
  (setq corfu-count 7
        corfu-preselect 'first
        corfu-preview-current nil
        +corfu-want-minibuffer-completion nil))

;; vertical interactive completion
;; doc: https://github.com/minad/vertico
(after! vertico
  (setq vertico-count 15)
  (map! (:leader
         (:prefix "s"
          ;; Search a symbol at point using Vertico
          :desc "Search project (at point)" "w" #'my/vertico-search-project-symbol-at-point
          ;; Repeat the last Vertico search. Doom also allow this with <SPC '>
          ;; but I find it easier to remember memo-technically with <SPC s .>
          :desc "Repeat last search" "." #'vertico-repeat))))


;;
;;; Programming

;; LSP
(after! eglot
  (map! :map eglot-mode-map
        :leader
        :prefix "r"
        :desc "Reconnect Eglot" "w" #'eglot-reconnect))

;; remove intrusive hints from Eglot
(add-hook! 'eglot-managed-mode-hook (eglot-inlay-hints-mode -1))

(set-eglot-client! 'python-mode '("basedpyright-langserver" "--stdio"))

;; Shell scripts (bash, zsh...)
(after! sh-mode
  ;; Formatter
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2" ; nb of spaces used for indentation
      "-ci"    ; indent switch cases
      "-bn")   ; binary ops may start a line
    :modes '(sh-mode)))

(setq-hook! 'sh-mode-hook
  flycheck-checker 'sh-shellcheck
  flycheck-shellcheck-excluded-warnings '("SC1091")
  sh-basic-offset 2
  indent-tabs-mode nil)

;; Check for spelling mistakes
;; doc: https://gitlab.com/ideasman42/emacs-spell-fu
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

;; spell-fu is by default enabled in some modes, but I find this quite
;; annoying, so force it to be disabled, and we can explicitly enable it
;; if we need to use it.
(remove-hook! (text-mode yaml-mode) #'spell-fu-mode)

;; Flycheck
;; doc: https://github.com/flycheck/flycheck
(after! flycheck
  ;; disable outdated checkers
  (setq flycheck-disabled-checkers '(python-flake8 python-pylint))
  (map! :map flycheck-mode-map
        :leader
        :localleader
        :desc "Flycheck list errors" "l" #'flycheck-list-errors))

;; only activate flycheck on demand, too intrusive
(remove-hook! 'eglot-managed-mode-hook #'flycheck-eglot-mode)
(remove-hook! 'doom-first-buffer-hook #'global-flycheck-mode)

;; Flycheck pop-up tooltips
;; doc: https://github.com/flycheck/flycheck-popup-tip
(after! flycheck-popup-tip
  (setq flycheck-popup-tip-error-prefix "(!) "))

;; Python
(after! python
  (defvar my-default-python-line-length 88
    "Default python line length.")

  ;; Disable annoying warnings about `python-shell-interpreter' readline
  ;; support.
  (setq python-shell-completion-native-enable nil)

  ;; Isort
  (after! py-isort
    (setq py-isort-options '("--trailing-comma" "--use-parentheses"))
    (add-to-list 'py-isort-options (format "-l %s"
                                           my-default-python-line-length)))

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
    (setq dap-python-debugger 'debugpy))

  (defun my/python-to-fstring ()
    "Make string to fstring."
    (interactive)
    (when (nth 3 (syntax-ppss))
      (let ((p (point)))
        (goto-char (nth 8 (syntax-ppss)))
        (insert "f")
        (goto-char p)
        (forward-char))))

  (map! :map python-mode-map
        :leader
        :localleader
        :desc "To f-string"     "f"   #'my/python-to-fstring
        :desc "Deactivate venv" "e d" #'my/deactivate-python-venv
        :desc "Activate venv"   "e a" #'my/activate-closest-python-venv)

  (add-to-list '+lookup-provider-url-alist
               '("Python Docs" "https://docs.python.org/3/search.html?q=%s")))

;; PET (P ython E xecutable T racker)
;; doc: https://github.com/wyuenho/emacs-pet/
(use-package! pet
  :config
  (add-hook 'python-mode-hook 'pet-mode -10))

;; Javascript
(after! js2-mode
  ;; Formatter
  (set-formatter! 'prettier
    '("prettier"
      "--print-width" "120"
      ("--stdin-filepath" "%s" buffer-file-name))
    :modes '(js2-mode)))

(setq-hook! 'js2-mode-hook js2-basic-offset 2)

;; Json
(setq-hook! 'json-mode-hook tab-width 2)

;; Restclient
(setq-hook! 'restclient-mode-hook tab-width 4)

;; Golang
(setq-hook! 'go-mode-hook indent-tabs-mode t)

;; Web mode
(defun my/web-mode-configs ()
  (setq-local tab-width 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-script-padding 2
              web-mode-style-padding 2))

(add-hook! 'web-mode-hook #'my/web-mode-configs)

;; Disable formatters for html and web modes
(setq-hook! '(html-mode-hook web-mode-hook)
  +format-with :none)

;; SQL
(use-package! sql
  :mode ("\\.\\(m\\|my\\)?sql\\'" . sql-mode)
  :custom
  ;; mostly used for local development only so disable SSL mode
  ;; by default to ease connectivity from localhost
  (sql-mysql-options '("--ssl-mode=DISABLED"))
  (sql-mysql-login-params '((user :default "root")
                            password
                            database
                            (server :default "127.0.0.1")
                            (port :default 3306)))
  (sql-postgres-login-params '((user :default "postgres")
                               password
                               database
                               (server :default "127.0.0.1")
                               (port :default 5432))))

;; Makefile
(use-package! makefile-mode
  :mode ("Makefile.*" . makefile-mode))

;; Yaml mode
;; doc: https://github.com/yoshiki/yaml-mode
(use-package! yaml-mode
  :mode ("\\.\\(yaml\\|yml\\)\\'" . yaml-mode)
  :config
  (defun my/remap-yaml-faces ()
    (face-remap-add-relative 'font-lock-variable-name-face
                             :inherit font-lock-keyword-face))

  ;; The above seems to leave `buffer-face-mode' on, disable it.
  (advice-add #'my/remap-yaml-faces :after (lambda (&rest _) (buffer-face-mode -1)))

  (add-hook! 'yaml-mode-hook #'my/remap-yaml-faces))

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

;; ROS launch files
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Lua
(add-to-list 'auto-mode-alist '("conky\\.conf\\'" . lua-mode))

;; ssh config mode
(add-to-list 'auto-mode-alist
             '("/\\.ssh/\\(?:work\\|private\\)\\'" . ssh-config-mode))


;;
;;; Terminals

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.01)

  (map! :map vterm-mode-map
        :n "B" #'vterm-beginning-of-line  ; beg of command
        :n "<return>" #'evil-insert-resume
        [remap delete-forward-char] #'vterm-send-delete
        :in "<M-backspace>" #'vterm-send-meta-backspace
        :n "<M-backspace>"  #'vterm-send-meta-backspace
        :in "C-k" #'vterm-send-up
        :in "C-j" #'vterm-send-down))

;; remote file access
(after! tramp
  (tramp-set-completion-function
   "ssh" '((tramp-parse-sconfig "~/.ssh/config")
           (tramp-parse-sconfig "~/.ssh/work")
           (tramp-parse-sconfig "~/.ssh/private")
           (tramp-parse-sconfig "/etc/ssh_config")
           (tramp-parse-shosts "/etc/hosts")
           (tramp-parse-shosts "~/.ssh/known_hosts"))))

(defun my/vterm-tramp-base-path ()
  "Returns the base tramp path of a Tramp buffer."
  (let* ((vec (or (car (tramp-list-connections))
                  (when (tramp-tramp-file-p default-directory)
                    (tramp-dissect-file-name default-directory))))
         (method (and vec (tramp-file-name-method vec)))
         (user   (and vec (tramp-file-name-user vec)))
         (host   (and vec (tramp-file-name-host vec))))
    (when (and method host)
      (format "/%s:%s%s"
              method
              (if (and user (not (string-empty-p user))) (concat user "@") "")
              host))))

(defun my/vterm-buffer-hooks-on-tramp ()
  "Useful hooks to run when using Vterm with Tramp."
  (when (and (eq major-mode 'vterm-mode)
             default-directory
             (file-remote-p default-directory))
    (let ((tramp-base-path (my/vterm-tramp-base-path)))
      (rename-buffer (format "*vterm@%s*" tramp-base-path) t)
      (vterm-send-string
       (format "e() { printf \"\\033]51;Efind-file %s:%s\\007\" \"$(pwd)/$1\"; } \n"
               tramp-base-path "%s")))
    (vterm-send-string "clear\n")))

;; inject to the remote shell a function that can edit remote files in
;; a local Emacs buffer using "e" in Vterm (e.g. "e .bashrc").
(add-hook! 'vterm-mode-hook #'my/vterm-buffer-hooks-on-tramp)

;; provides extra convenience functions for vterm
;; doc: https://github.com/Sbozzolo/vterm-extra
(use-package! vterm-extra
  :after vterm
  :bind (("s-t" . vterm-extra-dispatcher)
         :map vterm-mode-map
         (("C-c C-e" . vterm-extra-edit-command-in-new-buffer))))

(map! :leader
      :prefix "o"
      :desc "Terminal"               "1" #'my/terminal-here
      :desc "Vterm at root"          "T" #'+vterm/here
      :desc "Toggle vterm at root"   "t" #'+vterm/toggle
      :desc "Vterm at buffer"        "V" #'my/vterm/here-current-buffer
      :desc "Toggle vterm at buffer" "v" #'my/vterm/toggle-current-buffer)

(setq vterm-always-compile-module t)

;; Make sure to always display the modeline when using vterm. I feel like even
;; in a terminal, its still useful to see the modeline and its information.
(remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

;; LLMs
(setq gptel-directives
'((default   . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
        (assistant . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
        (writing   . "You are a large language model and a writing assistant. Respond concisely.")))
(after! gptel
  (setq gptel-model "gemini-2.5-pro"
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (auth-source-pass-get 'secret "gemini/key")
                        :stream t)))

;;
;;; GPG

(use-package! pinentry
  :config (pinentry-start))


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

(if (featurep :system 'macos)
    (setq sendmail-program "/opt/homebrew/bin/msmtp")
  (setq sendmail-program "/usr/bin/msmtp"))

(setq mail-user-agent 'message-user-agent
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

  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))

  ;; Use a custom command to fetch for new emails with mbsync
  (setq +notmuch-sync-backend "mbsync -a && notmuch new")

  (setq my-user-mail-address-2 (my/get-email "smallwat3r"))

  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        '((user-mail-address . "personal/sent -inbox +sent -unread")
          (my-user-mail-address-2 . "sws/sent -inbox +sent -unread"))))


;;
;;; Misc


;; Untappd
;; doc: https://github.com/smallwat3r/untappd.el
(use-package! untappd
  :commands (untappd-feed)
  :custom (untappd-access-token (auth-source-pass-get 'secret "untappd/token")))

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
          ("https://github.com/doomemacs/doomemacs/commits/master.atom"
           emacs doom)
          ("https://realpython.com/atom.xml?format=xml" python)
          ("http://feeds.feedburner.com/PythonInsider" python)))

  ;; Add private Github RSS feed to list of feeds. This needs to fetch my
  ;; Github RSS token, so this is done separately.
  (setq my-github-rss-feed
        (format "https://github.com/smallwat3r.private.atom?token=%s"
                (auth-source-pass-get 'secret "github/rss/token")))
  (add-to-list 'elfeed-feeds (list my-github-rss-feed))

  (defun my/configure-elfeed-search-update (&rest _)
    "Rename some elfeed feeds."
    (when-let ((feed (elfeed-db-get-feed my-github-rss-feed)))
      (setf (elfeed-feed-title feed) "Github feed"))
    (when-let ((feed (elfeed-db-get-feed "https://github.com/doomemacs/doomemacs/commits/master.atom")))
      (setf (elfeed-feed-title feed) "Doom Emacs commits")))
  (advice-add 'elfeed-search-update :before #'my/configure-elfeed-search-update))

;; debug mode
(defun my/echo-command-name-hook ()
  "Echo live command names."
  (unless (or (eq this-command 'self-insert-command)
              (eq this-command 'next-line))
    (message "%s" this-command)))

(define-minor-mode my-debug-mode
  "A minor mode to echo executed commands."
  :init-value nil
  :lighter " Debug"
  :global t
  (if my-debug-mode
      (add-hook 'post-command-hook #'my/echo-command-name-hook)
    (remove-hook 'post-command-hook #'my/echo-command-name-hook)))


;;
;;; Bindings

;; Bundle most of my custom bindings for using Doom Emacs. Some bindings are not
;; included here, when they are specific to packages and cannot be bundled
;; outside of the package configuration.
;;
;; It's important for these to be loaded (almost) in last so it makes sure
;; these are not getting overriden.

;; Disable bindings, due to other conflicts (specially coming from Hammerspoon).
(map! "M-k" nil
      "M-j" nil)

(when (featurep :system 'macos)
  ;; For some reason, Emacs wouldn't print # on ^3, even when configured with
  ;; an English UK keyboard. Re-enable this behaviour.
  (map! (:map key-translation-map "M-3" "#")))

(map!
 (:leader
  "§" #'other-frame
  "1" #'my/where-am-i

  (:prefix "w"
   :desc "Window switch" "w" #'persp-window-switch)

  (:prefix "b"
   :desc "Kill buffer"      "k" #'my/kill-buffer
   :desc "Kill all buffers" "K" #'my/kill-all-buffers
   :desc "Kill buffer"      "d" #'my/kill-buffer
   :desc "Kill all buffers" "D" #'my/kill-all-buffers
   :desc "Kill buffers not current" "q" #'my/kill-all-buffers-except-current)

  (:prefix "f"
   :desc "Find file in dotfiles" "." #'my/find-file-in-dotfiles)

  (:prefix "o"
   :desc "Browse URL at point" "l" #'browse-url-at-point
   :desc "Remote SSH conn"     "." #'my/open-remote-conn)

  (:prefix ("e" . "edit")
   :desc "Yank from killring" "p" #'yank-from-kill-ring)

  (:prefix "t"
   :desc "Theme toggle"   "T" #'my/theme-toggle
   :desc "Truncate lines" "t" #'toggle-truncate-lines
   :desc "Imenu"          "i" #'imenu-list-smart-toggle)

  (:prefix "p"
   :desc "Run Makefile target" "m" #'+make/run)

  (:prefix ("P" . "password")
   :desc "Open password-store buffer" "p" #'pass)))
