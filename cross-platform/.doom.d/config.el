;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; General

(defconst my-user-alias "smallwat3r"
  "User alias.")

(defvar my-email-addresses-alist
  '(("gmail"      . "mpetiteau.pro@gmail.com")
    ("sws"        . "matthieu@smallwatersolutions.com")
    ("smallwat3r" . "matt@smallwat3r.com"))
  "Alist of my email addresses.")

(defun my--get-email (name)
  "Helper function to get email address by NAME."
  (cdr (assoc name my-email-addresses-alist)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address (my--get-email "gmail"))

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

;; at least this works to check if its running on a GPD Pocket 3, not sure
;; about the other GPD models...
(defconst IS-GPD (string= my-hardware-vendor "GPD")
  "Is it running on a GPD?")

(when (featurep :system 'macos)
  ;; stuff installed via homebrew
  (add-to-list 'exec-path "/opt/homebrew/bin"))


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

;; this is useful when changing from light and dark theme to have fonts with
;; different weigths, as often a thicker font renders better on a light
;; background, and a thiner font renders best of a darker background.
(defvar my-thinner-font "Triplicate B Code"
  "Thinner font family to use.")

(defvar my-thicker-font "Triplicate A Code"
  "Thicker font family to use.")

(setq my-font-size 16)
(setq doom-font (font-spec :family my-thinner-font
                           :size my-font-size
                           :hintstyle 3))
(setq doom-variable-pitch-font doom-font)

;; Enable proportional fonts for text-mode buffers.
(add-hook! 'text-mode-hook 'variable-pitch-mode)

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default tab-width 8
              with-editor-emacsclient-executable "emacsclient")

(setq-default line-spacing 3)
(when (not (= line-spacing 0))
  ;; images would not render correctly if `line-spacing' is not 0
  (setq +rss-enable-sliced-images nil))

;; Theme
(defvar my-light-theme 'smallwat3r
  "My light theme.")

(defvar my-dark-theme 'smallwat3r-dark
  "My dark theme.")

(defvar my-current-theme my-dark-theme
  "Current theme tracker.")

(defun my/theme-toggle ()
  "Toggle between dark and light theme."
  (interactive)
  (let* ((theme nil) (font nil))
    (if (eq my-light-theme my-current-theme)
        (setq theme my-dark-theme
              font my-thinner-font)
      (setq theme my-light-theme
            font my-thicker-font))
    (setq doom-font (font-spec :family font :size my-font-size))
    (doom/reload-font)
    (setq my-current-theme theme)
    (load-theme theme t)))

(global-set-key (kbd "<f5>") 'my/theme-toggle)

(setq doom-theme my-current-theme)

;; Remove highlighting on some major programming faces. I think it makes
;; the buffer too busy and difficult to read.
(custom-set-faces!
  '((font-lock-function-name-face
     font-lock-variable-name-face
     font-lock-constant-face
     font-lock-builtin-face
     font-lock-type-face)
    :foreground unspecified :weight normal))

(defun my--dashboard-message ()
  (insert (concat "MAIN BUFFER\n"
                  my-title-emacs-version
                  " on " my-system-distro " (" my-system-os ")\n"
                  "Built for: " system-configuration)))

;; Dashboard displayed when starting Emacs. As a personal preference, I like to
;; keep it very simple. It is ligther than the default scratch buffer in many
;; cases. But it can also not be killed, hence remembers the working directory
;; of the last open buffer, `find-file' will work from the directory I expect.
(setq +doom-dashboard-functions '(my--dashboard-message))

(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    ;; Do not display the cursor on the dashboard.
    (setq-local evil-normal-state-cursor (list nil))))

(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)

(setq display-line-numbers-type nil
      scroll-margin 7
      ;; Makes underlines render a bit cleaner.
      x-underline-at-descent-line t
      ;; No confirmation can be annoying as I realised it often happens by
      ;; mistake.
      confirm-kill-emacs 'yes-or-no-p)

;; Smooth scrolling
(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

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
        "C-;"   #'my/scroll-up
        "C-l"   #'my/scroll-down
        "S-C-h" #'my/enlarge-window-horizontally
        "S-C-l" #'my/shrink-window-horizontally
        "S-C-k" #'my/enlarge-window
        "S-C-j" #'my/shrink-window

        ;; graphite
        "S-C-y" #'my/enlarge-window-horizontally
        "S-C-n" #'my/shrink-window-horizontally
        "S-C-a" #'my/enlarge-window
        "S-C-e" #'my/shrink-window

        "M-SPC" #'cycle-spacing
        "M-o"   #'delete-blank-lines
        ";d"    #'my/save-and-close-buffer
        ";w"    #'save-buffer
        "C-k"   #'join-line
        "B"     #'beginning-of-line-text
        "E"     #'end-of-line
        "M-<delete>" #'kill-word
        "C-e"   #'join-line
        "C-n"   #'electric-newline-and-maybe-indent
        "C-a"   #'join-line  ; graphite
        "C-n"   #'electric-newline-and-maybe-indent  ; graphite
        "S-C-m" #'my/enlarge-window-horizontally
        "S-C-n" #'my/shrink-window-horizontally
        "S-C-e" #'my/enlarge-window
        "S-C-i" #'my/shrink-window
        :leader
        ;; graphite layout
        "wy" #'evil-window-left
        "wn" #'evil-window-down
        "wa" #'evil-window-up
        "we" #'evil-window-right
        ;; easier access
        "ly" #'evil-window-left
        "ln" #'evil-window-down
        "la" #'evil-window-up
        "le" #'evil-window-right
        "ls" #'evil-window-split
        "lv" #'evil-window-vsplit)

  ;; Change the cursor color depending on the evil mode
  (setq evil-default-state-cursor  '(box "cyan3")
        evil-normal-state-cursor   '(box "cyan3")
        evil-insert-state-cursor   '(box "green3")
        evil-visual-state-cursor   '(box "OrangeRed2")
        evil-replace-state-cursor  '(box "red4")
        evil-operator-state-cursor '(box "red4")))

;; Evil visual hints when yanking, pasting, deleting etc.
;; doc: https://github.com/edkolev/evil-goggles
(after! evil-goggles
  (setq evil-goggles-duration 0.15)
  (evil-goggles-use-diff-refine-faces))

;; Evil escape
;; doc: https://github.com/syl20bnr/evil-escape
(after! evil-escape
  ;; Do not activate evil-escape through the 'jk' escape sequence in the
  ;; following modes. It might be because 'j' and 'k' allow scrolling up or
  ;; down even in insert-mode, or it is just breaking the expected mode
  ;; behaviour.
  (setq evil-escape-excluded-major-modes '(treemacs-mode)))

;; Evil snipe
;; doc: https://github.com/hlissner/evil-snipe
(after! evil-snipe
  (map! :map evil-snipe-parent-transient-map
        :g "j" #'evil-snipe-repeat
        :g "k" #'evil-snipe-repeat-reverse
        ;; graphite layout support
        :g "h" #'evil-snipe-repeat
        :g "a" #'evil-snipe-repeat-reverse))

;; Icons
;; doc: https://github.com/domtronn/all-the-icons.el
(after! all-the-icons
  ;; Keep icons small.
  (if (featurep :system 'macos)
      (setq all-the-icons-scale-factor 0)
    (setq all-the-icons-scale-factor 0.8))
  (setq all-the-icons-default-adjust 0))

;; Scrolling
(when (boundp 'mac-mouse-wheel-smooth-scroll)
  (setq mac-mouse-wheel-smooth-scroll t))
(when (> emacs-major-version 28)
  (pixel-scroll-precision-mode))

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
  ;; These bindings are hard to work with as I'm running evil mode. I don't
  ;; want the 'h' or the 'l' key to be bound to anything as I'm expected those
  ;; keys to allow me to move the cursor to the left and right.
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil)

  (after! git-commit
    ;; Try to keep summary short for ease of readability as of the original Git
    ;; convention. The rest can go into the body.
    (setq git-commit-summary-max-length 50))

  ;; Remap keys to move commits up or down when using interactive rebase.
  (after! git-rebase
    (define-key git-rebase-mode-map "K" 'git-rebase-move-line-up)
    (define-key git-rebase-mode-map "J" 'git-rebase-move-line-down)
    ;; graphite layout support
    (define-key git-rebase-mode-map "N" 'git-rebase-move-line-up)
    (define-key git-rebase-mode-map "A" 'git-rebase-move-line-down)))

;; git-timemachine
;; doc: https://github.com/emacsmirror/git-timemachine
(after! git-timemachine
  ;; add graphite layout support
  (map! :map git-timemachine-mode-map
        :n "C-n" #'git-timemachine-show-previous-revision
        :n "C-a" #'git-timemachine-show-next-revision))

;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2)
  ;; Make evil commands less verbose, use small arrow symbols instead.
  ;; Stolen from:
  ;; https://tecosaur.github.io/emacs-config/config.html#which-key,code--2
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

;; add confirmation message after calling `save-buffer'
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
        projectile-project-search-path '("~/dotfiles/" "~/code/" "~/work/")
        projectile-globally-ignored-files '(".DS_Store" "TAGS" "*.pyc")
        projectile-globally-ignored-directories
        '(".npm" ".poetry" "GoogleDrive" ".mypy_cache"
          "Library" ".git" "__pycache__" "node_modules"
          ".idea" ".vscode" ".svn" ".tox" ".cache"))
  (setq projectile-ignored-projects '("~/" "/tmp" "~/Downloads" "~/backups"))
  (when (featurep :system 'macos)
    (pushnew! projectile-ignored-projects
              "/Applications" "/Volumes/GoogleDrive"))

  ;; Make the projectile command use fd with some more sensitive defaults, as I
  ;; noticed some performance issues with the one used by Doom or projectile
  ;; natively.
  (let ((excludes (mapcar (lambda (val) (format "-E '%s'" val))
                          (append projectile-globally-ignored-files
                                  projectile-globally-ignored-directories))))
    (setq projectile-generic-command
          (cons "fd . -0 -H -c never -t file -t symlink --strip-cwd-prefix"
                excludes))))


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
                (intern (format"dired-subtree-depth-%d-face" i)))
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
        corfu-preview-current nil))

;; vertical interactive completion
;; doc: https://github.com/minad/vertico
(after! vertico
  (setq vertico-count 15)
  (map! (:leader
         (:prefix "s"
          ;; Search a symbol at point using Vertico
          :desc "Search project (at point)" "w"
          #'my/vertico-search-project-symbol-at-point
          ;; Repeat the last Vertico search. Doom also allow this with <SPC '>
          ;; but I find it easier to remember memo-technically with <SPC s .>
          :desc "Repeat last search" "." #'vertico-repeat))))


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
  (setq lsp-restart 'interactive)
  ;; Iterate quickly (default is 10).
  (setq lsp-response-timeout 5)

  (map! :map lsp-mode-map
        :leader
        :prefix "r"
        :desc "Restart LSP workspace" "w" #'lsp-restart-workspace))

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
  ;; Add support for Ruff Python linter.
  (defvar my-ruff-global-config "~/.config/ruff.toml"
    "Path of global Ruff configuration file.")

  (flycheck-define-checker my-python-ruff
    "A Python syntax and style checker using the ruff utility."
    :command ("ruff"
              "check"
              "--output-format=text"
              (eval (format "--config=%s" my-ruff-global-config))
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)

  (add-to-list 'flycheck-checkers 'my-python-ruff)

  (map! :map flycheck-mode-map
        :leader
        :localleader
        :desc "Flycheck list errors" "l" #'flycheck-list-errors))

;; Flycheck pop-up tooltips
;; doc: https://github.com/flycheck/flycheck-popup-tip
(after! flycheck-popup-tip
  (setq flycheck-popup-tip-error-prefix "(!) "))

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

(setq-hook! 'python-mode-hook
  flycheck-checker 'my-python-ruff
  flycheck-python-mypy-config "~/.config/mypy/config")

;; PET (P ython E xecutable T racker)
;; doc: https://github.com/wyuenho/emacs-pet/
(use-package! pet
  :config
  (add-hook 'python-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local lsp-ruff-lsp-python-path python-shell-interpreter))))

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
(defun my--web-mode-configs ()
  (setq-local tab-width 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-script-padding 2
              web-mode-style-padding 2))

(add-hook! 'web-mode-hook #'my--web-mode-configs)

;; Disable formatters for html and web modes
(setq-hook! '(html-mode-hook web-mode-hook)
  +format-with :none)

;; SQL
(use-package! sql
  :mode ("\\.\\(m\\|my\\)?sql\\'" . sql-mode)
  :custom
  ;; I use this for local development only. Disable SSL mode by default to ease
  ;; connectivity using localhost.
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
  :config
  (defun my/remap-yaml-faces ()
    (face-remap-add-relative 'font-lock-variable-name-face
                             :inherit font-lock-keyword-face))

  ;; The above seems to leave `buffer-face-mode' on, disable it.
  (defadvice my/remap-yaml-faces (after my/deactivate-buffer-faces activate)
    (buffer-face-mode -1))

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

;; Lua
(add-to-list 'auto-mode-alist '("conky\\.conf\\'" . lua-mode))

;; ssh config mode
(add-to-list 'auto-mode-alist
             '("/\\.ssh/\\(?:work\\|private\\)\\'" . ssh-config-mode))

;; ;; tree-sitter
;; (after! tree-sitter-langs
;;   (defvar my-tree-sitter-nodes-ignore
;;     '("property" "operator" "method.call" "function.call"
;;       "function.method.call" "function.special" "label")
;;     "List of tree-sitter nodes to ignore.")

;;   ;; deactivate highliting on some specific programming nodes, as I find this
;;   ;; makes the buffer too busy and difficult to read.
;;   (add-function
;;    :before-while tree-sitter-hl-face-mapping-function
;;    (lambda (capture-name)
;;      (not (member capture-name my-tree-sitter-nodes-ignore))))

;;   ;; ensure Python docstring apostrophes are rendered in the same syntax as
;;   ;; strings.
;;   (tree-sitter-hl-add-patterns 'python
;;     [((string) @doc
;;       (.match? @doc "^(\"\"\"|r\"\"\")"))]))


;; HACK: since some upstream changes, formatting a specific region seems
;; broken, and calling `+format/region' raises: "with Symbol’s function
;; definition is void: apheleia--get-formatters", ensure to autoload the
;; required function.
(use-package! apheleia
  :commands (apheleia--get-formatters))

;; HACK: re the above hack, for some reason it also seems to break the
;; tree-sitter syntax highlighting, we add a wrapper to re-enable tree-sitter
;; after calling `+format/region', in case the highlighting was broken.
(defadvice +format/region (after my/format-region activate)
  (ignore-errors
    (tree-sitter--teardown)
    (turn-on-tree-sitter-mode)))


;;
;;; Terminals

;; vterm
;; doc: https://github.com/akermu/emacs-libvterm
(after! vterm
  (setq vterm-max-scrollback 6000
        vterm-timer-delay 0.01)

  ;; load bash when using ssh over trmap
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))

  ;; Improve rendering of colors.
  (setq vterm-term-environment-variable "eterm-color")
  (add-hook! 'vterm-mode-hook #'eterm-256color-mode)

  (map! :map vterm-mode-map
        :n "B" #'vterm-beginning-of-line  ; beg of command
        :n "<return>" #'evil-insert-resume
        [remap delete-forward-char] #'vterm-send-delete
        :in "<M-backspace>" #'vterm-send-meta-backspace
        :n "<M-backspace>"  #'vterm-send-meta-backspace
        :in "C-k" #'vterm-send-up
        :in "C-j" #'vterm-send-down))

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

  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))

  ;; Use a custom command to fetch for new emails with mbsync
  (setq +notmuch-sync-backend "mbsync -a && notmuch new")

  (setq my-user-mail-address-2 (my--get-email "sws"))

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

  ;; Rename some feeds titles.
  (defadvice elfeed-search-update
      (before configure-elfeed-search-update activate)
    (let ((github-feed (elfeed-db-get-feed my-github-rss-feed))
          (git-doom-feed
           (elfeed-db-get-feed
            "https://github.com/doomemacs/doomemacs/commits/master.atom")))
      (setf (elfeed-feed-title github-feed) "Github feed")
      (setf (elfeed-feed-title git-doom-feed) "Doom Emacs commits"))))

;; debug mode
(defun my--echo-command-name-hook ()
  "Echo live command names."
  (unless (or (eq this-command 'self-insert-command)
              (eq this-command 'next-line))
    (message "%s" this-command)))

(define-minor-mode my-debug-mode
  "Custom debug mode.")

(add-hook! 'post-command-hook
  (if (bound-and-true-p my-debug-mode)
      (my--echo-command-name-hook)))


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
   :desc "Browse URL at point" "l" #'browse-url-at-point)

  (:prefix ("e" . "edit")
   :desc "Yank from killring" "p" #'yank-from-kill-ring)

  (:prefix "t"
   :desc "Truncate lines" "t" #'toggle-truncate-lines
   :desc "Imenu"          "i" #'imenu-list-smart-toggle)

  (:prefix "p"
   :desc "Run Makefile target" "m" #'+make/run)

  (:prefix ("P" . "password")
   :desc "Open password-store buffer" "p" #'pass)))
