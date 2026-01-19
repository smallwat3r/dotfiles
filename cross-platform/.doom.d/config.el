;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; General

(defconst my-user-alias "smallwat3r"
  "User alias.")

(defvar my-email-addresses-alist
  `(("gmail" . "mpetiteau.pro@gmail.com")
    (,my-user-alias . "matt@smallwat3r.com"))
  "Alist of my email addresses.")

(defun my/get-email (name)
  "Helper function to get email address by NAME."
  (cdr (assoc name my-email-addresses-alist)))

(setq user-full-name "Matthieu Petiteau"
      user-mail-address (my/get-email "gmail"))

(setq default-directory "~/")

(defvar my-dotfiles-dir (concat default-directory "dotfiles")
  "Directory containing my dotfiles.")

(defvar my-notes-directory "~/org"
  "Where I'm storing my notes.")

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
        (string-trim (with-temp-buffer
                       (insert-file-contents board-vendor-file)
                       (buffer-string)))
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
  ;; ns stands for NeXTSTEP, the interface used on macOS.
  (setq ns-use-thin-smoothing t
        ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        ns-antialias-text t))


;;
;;; Editor

;; Fonts

(defun my/get-font-size-based-on-os ()
  "Return a font size depending on the device or OS."
  (let ((os (downcase (or (doom-system-distro-version) ""))))
    (cond
     (IS-GPD 13)
     ((string-match-p "ubuntu" os) 22)
     ((string-match-p "fedora" os) 17)
     (t 16))))

(defun my/font-available-p (font)
  "Return non-nil if FONT is available on this system."
  (and (find-font (font-spec :name font)) t))

(defun my/safe-font (fonts &rest spec)
  "Return a font-spec using the first available font in FONTS."
  (let ((available
         (seq-find (lambda (f)
                     (if (my/font-available-p f) t
                       (message "Warning: font not found: %s" f) nil))
                   fonts)))
    (when available
      (apply #'font-spec :family available spec))))

;; pick font based on OS, with support for fallback
(defvar my--fonts-configured nil
  "Non-nil if fonts have been successfully configured.")

(defun my/configure-fonts (&optional frame)
  "Configure doom-font when a graphical display is available."
  (let ((frame (or frame (selected-frame))))
    (when (and (not my--fonts-configured)
               (display-graphic-p frame))
      (let ((size (my/get-font-size-based-on-os)))
        (setq doom-font
              (cond
               (IS-GPD
                (my/safe-font '("MonacoB" "Monospace") :size size))
               ((eq system-type 'gnu/linux)
                (my/safe-font '("Triplicate A Code" "MonacoB" "Monospace") :size size))
               ((eq system-type 'darwin)
                (my/safe-font '("Triplicate A Code" "Monaco") :size size))
               (t
                (my/safe-font '("Triplicate A Code") :size size))))
        (setq doom-variable-pitch-font doom-font)
        (when doom-font
          (doom/reload-font))
        (setq my--fonts-configured t)
        (remove-hook 'focus-in-hook #'my/configure-fonts)
        (remove-hook 'window-setup-hook #'my/configure-fonts)))))

;; Defer font configuration until the frame is ready
(if (daemonp)
    (add-hook 'focus-in-hook #'my/configure-fonts)
  (add-hook 'window-setup-hook #'my/configure-fonts))

;; Enable proportional fonts for text-mode buffers.
(add-hook! 'text-mode-hook 'variable-pitch-mode)

(setq doom-font-increment 1
      doom-big-font-increment 2)

(setq-default tab-width 8
              with-editor-emacsclient-executable "emacsclient")

(setq-default line-spacing 2)
(unless (zerop line-spacing)
  ;; Sliced images break with non-zero line-spacing because Emacs adds extra
  ;; pixels between each image slice, causing visible gaps.
  (setq +rss-enable-sliced-images nil))

;; Theme
(setq doom-theme 'creamy)
(custom-theme-set-faces! 'creamy
  '(font-lock-function-name-face :foreground "MidnightBlue"))


;; Dashboard displayed when starting Emacs. As a personal preference, I like to
;; keep it very simple. It is lighter than the default scratch buffer in many
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

;; smart parens pairs
;; doc: https://github.com/Fuco1/smartparens
(after! smartparens-config
  ;; add backtick pairing on general markdown modes
  (dolist (mode '(markdown-mode gfm-mode markdown-ts-mode))
    (sp-local-pair mode "`" "`" :actions '(insert wrap navigate)))
  ;; re-define quote pairs to disable autoskip
  (dolist (p '(("\"" . "\"") ("'" . "'")))
    (sp-pair (car p) (cdr p) :actions '(insert wrap navigate))))

;; quick note editing within Emacs
;; doc: https://github.com/jrblevin/deft/
(after! deft
  (setq deft-recursive t
        deft-directory my-notes-directory
        deft-use-filter-string-for-filename t
        deft-extensions '("org" "md" "txt")
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))
        deft-use-filename-as-title t))


;; Icons
;; doc: https://github.com/domtronn/all-the-icons.el
(after! all-the-icons
  ;; Keep icons small.
  (if (featurep :system 'macos)
      (setq all-the-icons-scale-factor 0)
    (setq all-the-icons-scale-factor 0.8))
  (setq all-the-icons-default-adjust 0))


;; Show keybindings in a pop-up
;; doc: https://github.com/justbur/emacs-which-key
(after! which-key
  (setq which-key-idle-delay 0.2))


(defvar my-global-window-divider-width 2
  "Default global width size (in pixels) of a window divider.")

;; Set window dividers widths.
(setq window-divider-default-right-width my-global-window-divider-width
      window-divider-default-bottom-width my-global-window-divider-width)

;; highlight URLs and emails in current buffer
(use-package! goto-addr
  :init
  (defface my-goto-address-mail-face '((t :italic nil :underline t))
    "Face for email address."
    :group 'basic-faces)
  :hook
  (prog-mode . goto-address-prog-mode)
  ((text-mode vterm-mode restclient-mode compilation-mode) . goto-address-mode)
  :custom
  (goto-address-mail-regexp "\\w+\\(\\.\\w+\\)?\\(\\+\\w+\\)?@\\(\\w\\|\\.\\)+\\.\\w+")
  (goto-address-mail-face 'my-goto-address-mail-face))


;; Zen mode. Implements a distraction free writing mode.
;; doc: https://github.com/joostkremers/writeroom-mode
(after! writeroom-mode
  (setq +zen-window-divider-size my-global-window-divider-width
        +zen-text-scale 0))

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

;;
;;; Custom templates

(setq +file-templates-dir (expand-file-name "templates" doom-user-dir)
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
        projectile-project-search-path '("~/dotfiles" "~/code" "~/work"))

  ;; ROS (Robot Operating System) workspaces follow a naming convention of
  ;; `*_ws` or `ws_*` (e.g. `catkin_ws`, `ws_ros2`). Automatically add any
  ;; matching directories in home to the project search path.
  (when (featurep :system 'linux)
    (dolist (file (directory-files "~/" t "^[^.].*"))
      (when (file-directory-p file)
        (let ((name (file-name-nondirectory (directory-file-name file))))
          (when (or (string-suffix-p "_ws" name)
                    (string-prefix-p "ws_" name))
            (add-to-list 'projectile-project-search-path file))))))

  (pushnew! projectile-globally-ignored-directories
            ".npm" ".poetry" "GoogleDrive" ".mypy_cache"
            "Library" "__pycache__")
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
  (setq vertico-count 15))


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
(remove-hook! (text-mode) #'spell-fu-mode)

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

;; Yaml, invert colors when using creamy, as most
;; of the colors are deactivated by default.
(add-hook! 'yaml-mode-hook
  (when (eq doom-theme 'creamy)
    (face-remap-add-relative
     'font-lock-variable-name-face
     '(:inherit font-lock-keyword-face))
    (buffer-face-mode -1)))

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


;;
;;; Keybindings

(load! "keybindings")
