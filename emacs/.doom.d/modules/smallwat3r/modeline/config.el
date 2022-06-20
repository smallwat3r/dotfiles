;;; smallwat3r/modeline/config.el -*- lexical-binding: t; -*-

;; Show counter while in search modes
;; doc: https://github.com/emacsorphanage/anzu
;; doc: https://github.com/emacsorphanage/evil-anzu
(use-package! anzu
  :delight
  :after-call isearch-mode)

(use-package! evil-anzu
  :delight
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

;; Manage how modes are displayed
;; doc: https://www.emacswiki.org/emacs/DelightedModes
(use-package! delight
  :config
  (delight
   '((emacs-lisp-mode "ξ" :major)
     (js2-mode "Js" :major)
     (yas-minor-mode " Y" yasnippet)

     ;; hidden minor-modes from modeline
     (editorconfig-mode nil editorconfig)
     (mini-modeline-mode nil mini-modeline)
     (company-mode nil company)
     (git-gutter-mode nil git-gutter)
     (highlight-indent-guides-mode nil highlight-indent-guides)
     (abbrev-mode nil abbrev)
     (better-jumper-mode nil better-jumper)
     (better-jumper-local-mode nil better-jumper)
     (ivy-mode nil ivy)
     (evil-org-mode nil evil-org)
     (which-key-mode nil which-key)
     (gcmh-mode nil gcmh)
     (ws-butler-mode nil ws-butler)
     (eldoc-mode nil eldoc)
     (beacon-mode nil beacon)
     (dtrt-indent-mode nil dtrt-indent)
     (evil-escape-mode nil evil-escape)
     (evil-traces-mode nil evil-traces)
     (org-indent-mode nil org-indent)
     (outline-minor-mode nil outline)
     (persp-mode nil persp-mode)
     (whitespace-mode nil whitespace)
     (smartparens-mode nil smartparens)
     (evil-goggles-mode nil evil-goggles)
     (evil-snipe-local-mode nil evil-snipe))))

;;
;;; Set default modeline format

(defface my-git-branch-face
  '((t :background unspecified :foreground unspecified))
  "The face used to display the current git branch in mode-line.")

(defvar my-modeline-format
  '(("%e"
     evil-mode-line-tag
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-buffer-identification
     (vc-mode (:eval (propertize vc-mode 'face 'my-git-branch-face)))
     "   %p %l,%c   "
     mode-line-misc-info
     mode-line-modes))
  "Custom modeline format Alist.")

(setq-default mode-line-format my-modeline-format)
