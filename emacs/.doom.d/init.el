;;; init.el -*- lexical-binding: t; -*-

(use-package-hook! lsp-pyright
  :pre-init
  ;; Enforce lsp-pyright to use one session per project. This needs to be set-up
  ;; before initialising lsp-pyright to work.
  (setq lsp-pyright-multi-root nil))

(doom! :smallwat3r
       containerization
       deft
       everywhere
       google
       hl-todo
       modeline
       scratch
       slack
       tree-sitter

       :completion
       company
       (vertico +icons)

       :ui
       doom-dashboard
       (emoji +unicode)
       ophints
       (popup +defaults)
       treemacs
       vc-gutter
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       snippets

       :emacs
       (dired +icons)
       electric
       undo
       vc

       :term
       vterm

       :checkers
       (spell +aspell)
       syntax

       :tools
       (debugger +lsp)
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets)
       lsp
       (magit +forge)
       make
       pass
       pdf
       rgb

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       (go +lsp)
       (javascript +lsp)
       json
       lua
       markdown
       (org +journal +pandoc)
       (python +lsp +pyright)
       rest
       (sh +lsp)
       (web +lsp)

       :email
       notmuch

       :app
       rss

       :config
       (default +bindings +smartparens))
