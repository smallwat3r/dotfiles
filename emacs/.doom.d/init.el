;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       containerization
       deft
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
       (org +journal)
       (python +lsp +pyright)
       rest
       (sh +lsp)
       (web +lsp)

       :email
       notmuch

       :app
       everywhere
       rss

       :config
       (default +bindings +smartparens))
