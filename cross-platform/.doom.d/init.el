;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       containerization
       deft
       everywhere
       google
       scratch
       slack
       tree-sitter

       :completion
       company
       (vertico +icons)

       :ui
       doom-dashboard
       (emoji +unicode)
       hl-todo
       modeline
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
       (terraform +lsp)

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
       (python +lsp)
       rest
       (sh +lsp)
       (web +lsp)

       :email
       notmuch

       :app
       rss

       :config
       (default +bindings +smartparens))
