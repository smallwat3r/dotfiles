;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       deft
       google
       hl-todo
       kubernetes
       modeline
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
       eshell
       vterm

       :checkers
       (spell +aspell)
       syntax

       :tools
       (debugger +lsp)
       docker
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
       (json)
       lua
       (markdown +grip)
       (org +journal)
       (python +lsp +pyright)
       rest
       (sh +lsp)
       (web +lsp)
       yaml

       :email
       notmuch

       :app
       everywhere

       :config
       (default +bindings +smartparens))
