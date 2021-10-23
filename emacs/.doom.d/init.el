;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       deft
       google
       kubernetes
       modeline
       slack
       tree-sitter

       :completion
       company
       vertico

       :ui
       doom-dashboard
       (emoji +unicode)
       hl-todo
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
       dired
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
       docker
       (eval +overlay)
       (lookup +dictionary +docsets)
       lsp
       magit
       make
       pass
       pdf
       rgb

       :os
       (:if IS-MAC macos)
       tty

       :lang
       cc
       emacs-lisp
       (go +lsp)
       (javascript +lsp)
       json
       (markdown +grip)
       (org +journal)
       (python +lsp +pyright +poetry)
       rest
       (sh +lsp)
       web
       yaml

       :email
       notmuch

       :config
       (default +bindings +smartparens))
