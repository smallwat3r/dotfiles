;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       deft
       google
       kubernetes
       modeline
       slack

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
       tree-sitter

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       (go +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +tree-sitter)
       lua
       (markdown +grip)
       (org +journal)
       (python +lsp +pyright +tree-sitter)
       rest
       (sh +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       yaml

       :email
       notmuch

       :app
       everywhere

       :config
       (default +bindings +smartparens))
