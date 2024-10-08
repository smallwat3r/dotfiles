;;; init.el -*- lexical-binding: t; -*-

(doom! :smallwat3r
       csv
       deft
       docker
       everywhere
       google
       modeline
       scratch
       slack
       tools

       :completion
       (corfu +orderless +icons +dabbrev)
       (vertico +icons)

       :ui
       doom-dashboard
       (emoji +unicode)
       hl-todo
       ophints
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
       (terraform +lsp)
       tree-sitter

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       (go +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +tree-sitter)
       (lua +tree-sitter)
       markdown
       (org +journal +pandoc)
       (python +lsp +tree-sitter)
       rest
       (rust +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (web +lsp +tree-sitter)

       :email
       notmuch

       :app
       rss

       :config
       (default +bindings +smartparens))
