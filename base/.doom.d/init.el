;;; init.el -*- lexical-binding: t; -*-
;;
;; Doom Emacs module configuration.
;; Modules are loaded in order. Personal modules are placed at the end so
;; their config runs after Doom's built-in modules, allowing overrides.

(doom! :completion
       (corfu +orderless +icons +dabbrev)
       (vertico +icons)

       :ui
       deft
       doom-dashboard
       (emoji +unicode)
       hl-todo
       ophints
       smooth-scroll
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
       tramp
       undo
       vc

       :checkers
       (spell +aspell)
       syntax

       :tools
       debugger
       (eval +overlay)
       ; llm
       (lookup +dictionary +docsets)
       (lsp +eglot +booster)
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
       (markdown +tree-sitter)
       (org +journal +pandoc)
       (python +lsp +tree-sitter)
       rest
       (rust +lsp +tree-sitter)
       (sh +lsp)
       (web +lsp +tree-sitter)
       (yaml +tree-sitter)

       :email
       notmuch

       :app
       rss

       ;;
       ;;; Personal modules (loaded last for override priority)

       :smallwat3r
       claude
       debug
       dired
       docker
       elfeed-ext
       evil-ext
       filetypes
       git-ext
       google
       highlighting
       mail-ext
       modeline
       org-ext
       programming-ext
       python-ext
       slack
       terminal
       tools

       :config
       (default +bindings +smartparens))
