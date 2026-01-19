;;; init.el -*- lexical-binding: t; -*-
;;
;; Doom Emacs module configuration.
;; Modules are loaded in order: personal modules first, then Doom's built-in
;; modules. Each module can have packages.el (dependencies) and config.el
;; (configuration).

;;
;;; Personal modules

(doom! :smallwat3r
       claude
       csv
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
       python-ext
       slack
       tools
       terminal

       ;;
       ;;; Doom modules

       :completion
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

       :term
       vterm

       :checkers
       (spell +aspell)
       syntax

       :tools
       (debugger +lsp)
       (eval +overlay)
       ; llm
       (lookup +dictionary +docsets)
       (lsp +eglot +booster)
       (magit +forge)
       make
       pass
       pdf
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
       (rust +lsp)
       (sh +lsp)
       (web +lsp)
       yaml

       :email
       notmuch

       :app
       rss

       :config
       (default +bindings +smartparens))
