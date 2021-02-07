;;; init.el -*- lexical-binding: t; -*-

(doom! :completion company
       (ivy +fuzzy +prescient)
       :ui deft doom-dashboard hl-todo ophints treemacs vc-gutter workspaces zen
       (emoji +unicode)
       (popup +defaults)
       :editor file-templates format snippets word-wrap
       (evil +everywhere)
       (fold +vimish-fold +evil-vimish-fold)
       :emacs dired electric undo vc
       :term eshell vterm
       :checkers syntax
       (spell +aspell)
       :tools docker lsp magit make pass pdf rgb
       (eval +overlay)
       (lookup +dictionary +docsets)
       :os tty
       (:if IS-MAC macos)
       :lang cc emacs-lisp json rest yaml
       (go +lsp)
       (javascript +lsp)
       (markdown +grip)
       (org +journal)
       (python +lsp)
       (sh +lsp)
       :email notmuch
       :config (default +bindings +smartparens))
