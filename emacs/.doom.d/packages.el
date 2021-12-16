;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(disable-packages!
 lsp-ui
 lsp-python-ms
 company-anaconda
 anaconda-mode
 pipenv
 solaire-mode)

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitconfig-mode.el")))
(package! gitignore-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitignore-mode.el")))

(package! dired-narrow)
(package! dired-subtree)
(package! scratch)
(package! shrink-path)
(package! org-appear)
(package! lorem-ipsum)
(package! applescript-mode)
(package! nginx-mode)
(package! vimrc-mode)
(package! tubestatus)
(package! untappd)
