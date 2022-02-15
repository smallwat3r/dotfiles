;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(disable-packages!
 lsp-ui
 lsp-python-ms
 company-anaconda
 anaconda-mode
 pipenv
 solaire-mode)

(package! dired-narrow)
(package! dired-subtree)
(package! scratch)
(package! shrink-path)
(package! org-appear)
(package! lorem-ipsum)
(package! applescript-mode)
(package! nginx-mode)
(package! vimrc-mode)
(package! keycast)

(package! tubestatus :recipe
  (:host github :repo "smallwat3r/tubestatus.el"))

(package! untappd :recipe
  (:host github :repo "smallwat3r/untappd.el"))

(package! simplicity-theme :recipe
  (:host github :repo "smallwat3r/emacs-simplicity-theme"))
