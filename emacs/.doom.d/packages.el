;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Disable some of Doom's default packages, as not used or obsolete.
(disable-packages!
 anaconda-mode
 company-anaconda
 lsp-python-ms
 lsp-ui
 pipenv
 solaire-mode)

(package! applescript-mode)
(package! dired-narrow)
(package! dired-subtree)
(package! keycast)
(package! lorem-ipsum)
(package! nginx-mode)
(package! org-appear)
(package! scratch)
(package! shrink-path)
(package! simplicity-theme :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))
(package! symbol-overlay)
(package! tubestatus :recipe (:host github :repo "smallwat3r/tubestatus.el"))
(package! untappd :recipe (:host github :repo "smallwat3r/untappd.el"))
(package! vimrc-mode)
