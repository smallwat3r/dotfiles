;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Disable some of Doom's default packages, as not used or obsolete.
(disable-packages!
 anaconda-mode
 company-anaconda
 nose
 lsp-python-ms
 lsp-ui
 pipenv
 solaire-mode)

(package! applescript-mode :pin "9b84a7cb74d687745df37ba15113933fc6256274")
(package! dired-narrow :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
(package! dired-subtree :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
(package! keycast :pin "296fba536287e7a0d88109e75a0bc0181647dc5e")
(package! lorem-ipsum :pin "da75c155da327c7a7aedb80f5cfe409984787049")
(package! nginx-mode :pin "6e9d96f58eddd69f62f7fd443d9b9753e16e0e96")
(package! org-appear :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac")
(package! scratch :pin "f000648c9663833a76a8de9b1e78c99a9d698e48")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! symbol-overlay :pin "c439b73a5f9713bb3dce98986b589bb901e22130")
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! alert :pin "7046393272686c7a1a9b3e7f7b1d825d2e5250a6")
(package! yaml-mode :pin "535273d5a1eb76999d20afbcf4d9f056d8ffd2da")

;; I'm ok with not pinning those as they are managed by me.
(package! simplicity-theme :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))
(package! tubestatus :recipe (:host github :repo "smallwat3r/tubestatus.el"))
(package! untappd :recipe (:host github :repo "smallwat3r/untappd.el"))
