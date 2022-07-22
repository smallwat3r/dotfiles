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
(package! symbol-overlay :pin "c439b73a5f9713bb3dce98986b589bb901e22130")
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! yaml-mode :pin "535273d5a1eb76999d20afbcf4d9f056d8ffd2da")
(package! cliphist :pin "d02b97a2aa0da13711d9a6f845649115de8ac11b")
(package! bitwarden :pin "b536543d785d38d4d33ff7ea44ab435f0e12a20f"
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))
(package! logview :pin "6446ccf25f834519d7f5c71e5293b062d599b355")
(package! golden-ratio :pin "007911d8a431b72670f5fe5f0e5b4380c2777a31")

;; I'm ok with not pinning those as they are managed by me.
(package! simplicity-theme :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))
(package! tubestatus :recipe (:host github :repo "smallwat3r/tubestatus.el"))
(package! untappd :recipe (:host github :repo "smallwat3r/untappd.el"))
