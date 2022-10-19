;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;
;;; Disabled packages

;; These are some of Doom's default packages, that I don't use.
(disable-packages!
 ;; Python packages
 anaconda-mode
 company-anaconda
 lsp-python-ms  ; prefer lsp-pyright
 pipenv         ; prefer poetry
 nose           ; prefer pytest
 ;; UI packages
 elfeed-goodies
 flycheck-popup-tip
 lsp-ui
 solaire-mode
 ns-auto-titlebar)

;;
;;; Additional packages.

;; Melpa
(package! applescript-mode :pin "9b84a7cb74d687745df37ba15113933fc6256274")
(package! logview :pin "6446ccf25f834519d7f5c71e5293b062d599b355")
(package! nginx-mode :pin "6e9d96f58eddd69f62f7fd443d9b9753e16e0e96")
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
(package! yaml-mode :pin "535273d5a1eb76999d20afbcf4d9f056d8ffd2da")
(package! dired-narrow :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
(package! dired-subtree :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
(package! keycast :pin "296fba536287e7a0d88109e75a0bc0181647dc5e")
(package! lorem-ipsum :pin "da75c155da327c7a7aedb80f5cfe409984787049")
(package! symbol-overlay :pin "c439b73a5f9713bb3dce98986b589bb901e22130")
(package! cliphist :pin "d02b97a2aa0da13711d9a6f845649115de8ac11b")
(package! golden-ratio :pin "007911d8a431b72670f5fe5f0e5b4380c2777a31")
(package! org-bullets :pin "b70ac2ec805bcb626a6e39ea696354577c681b36")
(package! shrink-path :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! lsp-pyright :pin "c745228f39fdb35372b29b909f25fa6c98dc7c88")

;; Github
(package! bitwarden :pin "b536543d785d38d4d33ff7ea44ab435f0e12a20f"
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))

;; Personal (I'm ok with not pinning those as I manage them)
(package! simplicity-theme
  :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))
(package! tubestatus
  :recipe (:host github :repo "smallwat3r/tubestatus.el"))
(package! untappd
  :recipe (:host github :repo "smallwat3r/untappd.el"))
