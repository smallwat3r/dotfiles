;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;
;;; Disabled packages

;; A list of Doom's default packages, that I don't use.
(disable-packages!
 ;; Python packages
 anaconda-mode
 company-anaconda
 lsp-python-ms  ; prefer lsp-pyright
 pipenv         ; prefer poetry
 nose           ; prefer pytest
 ;; UI packages
 elfeed-goodies
 lsp-ui
 solaire-mode
 ns-auto-titlebar)

;;
;;; External packages

;; Support for Applescript
;; https://github.com/emacsorphanage/applescript-mode/commits/master
(package! applescript-mode :pin "f7bbff161a6e5eb57f87b54064a8636c9d6acfb0")

;; Add syntax highlighting to log files
;; https://github.com/doublep/logview/commits/master
(package! logview :pin "272e5ccc13c3c058c73f9b89cd90b9832d976dc0")

;; Nginx config files syntax highlighting
;; https://github.com/ajc/nginx-mode/commits/master
(package! nginx-mode :pin "c4ac5de975d65c84893a130a470af32a48b0b66c")

;; Vim script syntax highlighting
;; https://github.com/mcandre/vimrc-mode/commits/master
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")

;; Support for YAML
;; https://github.com/yoshiki/yaml-mode/commits/master
(package! yaml-mode :pin "7b5ce294fb15c2c8926fa476d7218aa415550a2a")

;; Dired utilities
;; https://github.com/Fuco1/dired-hacks/commits/master
(package! dired-narrow :pin "e9e408e8571aee5574ca0a431ef15cac5a3585d4")
(package! dired-subtree :pin "e9e408e8571aee5574ca0a431ef15cac5a3585d4")

;; Display keycast
;; https://github.com/tarsius/keycast/commits/main
(package! keycast :pin "c44618d2867fc2410e5061fef2a805e974198cf2")

;; Ability to add overlay over symbols in code
;; https://github.com/wolray/symbol-overlay/commits/master
(package! symbol-overlay :pin "de215fff392c916ffab01950fcb6daf6fd18be4f")

;; Lsp for Pyright
;; https://github.com/emacs-lsp/lsp-pyright/commits/master
(package! lsp-pyright :pin "0c0d72aedd18b16f48379b7d2f9ecb9c068713b0")

;; Make org mode more beautiful
;; https://github.com/minad/org-modern/commits/main
(package! org-modern :pin "f619912e55b409a8a3ecb807a8c2a35faa0e482d")

;; SSH config file syntax highlighting
;; https://github.com/jhgorrell/ssh-config-mode-el/commits/master
(package! ssh-config-mode :pin "2d8e321c34a7535ae6dd0f6a1b0fd54e47aba612")

;; GPG pinentry support
;; http://git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=shortlog;h=refs/heads/externals/pinentry
(package! pinentry :pin "d3d96378945bdd6881db46848aecdb4d0f8ff55b")

;; Improved color palette in terminal
;; https://github.com/dieggsy/eterm-256color/commits/master
(package! eterm-256color :pin "c9cfccef03e730f7ab2b407aada3df15ace1fe32")

;; PET (P ython E xecutable T racker)
;; https://github.com/wyuenho/emacs-pet/commits/main
(package! pet :pin "e5e3f9f0326ea1cc6edef017f0ee34cc42754b08")

;; Provides extra convenience functions for vterm
;; https://github.com/Sbozzolo/vterm-extra/commits/master/
(package! vterm-extra
  :recipe (:host github :repo "Sbozzolo/vterm-extra")
  :pin "2d4a34f03d0f0d863a319cd0496ed6cafe94c27e")


;;
;;; Personal packages (not pinned as managed by myself)

;; Simple dark theme
(package! simplicity-theme
  :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))

;; Display London TfL tube status
(package! tubestatus
  :recipe (:host github :repo "smallwat3r/tubestatus.el"))

;; Untappd feed
(package! untappd
  :recipe (:host github :repo "smallwat3r/untappd.el"))
