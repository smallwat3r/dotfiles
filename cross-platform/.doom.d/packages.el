;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;
;;; Disabled packages

;; A list of Doom's default packages, that I don't use.
(disable-packages!
 ;; Python packages
 lsp-python-ms  ; prefer lsp basedpyright
 pipenv         ; prefer poetry
 nose           ; prefer pytest
 ;; UI packages
 elfeed-goodies
 lsp-ui
 solaire-mode
 ;; others
 ;; disabled as escape sequences are configured physically on the
 ;; keyboard I'm using, and with it disable, we are not affecting
 ;; typing delays.
 evil-escape)

;;
;;; External packages

;; Support for Applescript
;; https://github.com/emacsorphanage/applescript-mode/commits/master
(package! applescript-mode :pin "67c68afc479581ecb6163371523435f244f5ec92")

;; Add syntax highlighting to log files
;; https://github.com/doublep/logview/commits/master
(package! logview :pin "649d878f7e2aad0f938b2cf0a870f1968b4d5e30")

;; Nginx config files syntax highlighting
;; https://github.com/ajc/nginx-mode/commits/master
(package! nginx-mode :pin "c4ac5de975d65c84893a130a470af32a48b0b66c")

;; Vim script syntax highlighting
;; https://github.com/mcandre/vimrc-mode/commits/master
(package! vimrc-mode :pin "f594392a0834193a1fe1522d007e1c8ce5b68e43")

;; Support for YAML
;; https://github.com/yoshiki/yaml-mode/commits/master
(package! yaml-mode :pin "d91f878729312a6beed77e6637c60497c5786efa")

;; Dired utilities
;; https://github.com/Fuco1/dired-hacks/commits/master
(package! dired-narrow :pin "de9336f4b47ef901799fe95315fa080fa6d77b48")
(package! dired-subtree :pin "de9336f4b47ef901799fe95315fa080fa6d77b48")

;; Display keycast
;; https://github.com/tarsius/keycast/commits/main
(package! keycast :pin "6570b73c4d726d18d6ee48a46494b6ff35aacea6")

;; Ability to add overlay over symbols in code
;; https://github.com/wolray/symbol-overlay/commits/master
(package! symbol-overlay :pin "6151f4279bd94b5960149596b202cdcb45cacec2")

;; Lsp for Pyright
;; https://github.com/emacs-lsp/lsp-pyright/commits/master
(package! lsp-pyright :pin "73377169beff8fe22cc6d52d65099db88bf49679")

;; Make org mode more beautiful
;; https://github.com/minad/org-modern/commits/main
(package! org-modern :pin "1723689710715da9134e62ae7e6d41891031813c")

;; SSH config file syntax highlighting
;; https://github.com/jhgorrell/ssh-config-mode-el/commits/master
(package! ssh-config-mode :pin "d0596f5fbeab3d2c3c30eb83527316403bc5b2f7")

;; GPG pinentry support
;; http://git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=shortlog;h=refs/heads/externals/pinentry
(package! pinentry :pin "99480adc192f657d7d9f2eb3ed4e568df3de8613")

;; PET (P ython E xecutable T racker)
;; https://github.com/wyuenho/emacs-pet/commits/main
(package! pet :pin "1abf16ebe1f4280fc2f875b4e99d8c26756d0f36")

;; Provides extra convenience functions for vterm
;; https://github.com/Sbozzolo/vterm-extra/commits/master/
(package! vterm-extra
  :recipe (:host github :repo "Sbozzolo/vterm-extra")
  :pin "2d4a34f03d0f0d863a319cd0496ed6cafe94c27e")

;; LSP booster
;; https://github.com/jdtsmith/eglot-booster/commits/main/
(package! emacs-lsp-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster")
  :pin "1260d2f7dd18619b42359aa3e1ba6871aa52fd26")

;; rainbow delimiters
;; https://github.com/Fanael/rainbow-delimiters/commits/master/
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")

;; highlight numbers
;; https://github.com/Fanael/highlight-numbers/commits/master/
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")

;;
;;; Personal packages (not pinned as managed by myself)

(package! creamy-theme
  :recipe (:host github :repo "smallwat3r/emacs-creamy-theme"))

;; Simple dark theme
(package! simplicity-theme
  :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))

;; Display London TfL tube status
(package! tubestatus
  :recipe (:host github :repo "smallwat3r/tubestatus.el"))

;; Untappd feed
(package! untappd
  :recipe (:host github :repo "smallwat3r/untappd.el"))
