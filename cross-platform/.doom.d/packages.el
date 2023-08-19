;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;
;;; Disabled packages.

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
 lsp-ui
 solaire-mode
 ns-auto-titlebar)


;;
;;; Misc packages.

;; Support for Applescript
;; https://github.com/emacsorphanage/applescript-mode/commits/master
(package! applescript-mode :pin "5b7a9195ffc25ed58f2c2437dd9c742241734904")

;; Add syntax highlighting to log files
;; https://github.com/doublep/logview/commits/master
(package! logview :pin "a185254605d212881648ceaf6d1504d34413ef8c")

;; Nginx config files syntax highlighting
;; https://github.com/ajc/nginx-mode/commits/master
(package! nginx-mode :pin "6e9d96f58eddd69f62f7fd443d9b9753e16e0e96")

;; Vim script syntax highlighting
;; https://github.com/mcandre/vimrc-mode/commits/master
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")

;; Support for YAML
;; https://github.com/yoshiki/yaml-mode/commits/master
(package! yaml-mode :pin "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7")

;; Dired utilities
;; https://github.com/Fuco1/dired-hacks/commits/master
(package! dired-narrow :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
(package! dired-subtree :pin "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")

;; Display keycast
;; https://github.com/tarsius/keycast/commits/master
(package! keycast :pin "30f2b446c013dda490f44dbf82d0ab34eb016726")

;; Input random text
;; https://github.com/jschaf/emacs-lorem-ipsum/commits/master
(package! lorem-ipsum :pin "da75c155da327c7a7aedb80f5cfe409984787049")

;; Ability to add overlay over symbols in code
;; https://github.com/wolray/symbol-overlay/commits/master
(package! symbol-overlay :pin "7c7d369bb749d96346b40cf714884a39aaa743f4")

;; Fetch the clipboard history
;; https://github.com/redguardtoo/cliphist/commits/master
(package! cliphist :pin "d02b97a2aa0da13711d9a6f845649115de8ac11b")

;; Lsp for Pyright
;; https://github.com/emacs-lsp/lsp-pyright/commits/master
(package! lsp-pyright :pin "54a2acddfdd7c3d31cb804a042305a3c6e60cf81")

;; Bitwarden support
;; https://github.com/seanfarley/emacs-bitwarden/commits/main
(package! bitwarden :pin "02d6410003a42e7fbeb4aa109aba949eea553706"
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))

;; Imenu list of buffers
;; https://github.com/bmag/imenu-list/commits/master
(package! imenu-list :pin "76f2335ee6f2f066d87fe4e4729219d70c9bc70d")

;; Make org mode more beautiful
;; https://github.com/minad/org-modern/commits/main
(package! org-modern :pin "067f9319d67b1170d46a394e039537bb9cb413e3")

;; SSH config file syntax highlighting
;; https://github.com/jhgorrell/ssh-config-mode-el/commits/master
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")

;; GPG pinentry support
;; http://git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=shortlog;h=refs/heads/externals/pinentry
(package! pinentry :pin "0f42e75676cb01bd49c70ccc65bce98a882bffed")

;; Improved color palette in terminal
;; https://github.com/dieggsy/eterm-256color/commits/master
(package! eterm-256color :pin "c9cfccef03e730f7ab2b407aada3df15ace1fe32")

;; Add sticky function headers
;; https://github.com/alphapapa/topsy.el/commits/master
(package! topsy :pin "86d4234e4a0e9d2f5bf0f1114ea9893da48e77d1")


;;
;;; Personal packages (I'm ok with not pinning those as I manage them).

;; Simple dark theme
(package! simplicity-theme
  :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))

;; Display London TfL tube status
(package! tubestatus
  :recipe (:host github :repo "smallwat3r/tubestatus.el"))

;; Untappd feed
(package! untappd
  :recipe (:host github :repo "smallwat3r/untappd.el"))


;;
;;; Experimental packages.
