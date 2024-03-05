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
(package! applescript-mode :pin "1e04ff20745fbf8aac9279aa4e144957c5e2b224")

;; Add syntax highlighting to log files
;; https://github.com/doublep/logview/commits/master
(package! logview :pin "9140067afdc2f0d1eb493dc4dfdb53645289dd2b")

;; Nginx config files syntax highlighting
;; https://github.com/ajc/nginx-mode/commits/master
(package! nginx-mode :pin "6e9d96f58eddd69f62f7fd443d9b9753e16e0e96")

;; Vim script syntax highlighting
;; https://github.com/mcandre/vimrc-mode/commits/master
(package! vimrc-mode :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")

;; Support for YAML
;; https://github.com/yoshiki/yaml-mode/commits/master
(package! yaml-mode :pin "23395dfcf004c952850691b70e63f97f169596e8")

;; Dired utilities
;; https://github.com/Fuco1/dired-hacks/commits/master
(package! dired-narrow :pin "874449d6fc98aee565e1715ec18acec3c1c2cafb")
(package! dired-subtree :pin "874449d6fc98aee565e1715ec18acec3c1c2cafb")

;; Display keycast
;; https://github.com/tarsius/keycast/commits/main
(package! keycast :pin "481a1620cac3118aff4b2db027acde7d3834b153")

;; Input random text
;; https://github.com/jschaf/emacs-lorem-ipsum/commits/master
(package! lorem-ipsum :pin "4e87a899868e908a7a9e1812831d76c8d072f885")

;; Ability to add overlay over symbols in code
;; https://github.com/wolray/symbol-overlay/commits/master
(package! symbol-overlay :pin "a783d7b5d8dee5ba9f5e7c00a834fbd6d645081b")

;; Fetch the clipboard history
;; https://github.com/redguardtoo/cliphist/commits/master
(package! cliphist :pin "d02b97a2aa0da13711d9a6f845649115de8ac11b")

;; Lsp for Pyright
;; https://github.com/emacs-lsp/lsp-pyright/commits/master
(package! lsp-pyright :pin "2f2631ae242d5770dbe6cb924e44c1ee5671789d")

;; Bitwarden support
;; https://github.com/seanfarley/emacs-bitwarden/commits/main
(package! bitwarden :pin "1120647d974e06af431523f5824eb2e868e8098b"
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

;; Support for CSV
;; https://git.savannah.gnu.org/cgit/emacs/elpa.git
(package! csv-mode :pin "1f7cf5ed71a67be73c9ca9e672b35e897017ea73")

;; PET (P ython E xecutable T racker)
;; https://github.com/wyuenho/emacs-pet/commits/main
(package! pet :pin "d431f4d4f262752f9e4498471b11b20eaa49b8cf")

;; Copy active region to the kill ring formatted as Github, Slack etc...
;; https://github.com/sshaw/copy-as-format/commits/master
(package! copy-as-format :pin "b9f6f725ca9701c5a02bfb479573fdfcce2e1e30")

;; Provides extra convenience functions for vterm
;; https://github.com/Sbozzolo/vterm-extra/commits/master/
(package! vterm-extra
  :recipe (:host github :repo "Sbozzolo/vterm-extra")
  :pin "2d4a34f03d0f0d863a319cd0496ed6cafe94c27e")

;; Modeline in minibuffer
;; https://github.com/manateelazycat/awesome-tray/commits/master/
(package! awesome-tray
          :recipe (:host github :repo "manateelazycat/awesome-tray")
          :pin "ce0fbeaa594e30aafdcaf6ee391202e06dad98bf")


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
