;; -*- no-byte-compile: t; -*-
;;; smallwat3r/tools/packages.el

;; Input random text
;; https://github.com/jschaf/emacs-lorem-ipsum/commits/master
(package! lorem-ipsum :pin "4e87a899868e908a7a9e1812831d76c8d072f885")

;; Fetch the clipboard history
;; https://github.com/redguardtoo/cliphist/commits/master
(package! cliphist :pin "d02b97a2aa0da13711d9a6f845649115de8ac11b")

;; Bitwarden support
;; https://github.com/seanfarley/emacs-bitwarden/commits/main
(package! bitwarden :pin "50c0078d356e0ac0bcaf26b40113700ba4123ec3"
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))

;; Copy active region to the kill ring formatted as Github, Slack etc...
;; https://github.com/sshaw/copy-as-format/commits/master
(package! copy-as-format :pin "b9f6f725ca9701c5a02bfb479573fdfcce2e1e30")

;; Imenu list of buffers
;; https://github.com/bmag/imenu-list/commits/master
(package! imenu-list :pin "76f2335ee6f2f066d87fe4e4729219d70c9bc70d")
