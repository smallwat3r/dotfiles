;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! key-chord
  :recipe (:host github :repo "emacsorphanage/key-chord"))

(package! exec-path-from-shell
  :recipe (:host github :repo "purcell/exec-path-from-shell"))

(package! telephone-line
  :recipe (:host github :repo "dbordak/telephone-line"))

(package! emacs-language-id
  :recipe (:host github :repo "lassik/emacs-language-id"))

(package! emacs-format-all-the-code
  :recipe (:host github :repo "lassik/emacs-format-all-the-code"))
