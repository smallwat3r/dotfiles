; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! key-chord
  :recipe (:host github :repo "emacsorphanage/key-chord"))

(package! exec-path-from-shell
  :recipe (:host github :repo "purcell/exec-path-from-shell"))

(package! emacs-language-id
  :recipe (:host github :repo "lassik/emacs-language-id"))

(package! emacs-format-all-the-code
  :recipe (:host github :repo "lassik/emacs-format-all-the-code"))

(package! emacs-mini-modeline
  :recipe (:host github :repo "kiennq/emacs-mini-modeline"))

(package! py-isort)
(package! flycheck-mypy)

(package! org-bullet
  :recipe (:host github :repo "sabof/org-bullets"))
