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
 ;; Disabled because my keyboard has a dedicated Escape key, and
 ;; evil-escape adds input latency while waiting for key sequences.
 evil-escape)

;;
;;; Personal packages (not pinned as managed by myself)

;; Custom themes
(package! creamy-theme
  :recipe (:host github :repo "smallwat3r/emacs-creamy-theme"))

(package! simplicity-theme
  :recipe (:host github :repo "smallwat3r/emacs-simplicity-theme"))

