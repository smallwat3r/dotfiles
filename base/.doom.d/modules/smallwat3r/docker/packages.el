;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "375e0ed45bb1edc655d9ae2943a09864bec1fcba")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "8135740bfc6ad96ab82d39d9fe68dbce56180f4c")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "c60a0d2a52f2e24adf75e8b6f017c1f8f2d926fd")
(package! kubernetes-evil :pin "c60a0d2a52f2e24adf75e8b6f017c1f8f2d926fd")
