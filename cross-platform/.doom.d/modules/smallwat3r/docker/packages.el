;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "91233a7c559d87c47de6193a64913732756f0799")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "8135740bfc6ad96ab82d39d9fe68dbce56180f4c")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "5cb580d0e1d18a97ec4d0ba33b374a0822a96d4f")
(package! kubernetes-evil :pin "5cb580d0e1d18a97ec4d0ba33b374a0822a96d4f")
