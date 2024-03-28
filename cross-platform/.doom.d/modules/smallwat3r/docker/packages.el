;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "d5255a65b7240d0038cc417f301b43df05a27922")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "afcd418d82a0162dd59c44a5e0c558bcb1d1d7ca")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "423c216fdef06d32dde835bb91daff353f41b72e")
(package! kubernetes-evil :pin "423c216fdef06d32dde835bb91daff353f41b72e")
