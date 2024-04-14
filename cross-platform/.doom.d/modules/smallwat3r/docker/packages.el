;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "d5255a65b7240d0038cc417f301b43df05a27922")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "39a012a27fcf6fb629c447d13b6974baf906714c")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "24de7a1621dc06234abf63db454d63a522aaa632")
(package! kubernetes-evil :pin "24de7a1621dc06234abf63db454d63a522aaa632")
