;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "6f8bba0d11a5143872dfc25afdabe16cae410d11")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "4d893bd2da15833ce056332e6c972d5d93e78f04")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "24de7a1621dc06234abf63db454d63a522aaa632")
(package! kubernetes-evil :pin "24de7a1621dc06234abf63db454d63a522aaa632")
