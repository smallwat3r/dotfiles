;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "46b597a711492e1c19d1260951f39372450278f5")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "7ce17e054eca4d56ca8bc1e4a6a0dbf58efd8d52")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "3a73b7113beacff368960f96b2ee10c11190b3a2")
(package! kubernetes-evil :pin "3a73b7113beacff368960f96b2ee10c11190b3a2")
