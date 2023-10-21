;; -*- no-byte-compile: t; -*-
;;; smallwat3r/containerization/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "61bb3ac0f2195a7a592df0453fe9f404bd92f9b1")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "423c216fdef06d32dde835bb91daff353f41b72e")
(package! kubernetes-evil :pin "423c216fdef06d32dde835bb91daff353f41b72e")
