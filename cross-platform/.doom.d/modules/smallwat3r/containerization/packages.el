;; -*- no-byte-compile: t; -*-
;;; smallwat3r/containerization/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "cc0046e6a557dce0ccc4108dd22e04f21ba8b0dc")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "930d7b46c180d8a13240a028c1b40af84f2a3219"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "8163fd38015cbde0485f6eaab41450132bf6e19d")
(package! kubernetes-evil :pin "d2559c3c506372ab06e495f97034537eaac0f6e6")
