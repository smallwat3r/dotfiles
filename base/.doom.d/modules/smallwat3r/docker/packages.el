;; -*- no-byte-compile: t; -*-
;;; smallwat3r/docker/packages.el

;; https://github.com/Silex/docker.el/commits/master
(package! docker :pin "916686b86e83a3bd2281fbc5e6f98962aa747429")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  ;; https://github.com/emacs-pe/docker-tramp.el/commits/master
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
;; https://github.com/spotify/dockerfile-mode/commits/master
(package! dockerfile-mode :pin "97733ce074b1252c1270fd5e8a53d178b66668ed")
;; https://github.com/kubernetes-el/kubernetes-el/commits/master
(package! kubernetes :pin "036583995bfceb0231738f65dd09c029ad812b02")
(package! kubernetes-evil :pin "036583995bfceb0231738f65dd09c029ad812b02")
