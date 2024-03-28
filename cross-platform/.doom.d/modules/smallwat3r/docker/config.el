;;; smallwat3r/docker/config.el -*- lexical-binding: t; -*-

;; Docker
;; doc: https://github.com/Silex/docker.el
(use-package! docker
  :defer t)

;; Dockerfile mode
;; doc: https://github.com/spotify/dockerfile-mode
(use-package! dockerfile-mode
  :mode "Dockerfile$")

;; Docker tramp
;; doc: https://github.com/emacs-pe/docker-tramp.el
(when (< emacs-major-version 29)
  (use-package! docker-tramp
    :after tramp))

;; Kubernetes
;; doc: https://github.com/chrisbarrett/kubernetes-el
(use-package! kubernetes
  :defer t
  :custom
  (kubernetes-redraw-frequency 10)
  (kubernetes-poll-frequency 10)
  (kubernetes-default-exec-command "/bin/sh"))

;; Evil integration to Kubernetes
(use-package! kubernetes-evil
  :after kubernetes-overview)
