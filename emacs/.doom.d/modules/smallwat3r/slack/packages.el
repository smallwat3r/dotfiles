;; -*- no-byte-compile: t; -*-
;;; smallwat3r/slack/packages.el

;; (package! slack)
;; The default branch of the Slack package breaks while authenticating with
;; Slack. A PR is pending for approval since ages, so use its branch while
;; its pending being merged.
(package! slack :recipe (:host github :repo "aculich/emacs-slack" :branch "cookie"))
