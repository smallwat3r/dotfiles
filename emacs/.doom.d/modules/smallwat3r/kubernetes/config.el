;;; smallwat3r/kubernetes/config.el -*- lexical-binding: t; -*-

;; Manage kubernetes from Emacs
;; doc: https://github.com/chrisbarrett/kubernetes-el
(use-package! kubernetes
  :commands (kubernetes-overview)
  :custom
  (kubernetes-redraw-frequency 10)
  (kubernetes-poll-frequency 10)
  (kubernetes-default-exec-command "/bin/sh")
  :init
  (map!
   (:leader
    (:prefix ("k" . "kubernetes")
     :desc "Overview"           "o" #'kubernetes-overview
     :desc "Set context"        "c" #'kubernetes-use-context
     :desc "Set namespace"      "n" #'kubernetes-set-namespace
     :desc "Display logs"       "l" #'kubernetes-logs-fetch-all
     :desc "Display service"    "s" #'kubernetes-display-service
     :desc "Display secrets"    "S" #'kubernetes-display-secret
     :desc "Display deployment" "d" #'kubernetes-display-deployment
     :desc "Describe"           "D" #'kubernetes-describe-pod
     :desc "Exec into"          "e" #'kubernetes-exec-into))))

(use-package! kubernetes-evil
  :after kubernetes)
