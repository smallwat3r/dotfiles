;;; smallwat3r/slack/config.el -*- lexical-binding: t; -*-

;; Slack
;;
;; To get a token:
;;   - Open Chrome and sign into slack at https://my.slack.com/customize
;;   - From the dev tools console type: TS.boot_data.api_token
;;
;; To get a cookie:
;;   - Get cookie entry "d" with its default encoded value
;;
;; doc: https://github.com/yuya373/emacs-slack
(use-package! slack
  :commands (slack-start)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  :config
  (slack-register-team
   :default t
   :name "Work"
   :token (+pass-get-secret "slack/work/token")
   :cookie (+pass-get-secret "slack/work/cookie")
   :full-and-display-names t))
