;;; smallwat3r/mail-ext/config.el -*- lexical-binding: t; -*-

(if (featurep :system 'macos)
    (setq sendmail-program "/opt/homebrew/bin/msmtp")
  (setq sendmail-program "/usr/bin/msmtp"))

(setq mail-user-agent 'message-user-agent
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Email client
;; doc: https://notmuchmail.org/emacstips/
(after! notmuch
  ;; Main buffer sections information.
  (setq notmuch-show-log nil
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags))

  ;; Email list formats
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-15s ")
          ("tags" . "(%s) ")
          ("subject" . "%-72s")))

  ;; Use a custom command to fetch for new emails with mbsync
  (setq +notmuch-sync-backend "mbsync -a && notmuch new")

  (setq my-user-mail-address-2 (my/get-email my-user-alias))

  ;; Set default tags on replies
  (setq notmuch-fcc-dirs
        `((,user-mail-address . "personal/sent -inbox +sent -unread")
          (,my-user-mail-address-2 . "sws/sent -inbox +sent -unread"))))
