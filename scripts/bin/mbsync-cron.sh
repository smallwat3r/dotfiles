#!/usr/bin/env bash
#
# Syncing emails every minute, triggered by a cron job
# Run mbsync and add new emails to notmuch database
#
#   crontab -e
#   */1 * * * * /usr/local/bin/mbsync-cron.sh >/tmp/mbsync-cron-stdout.log 2>/tmp/mbsync-cron-stderr.log

/usr/bin/killall mbsync &>/dev/null  # kill current running instances of mbsync
/usr/local/bin/mbsync -a
/usr/local/bin/notmuch new
exit 0
