#!/usr/bin/env bash
#
# Syncing emails every minute, triggered by a cron job
# Run mbsync and add new emails to notmuch database
#
#   crontab -e
#   */1 * * * * /usr/local/bin/mbsync-cron.sh

killall mbsync &>/dev/null  # kill current running instances of mbsync
mbsync -a -q
notmuch new &>/dev/null
exit 0
