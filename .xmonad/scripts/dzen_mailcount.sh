#!/bin/bash

source $(dirname $0)/config.sh

MAILDIR="$HOME/.mail/lingnan.d"
NUMBER=`find "$MAILDIR/INBOX/new" "$MAILDIR/CAMBRIDGE/new" -type f | wc -l`
ICON='^i(/home/lingnan/.xmonad/dzen2/mail.xbm)'

if (( "$NUMBER" > 0 )); then
    echo "$ICON ^fg($highlight)$NUMBER"
else
    echo "$ICON ^fg()$NUMBER"
fi

