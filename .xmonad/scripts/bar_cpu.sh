#!/bin/bash
source $(dirname $0)/config.sh

PERC=`mpstat -P ALL 0 | awk '{gsub(//,""); print $4}' | sed -n 4p`

ICON="cpu.xbm"
if [[ $PERC > 75 ]]; then
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $warning -h 1 -w 40`
else
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $highlight -h 1 -w 40`
fi

ICON='^i(/home/lingnan/.xmonad/dzen2/'"$ICON)"
echo "$ICON$PERCBAR"
