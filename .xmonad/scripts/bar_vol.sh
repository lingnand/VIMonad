#!/bin/bash

source $(dirname $0)/config.sh

volLine=$(amixer get Master | egrep -m 1 "[0-9]+%")
AMASTER=${volLine%%\%*}
AMASTER=${AMASTER##*\[}
if [[ $volLine = *\[on\]* ]]; then
    ICON="spkr_01.xbm"
    #PERCBAR=`echo "$AMASTER"\
        #| gdbar -bg $bar_bg -fg $highlight -h 1 -w 40`
else
    ICON="spkr_02.xbm"
    #PERCBAR=`echo 0 \
        #| gdbar -bg $bar_bg -fg $highlight -h 1 -w 40`
fi

ICON='^i(/home/lingnan/.xmonad/dzen2/'"$ICON)"
#echo "$ICON $PERCBAR"
echo "$ICON"
