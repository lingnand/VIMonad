#!/bin/bash
CONFIG_FILE="$(dirname $0)/config.sh"

source "$CONFIG_FILE"

# we should position at the most
oldIFS="$IFS"
IFS=$'\n'
output=( $(screen-res $'%w\n%h') )
sw="${output[0]}"
sh="${output[1]}"
IFS="$oldIFS"

WIDTH=$((sw / 4))
HEIGHT=$(( WIDTH / 10 ))
XPOS=$(( (sw - WIDTH) / 2 ))
YPOS=$(( (sh - STATUSBAR_HEIGHT - HEIGHT) / 2 ))
# WIDTH="15"
# XPOS=$(( sw - WIDTH ))
# LINES="21"

vol="`vol`"
if [ $? = 1 ]; then
    vol=0
fi

echo "$vol" | (gdbar -bg $background -fg $white0 -h $HEIGHT -w $WIDTH; sleep 5) | tee >(dzen2 -x $XPOS -y $YPOS -h $HEIGHT -w $WIDTH -ta l -xs 1) >(dzen2 -x $XPOS -y $YPOS -h $HEIGHT -w $WIDTH -ta l -xs 2)

# using simpler drawing via gdbar


# val=()
#
# space=$((LINES-2))
# inc=$((100 / space))
# top=100
# while ((top > 0)); do
#     if [ "$vol" -gt "$top" ]
#     then
#         val+=("^bg($white0)      ")
#     else
#         val+=("^bg($background)      ")
#     fi
#     top=$((top-inc))
# done
#
# (echo " +" 
# for i in "${val[@]}"; do
#     echo "$i"
# done 
# echo " -"
# sleep 5) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse'
