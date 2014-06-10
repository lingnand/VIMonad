#!/bin/bash
CONFIG_FILE="$(dirname $0)/config.sh"

source "$CONFIG_FILE"

# we should position at the most
sw="$(screen-res width)"
WIDTH="15"
XPOS=$(( sw - WIDTH ))
LINES="22"

output="$(amixer get Master)"
if fgrep "[off]" <<< "$output" >/dev/null 2>&1; then
    vol=0
else
    vol="$(egrep -m 1 -o "[0-9]+%" <<< "$output")"
    vol="${vol%\%}"
fi
val=()

space=$((LINES-2))
inc=$((100 / space))
top=100
while ((top > 0)); do
    if [ "$vol" -gt "$top" ]
    then
        val+=("^bg($white0)      ")
    else
        val+=("^bg($background)      ")
    fi
    top=$((top-inc))
done

(echo " +" 
for i in "${val[@]}"; do
    echo "$i"
done 
echo " -"
sleep 5) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse'
