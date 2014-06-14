#! /bin/sh
source $(dirname $0)/config.sh
shopt -s extglob
# the aim of this script is to output the music information to dzen2 in a reasonable way

# the limit is for (status, song description, lyrics), in BYTES
#limits=(0 120 0)
# we use a diffusion algorithm (if the first field is below the limit then that difference is added to the second field, so on and so forth)
channel_limit=25
artist_limit=15
song_limit=20
sep=' '

channel_color=
artist_color="#bea492"
song_color="$highlight"
liked_song_color="$notify"
icon_color="#4c7899"
lyric_color="#a6e22e"
lyricinactive_color=

error() {
    echo " "
    [ -n "$1" ] && exit "$1" || exit 1
}

strol() {
    # first estimate the real length of the string
    local bl="`wc -c <<< "$1"`"
    local sl="${#1}"
    local mbl=$(( (bl-1-sl)/2 ))
    local al=$(( sl-mbl ))
    local rl=$(( al+mbl*2 ))
    echo "$rl"
}

#$1 the limit (in columns), $2 the string to be cut
cutstr() {
    local tr="${2##*( )}"
    tr="${tr%%*( )}"
    local ms="$tr" rl="`strol "$tr"`" l="$1" sl="${#tr}"
    # set up the diffusion
    l=$(( l+DIFFUSION ))
    if (( l != 0 )); then
        while (( rl > l )); do
            ms="${ms%?}"
            [ "${#ms}" == '0' ] && break
            rl="`strol "$ms"`"
            # add in the length for the two dots
            rl=$(( rl+2 ))
        done
    fi
    ! [ "$ms" == "$2" ] && ms="$ms.."
    # calculate the diffusion
    DIFFUSION=$(( l-rl ))
    RESULT="$ms"
}

DIFFUSION=0
output="`rpc info $'%u\t%t\t%a\t%c\t%k\t%r'`"
status="`cut -d$'\t' -f1 <<< "$output"`"
cutstr "$channel_limit" "`cut -d$'\t' -f4 <<< "$output"`"
channel="$RESULT"
cutstr "$artist_limit" "`cut -d$'\t' -f3 <<< "$output"`"
artist="$RESULT"
cutstr "$song_limit" "`cut -d$'\t' -f2 <<< "$output"`"
song="$RESULT"
kbps="`cut -d$'\t' -f5 <<< "$output"`"
liked="`cut -d$'\t' -f6 <<< "$output"`"

stopped=false
stat= desc= lrc=
case "$status" in
    play) 
        icon="$icondir/play.xbm"
        ;;
    pause)
        icon="$icondir/pause.xbm"
        ;;
    stop)
        stopped=true
        icon="$icondir/note.xbm"
        ;;
    *)
        error
        ;;
esac

stat="^fg($icon_color)^i($icon) ^fg($bar_bg)[^fg($channel_color)$channel^fg($bar_bg)]" 
[ "$liked" = '1' ] && sc="$liked_song_color" || sc="$song_color"
if ! $stopped && [ -n "$song" ]; then
    desc="^fg($artist_color)$artist ^fg()| ^fg($sc)$song" 
    #lrc="`tail -n 1 /tmp/fmclrc.log`"
    #lrc="^fg($lyric_color)${lrc//$'\t'/ ^fg($lyricinactive_color)}"
fi

shopt -u extglob
echo "$stat$sep$desc"
