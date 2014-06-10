#!/bin/bash
#
# This script gets the beautiful wallpapers from http://wallbase.cc
# This script is brought to you by 7sins@4geeksfromnet.com
# and at present is actively maintained by MacEarl
#
#
# This Script is written for GNU Linux, it should work under Mac OS
#
#
# Revision 2.8.1
# 1. addded "-w" flag to grep for check if wallpaper was already downloaded.
#    Before it was not looking for an exact match, so wallpaper-123 would be 
#    recognized as downloaded if you already downloaded wallpaper-1234
#
#
# Revision 2.8
# Contributed by MacEarl
# 1. Added Option to rename files accordingly to their tags. (experimental)
#
#
# Revision 2.7.1
# Contributed by MacEarl
# 1. Fixed Login, everything should work again (except the related wallpaper feature)
#
#
# Revision 2.7
# Contributed by MacEarl
# 1. Fixed most issues (wallbase v4 update)
#    No need for Base64 anymore
# 2. Login Feature does not work, that means no nsfw wallpapers, Favourites or Uploads from a User 
#    (Collections with some nsfw Images will be downloaded without the nsfw images)
# 3. Download related Wallpapers Function does not work (it seems the related Wallpaper function was removed, if you find it please let me know)
#
#
# Revision 2.6.2
# Contributed by MacEarl
# 1. Fixed Download related Wallpapers Fuction (just forgot to add the base64 stuff last time)
# 2. It is now possible to download a Range of Wallpapers with all related Wallpapers
# 
#
# Revision 2.6.1
# Contributed by MacEarl
# 1. Fixed Download Function (They added base64 encrypted urls)
#	 The Script now uses "base64" to decode those urls
#	 so make sure you got that installed ;)
#
#
# Revision 2.6
# Contributed by MacEarl
# 1. Added Function to download Related Wallpapers
#
#
# Revision 2.5.1
# Contributed by MacEarl
# 1. Added some Explanation in Section 6 to clarify the combination of the THPP and Max_Range Variables
#
#
# Revision 2.5
# Contributed by MacEarl
# 1. Added Function to download a specified Range of Wallpaper
#
#
# Revision 2.4
# Contributed by MacEarl and HansTester
# 1. Fixed Login Feature (NSFW working again) (Thanks to HansTester)
# 2. Implemented download uploads by specific User (Thanks to HansTester)
#
#
# Revision 2.3
# Contributed by MacEarl
# 1. Added Categorize Feature
# 2. Source Cleanup
#
#
# Revision 2.2
# Contributed by Axa-Ru and MacEarl
# 1. Source Cleanup by Axa-Ru
# 2. Fixed Download Code for new System
# 3. Improved the check for already downloaded Files ( much faster ;) )
#
#
# Revision 2.1
# Contributed by MacEarl
# 1. Added a Feature to download "Your Favorites"
# 2. Added a Feature to download "user created collections"
# 3. Rewrote the check for already downloaded Files
#    (You now can rename or remove Wallpapers and they dont get downloaded again.
#     Eg. If you don''t like a wallpaper, just delete it and it won''t get downloaded again.
#     To re-enable the download of a specific Wallpaper you need to remove the
#     Wallpaper number from the file "downloaded.txt")
#
#
# Revision 2.0
# Contributed by MacEarl
# 1. Rewritten code for new Wallbase System
#
#
# Revision 1.2.1
# Contributed by MacEarl
# 1. Added Login Check for new Wallpapers
#
#
# Revision 1.2
# Contributed by MacEarl
# 1. Added a login feature to download NSFW content/category
#
#
# Revision 1.1.1
# Contributed by Hab
# 1. Updated mkdir option with -p flag
#
#
# Revision 1.1
# Contributed by MacEarl
# 1. Added a Search Function
# 2. Added a check for already existing Files
# 3. Fixed a bug (imageshack mirrored files)
#
#
# Revision 1.0
# Contributed by MacEarl
# 1. Added the much needed fixes for NSFW category
# 2. Updated the script with more options
# 3. Modified the script
#
#
#
# Wallpapers can be sorted according to
#
################################
### Section 1 :: Resolution  ###
################################
#
# Resolution
#   Accepted values are 0 => All Standard
#       800x600 | 1024x768 | 1280x960 | 1280x1024 | 1400x1050 | 1600x1200 | 2560x2048
#   Widescreen
#       1024x600 | 1280x800 | 1366x768 | 1440x900 | 1600x900 | 1680x1050 | 1920x1080 | 1920x1200 | 2560x1440 | 2560x1600
#
################################
### Section 2 :: Aspect Ratio###
################################
#
# Aspect Ratio
#   Accepted values are 0 => All
#   1.33 => 4:3
#   1.25 => 5:4
#   1.77 => 16:9
#   1.60 => 16:10
#   1.70 => Netbook
#   2.50 => Dual
#   3.20 => Dual Wide
#   0.99 => Portrait
#
################################
###  Section 3 :: PURITY     ###
################################
#
# Category : SFW, Sketchy, NSFW
# Each being toggled by a 1/0 value
#   So to get only SFW use 100
#   To get all categories use 111
#   To get Sketchy and NSFW use 011
#
################################
###    Section 4 :: Topic    ###
################################
#
# Topic : Anime/Manga, Wallpapers/General, High Resolution Images
#   To get Anime/Manga use 1
#   To get Wallpapers/General use 2
#   To get HR Images use 3
#   To get all use 123
#   To get only HR and WP use 23 and so on
#
################################
###    Section 5 :: Size     ###
################################
#
# Size: at least and Exactly width x height
#   To get at least desired Resolution use gteq
#   To get exactly desired Resolution use eqeq
#
################################
###    Section 6 :: THPP     ###
################################
#
# Thumbnails per page.
#  Accepted values are 20, 32, 40, 60
#
# For Max_Range multiples of the chosen THPP variable are recommended.
#
# For example if your THPP variable is 20 you should use 20, 40, 60, ... and so on for Max_Range
#
################################
###  Section 7 :: Location   ###
################################
#
# The download location 
# Foldername of desired Location e.g. "~/Wallpapers"
#
################################
###   Section 8 :: Best of   ###
################################
#
# Best of:
#  All time = 1
#  3Months  = 3m
#  2Months  = 2m
#  1Month   = 1m
#  2Weeks   = 2w
#  1Week    = 1w
#  3Days    = 3d
#  1Day     = 1d
#
################################
###    Section 9 :: Type     ###
################################
#
# Random                    = 1
# Toplist                   = 2
# Newest                    = 3
# Search                    = 4
# Favourites                = 5
# User created collections  = 6
# Uploads from User X	    = 7
#
################################
###   Section 10 :: Order    ###
################################
#
# Date                  = date
# Amount of Views       = views
# Number of Favorites   = favs
# Relevancie            = relevance
#
################################
### Section 11 :: OrderType  ###
################################
#
# The following two Options are possible:
#  Ascending    = asc
#  Descending   = desc
#
################################
###   Section 12 :: Search   ###
################################
#
# Define your Search Query like this:
#  ./wallbase.sh Mario
#  For longer Search Queries you need to set QUERY manually
#  For Example set QUERY="Link | Zelda | Legend of Zelda | OoT"
#  or QUERY="=(Mario Luigi)"
#
#  Accepted Operators are "=( ...)" for AND and "|" for OR
#
################################
###   Section 13 :: Login    ###
################################
#
# Due to changes in the wallbase.cc "Policy"
#  you now need to login to see NSFW Content
#
#  It is also needed if you want to download "your own Favorites" (Duh!)
#  or uploads from a user
#  
#  Please provide your Username and Password below
#  to download NSFW content
#
################################
### Section 14 :: Collection ###
################################
#
# This Option is used for downloading your Favorites
# and to download Collections created by other users or
# all Wallpapers uploaded from a Specific User
#
# Set the value to "0" to download your Favorites in your "Home" Collection.
# 
# To download User Collections or different Favorite Collections open the desired
# Collection in your Browser and copy the following part
#  1. For your Favorites: http://wallbase.cc/favorites/"#number_of_the_collection"
#  2. For user created collections: http://wallbase.cc/collection/"#number_of_the_collection"
# You only need the number which is shown at the end of the URL
#
# To download Wallpapers uploaded by a specific user open the profile
# in your Browser and copy the following part
# http://wallbase.cc/user/id-"#UserID"/uploads
# You only need the number between user and uploads
#
################################
### Section 15 :: Categorize ###
################################
#
# This Option will help you keep your downloaded wallpapers ordered
# It will create subfolders for the TOPIC and PURITY
# so you wont have to spend much time looking for a special wallpaper
#
# Set this option to a value greater 0 to set it active
#
# For example:
#  TOPIC="2"
#  PURITY="100"
#  LOCATION="./wallpapers"
#
#  Then the path the folder are being downloaded to looks like this:
#  ./wallpapers/100/2/
#
################################
### Section 16 :: WP Range   ###
################################
#
# This Option will download a specified Range of Wallpapers
# 
# Set WP_RANGE_STOP to a value greater 0 to set it active
#
# For example:
#  WP_RANGE_START=10000
#  WP_RANGE_STOP=10200
#
#  This Setting will download all Wallpapers from
#  10.000 to 10.200
# 
# NOT WORKING! 
################################
### Section 17 :: Related    ###
################################
#
# This Option will also Download all Related Wallpapers
# 
# Related = 0 --> Deactivated
# Related = 1 --> Activated
#
#
################################
### Section 18 :: Keywords   ###
################################
#
# This Option changes the filename to the wallpapers
# Tags
#
# Warning: The filenames can get very long, also it could contain special
# Characters. Also some Wallpapers have no Tags, this will result in empty Filenames.
# Use with Caution
# 

##################################
###    Needed for NSFW/New     ###
##################################
 
# See Section 13
# Enter your Username
USER=""
# Enter your password
PASS=""
 
#################################
###  End needed for NSFW/New  ###
#################################

 
#################################
###   Configuration Options   ###
#################################
 
# For accepted values see Section 6
MAX_RANGE=180
# For accepted values of resolution see Section 1
RESOLUTION=1920x1080
# For accepted values of aspect ratio see Section 2
ASPECTRATIO=1.60+1.77
# For accepted values of PURITY see Section 3
PURITY=100
# For accepted values of topic see Section 4
TOPIC=123
# For accepted values for SIZE see Section 5
SIZE=gteq
# For accepted Thumbnails per page see Section 6
THPP=60
# For download location see Section 7
# let's make it a search based downloader (only search what's required)
LOCATION="$2"
# Best of : see Section 8
TIME=1
# For Types see Section 9
# we can determine the type from a single argument
case "$1" in
    '#rand') 
        # if it's random should we remove the content in the random first?
        rm -f "$LOCATION"/*
        TYPE=1
        ;;
    '#top')
        TYPE=2
        ;;
    '#new')
        TYPE=3
        ;;
    '#'*)
        ## we don't except other special commands
        exit 1
        ;;
    *)
        TYPE=4
        ;;
esac
# For order Options see Section 10
ORDER=relevance
# See Section 11
ORDER_TYPE=desc
# See Section 12
QUERY="$1"
# See Section 14
COLLECTION=0
# See Section 15
CATEGORIZE=0
# See Section 16
WP_RANGE_START=0
WP_RANGE_STOP=0
# See Section 18 | NOT RECOMMENDED!
KEYWORD_FILENAME=0

# not working
# # See Section 17
# Related=0

#################################
### End Configuration Options ###
#################################
 
# if wished categorize the downloads
# by their PURITY(nsfw,sfw,sketchy) 
# and TOPIC (manga, hd, general)
if [ $CATEGORIZE -gt 0 ]; then
	LOCATION="$LOCATION/$PURITY/$TOPIC"
fi

if [ ! -d "$LOCATION" ]; then
	mkdir -p "$LOCATION"
fi

cd "$LOCATION"


#
# logs in to the wallbase website to give the user more functionality
# requires 2 arguments:
# arg1: username
# arg2: password
#
function login {
	# checking parameters -> if not ok print error and exit script
	if [ $# -lt 2 ] || [ $1 == '' ] || [ $2 == '' ]; then
        echo "Please check the needed Options for NSFW/New Content (username and password)"
        echo ""
        echo "For further Information see Section 13"
        echo ""
        echo "Press any key to exit"
        read
        exit
    fi
    
    # everythings ok --> login
    wget --keep-session-cookies --save-cookies=cookies.txt --referer=http://wallbase.cc/home http://wallbase.cc/user/login
    csrf="$(cat login | grep 'name="csrf"' | sed  's .\{44\}  ' | sed 's/.\{2\}$//')"
    ref="$(rawurlencode $(cat login | grep 'name="ref"' | sed  's .\{43\}  ' | sed 's/.\{2\}$//'))" 
    wget --load-cookies=cookies.txt --keep-session-cookies --save-cookies=cookies.txt --referer=http://wallbase.cc/user/login --post-data="csrf=$csrf&ref=$ref&username=$USER&password=$PASS" http://wallbase.cc/user/do_login
} # /login

# 
# downloads Page with Thumbnails 
#
function getPage {
	# checking parameters -> if not ok print error and exit script
	if [ $# -lt 1 ]; then
		echo "getPage expects at least 1 argument"
		echo "arg1:	 parameters for the wget command"
		echo ""
		echo "press any key to exit"
		read
		exit
	fi

	# parameters ok --> get page
	wget --timeout 3 --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc -O tmp "http://wallbase.cc/$1"
} # /getPage

#
# downloads all the wallpapers from a wallpaperfile
# arg1:	the file containing the wallpapers
#
function downloadWallpapers {
	URLSFORIMAGES="$(cat tmp | grep -o "http://wallbase.cc/wallpaper/.*" | cut -d " " -f 1)"
	for imgURL in $URLSFORIMAGES
		do
		img="$(echo $imgURL | sed 's/.\{1\}$//')"
		number="$(echo $img | sed  's .\{29\}  ')"
		if cat downloaded.txt | grep -w "$number" >/dev/null
			then
				echo "File already downloaded!"
			else
				echo $number >> downloaded.txt
				wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc $img
				filename="$(cat $number | grep 'meta name="keywords"' | sed  's .\{35\}  ' | sed 's/.\{4\}$//' | tr -d ',' | sed -r 's/ background| desktop| best| widescreen| wallpaper|background |desktop |best |widescreen |wallpaper //g')"
				url="`cat $number | egrep -m1 -o "http://wallpapers.*(png|jpg|gif)"`"
                fname="${url##*/}"
                wget --keep-session-cookies --load-cookies=cookies.txt --referer=http://wallbase.cc/wallpaper/$number -O "$fname.tmp" "$url"
                mv "$fname.tmp" "$fname"
				if [ "$KEYWORD_FILENAME" == 1 ]
					then
						rename wallpaper-$number "$filename" wallpaper-$number????
				fi
				# if [ $Related == 1 ]
				# 	then
				# 		wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc -O related.html http://wallbase.cc/related/$number
				# 		URLSFORIMAGES_related="$(cat related.html | grep -o "http:.*" | cut -d " " -f 1 | grep wallpaper)"
				# 		rm $number
				# 		for imgURL in $URLSFORIMAGES_related
				# 			do
				# 			img="$(echo $imgURL | sed 's/.\{1\}$//')"
				# 			number="$(echo $img | sed  's .\{29\}  ')"
				# 			if cat downloaded.txt | grep "$number" >/dev/null
				# 				then
				# 					echo "File already downloaded!"
				# 				else
				# 					echo $number >> downloaded.txt
				# 					wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc $img
				# 					cat $number | grep -o "'+B.*+" | sed 's/.\{3\}$//' | sed 's .\{5\}  ' | base64 -d | wget --keep-session-cookies --load-cookies=cookies.txt --referer=http://wallbase.cc/wallpaper/$number -i -
				# 					rm $number
				# 			fi
				# 			done
				#	else
						rm $number
				# fi	
		fi
		done
        rm tmp
} #/downloadWallpapers

#
# urlencodes the ref value from the login page
# arg1:	the ref value from the login page
#
# source: http://stackoverflow.com/a/10660730
#
function rawurlencode() {
	local string="${1}"
  	local strlen=${#string}
  	local encoded=""

  	for (( pos=0 ; pos<strlen ; pos++ )); do
    	c=${string:$pos:1}
     	case "$c" in
        	[-_.~a-zA-Z0-9] ) o="${c}" ;;
        	* )               printf -v o '%%%02x' "'$c"
     	esac
     		encoded+="${o}"
  	done
  	echo "${encoded}"
} 
 
# login only when it is required ( for example to download favourites or nsfw content... )
if [ $PURITY == 001 ] || [ $PURITY == 011 ] || [ $PURITY == 111 ] || [ $TYPE == 5 ] || [ $TYPE == 7 ] ; then
   login $USER $PASS
fi

if [ $WP_RANGE_STOP -gt 0 ]; then
	#WP RANGE
	for (( count= "$WP_RANGE_START"; count< "$WP_RANGE_STOP"+1; count=count+1 ));
	do
		if cat downloaded.txt | grep "$count" >/dev/null
		 	then
		 		echo "File already downloaded!"
		 	else
				echo $count >> downloaded.txt
		 		wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc http://wallbase.cc/wallpaper/$count
		 		cat $count | egrep -o "http://wallpapers.*(png|jpg|gif)" | wget --keep-session-cookies --load-cookies=cookies.txt --referer=http://wallbase.cc/wallpaper/$number -i -
				# if [ $Related == 1 ]
				# 	then
				# 		wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc -O related.html http://wallbase.cc/related/$count
				# 		URLSFORIMAGES_related="$(cat related.html | grep -o "http:.*" | cut -d " " -f 1 | grep wallpaper)"
				# 		rm $count
				# 		for imgURL in $URLSFORIMAGES_related
				# 			do
				# 			img="$(echo $imgURL | sed 's/.\{1\}$//')"
				# 			number="$(echo $img | sed  's .\{29\}  ')"
				# 			if cat downloaded.txt | grep "$number" >/dev/null
				# 				then
				# 					echo "File already downloaded!"
				# 				else
				# 					echo $number >> downloaded.txt
				# 					wget --keep-session-cookies --load-cookies=cookies.txt --referer=wallbase.cc $img
				# 					cat $number | grep -o "'+B.*+" | sed 's/.\{3\}$//' | sed 's .\{5\}  ' | base64 -d | wget --keep-session-cookies --load-cookies=cookies.txt --referer=http://wallbase.cc/wallpaper/$number -i -
				# 					rm $number
				# 			fi
				# 			done
				# 		rm related.html
				# 	else
					rm $count
				# fi	
		fi
		done

elif [ $TYPE == 1 ] ; then
    # RANDOM
    for (( count= 0; count< "$MAX_RANGE"; count=count+"$THPP" )); 
    do
		getPage "random/index/$count?section=wallpapers&res_opt=$SIZE&res=$RESOLUTION&thpp=$THPP&purity=$PURITY&board=$TOPIC&aspect=$ASPECTRATIO"
		downloadWallpapers
    done
 
elif [ $TYPE == 2 ] ; then
    # TOPLIST
    for (( count= 0; count< "$MAX_RANGE"; count=count+"$THPP" ));
    do
        getPage "toplist/index/$count?section=wallpapers&res_opt=$SIZE&res=$RESOLUTION&thpp=$THPP&purity=$PURITY&board=$TOPIC&aspect=$ASPECTRATIO&ts=$TIME"
        downloadWallpapers
    done

elif [ $TYPE == 3 ] ; then
    # NEWEST
    for (( count= 0; count< "$MAX_RANGE"; count=count+"$THPP" ));
    do
        getPage "search/index/$count?section=wallpapers&res_opt=$SIZE&res=$RESOLUTION&order_mode=$ORDER_TYPE&order=date&thpp=$THPP&purity=$PURITY&board=$TOPIC&aspect=$ASPECTRATIO"
        downloadWallpapers
    done
 
elif [ $TYPE == 4 ] ; then
    # SEARCH
    for (( count= 0; count< "$MAX_RANGE"; count=count+"$THPP" ));
    do
        getPage "search/index/$count?section=wallpapers&q=$QUERY&res_opt=$SIZE&res=$RESOLUTION&order_mode=$ORDER_TYPE&order=$ORDER&thpp=$THPP&purity=$PURITY&board=$TOPIC&aspect=$ASPECTRATIO"
        downloadWallpapers
    done
	
elif [ $TYPE == 5 ] ; then
    # FAVOURITES
    for (( count= 0; count< "$MAX_RANGE"; count=count+"32" ));
	do
		getPage favorites/$COLLECTION/$count
		downloadWallpapers
	done
 
elif [ $TYPE == 6 ] ; then
    # USER CREATED COLLECTIONS
    for (( count= 0; count< "$MAX_RANGE"; count=count+"32" ));
    do
        getPage collection/$COLLECTION/$count
        downloadWallpapers
    done

elif [ $TYPE == 7 ] ; then
    # UPLOADS FROM SPECIFIC USER
    for (( count= 0; count< "$MAX_RANGE"; count=count+"32" ));
    do
        getPage user/id-$COLLECTION/uploads/$count
        downloadWallpapers
    done

else
	echo error in TYPE please check Variable
fi

rm -f cookies.txt login do_login
