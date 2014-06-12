import Data.List
import System.IO
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import qualified XMonad.Hooks.UrgencyHook as U
import XMonad.Layout.Decoration
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowProperties
import XMonad.Vim
import XMonad.Vim.Routine
import XMonad.Vim.Prompt.Vimb
import XMonad.Vim.TaskGroup
import XMonad.Vim.Term
import XMonad.Vim.WindowStyle
import XMonad.Vim.Workspaces
import XMonad.Vim.Constants

---- ModMask
myModMask = mod4Mask

---- Font
myTerminalFont = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
myFont = "xft:WenQuanYi Zen Hei Mono:pixelsize=18"
myBigFont = "xft:WenQuanYi Zen Hei Mono:pixelsize=20"
myDzenFont = "WenQuanYi Zen Hei Mono:pixelsize=17"

---- Color constants
color0= "#332d29"
color8= "#817267"

color1= "#8c644c"
color9= "#9f7155"

color2= "#c4be90"
color10= "#bec17e"

color3= "#bfba92"
color11= "#fafac0"

color4= "#646a6d"
color12= "#626e74"

color5= "#6d6871"
color13= "#756f7b"

color6= "#3b484a"
color14= "#444d4e"

color7= "#504339"
color15= "#9a875f"

---- Colorscheme
myFgColor               = "#888888"
myFgHLight              = "#ffffff"
myFgDimLight            = "#505050"
myTextHLight            = "#fafac0"
myNotifyColor           = "#f39d21"
myWarningColor          = "#d23d3d"

myBgColor               = "#181512"
myBgDimLight            = "#333333"
myBgHLight              = "#4c7899"
myBorderColor           = "#222222"
myBorderHLight          = "#285577"

myVimStatusTheme = def { backgroundColor = myBgColor
                       , foregroundColor = myFgColor
                       , textHighLight = myTextHLight
                       , notifyColor = myNotifyColor
                       , foregroundHighLight = myFgHLight
                       , foregroundDimLight = myFgDimLight
                       }

myTabsTheme = def { activeColor         = myBgHLight
                  , inactiveColor       = myBgDimLight
                  , activeBorderColor   = myBorderHLight
                  , inactiveBorderColor = myBorderColor
                  , activeTextColor     = myFgHLight
                  , inactiveTextColor   = myFgColor
                  , fontName            = myFont
                  , decoHeight          = 25 
                  }

mySubTheme = def { winInactiveColor = inactiveColor myTabsTheme
                 , winInactiveBorderColor = inactiveBorderColor myTabsTheme
                 , winInactiveTextColor = inactiveTextColor myTabsTheme
                 , winActiveColor = activeColor myTabsTheme
                 , winActiveBorderColor = activeBorderColor myTabsTheme
                 , winActiveTextColor = activeTextColor myTabsTheme
                 , winTitleAddons = []
                 , winTitleIcons = []
                 }

myXPConfig ref = def { 
      font = myBigFont
    , bgColor = myBgDimLight
    , fgColor = myFgColor
    , bgHLight = myBgHLight
    , fgHLight = myFgHLight
    , promptBorderWidth = 0
    , height = decoHeight myTabsTheme
    , historyFilter = deleteConsecutive
    , autoComplete = Nothing
    , alwaysHighlight = False
    , searchPredicate = infixSearchPredicate
    , promptKeymap = vimXPKeymap ref
}

isPidginBuddyList = className =? "Pidgin" <&&> propertyToQuery (Role "buddy_list")

scratchpads :: [NamedScratchpad]
scratchpads = [
      NS "pidgin" "pidgin" isPidginBuddyList idHook
    ] 
--- task group defintion
-- allowed filter keys for taskgroups
-- 1. must be typeable
-- 2. must not be number
-- 3. must not be 
--      c -- current group 
--      g -- used by gg
--      S-g -- used by gG
--      p -- used by gp  
--      S-p -- used by gP 
--      i -- used by gi 
--      S-i -- used by cgI 
--      a -- used by cga 
--      S-a -- used by cgA 
--      / -- used by a{h,j,k,l}/
taskGroups = 
    let tgd = def { colorScheme = mySubTheme } in
    [ -- vimb instances
      tgd { taskGroupName = "vimb"
          , filterKey = "b"
          , filterPredicate = className =? "Vimb"
          {-, construct = runShell "vimb \"`tail -n1 ~/.config/vimb/history | cut -d'\t' -f1`\""-}
          , construct = \n w -> do-- we can trick the system by activating a yP
                case w of
                     Just _ -> runProcessWithInput "/bin/sh" ["-c","xdotool key --clearmodifiers y; xsel"] "" >>= seqn n . vbAction
                     _ -> seqn n $ vbAction ""
          {-, launchHook = ask >>= doF . -}
          -- green
          , colorScheme = mySubTheme { winInactiveColor = "#1d371d"
                                     , winActiveColor = "#337f33"
                                     , winActiveBorderColor = "#1d371d"
                                     }
          }
      -- vim intances
    , tgd { taskGroupName = "vim"
          , filterKey = "v"
          , filterPredicate = isTerm <&&> (appName =? "vim" <||> fmap (\s -> (" - VIM" `isInfixOf` s) && (not $ isInfixOf "vimpager" s)) title)
          , construct = \n _ -> seqn n $ runShell "xvim"
          -- brown
          , colorScheme = mySubTheme { winInactiveColor = "#372517"
                                     , winActiveColor = "#7f5233"
                                     , winActiveBorderColor = "#372517"
                                     }
          }
      -- pidgin buddylist
    , tgd { taskGroupName = "pidgin-buddy"
          , filterPredicate = isPidginBuddyList
          , launchHook = rightPanelHook
          , construct = \n _ -> mkNamedScratchpad scratchpads "pidgin"
          , windowStyle = windowStyleFromList [rightPanelHook, leftPanelHook, doSink]
          }
      -- pidgin conversation windows
      -- we'd like to put the conversations (self poped one) into another workspace if they are not launched by the user
    , tgd { taskGroupName = "pidgin"
          , filterKey = "d"
          , filterPredicate = className =? "Pidgin" <&&> fmap not (propertyToQuery (Role "buddy_list"))
          , construct = \_ _ -> mkNamedScratchpad scratchpads "pidgin"
          , launchHook = (liftX $ do
                ws <- gets windowset 
                case W.peek ws of
                     Just f -> runQuery (className =? "Pidgin") f >>= return . not
                     Nothing -> return True
                ) --> liftX (findWorkspaceWithName "comm" Last) >>= doF . W.shift
          -- purple
          , colorScheme = mySubTheme { winInactiveColor = "#231536"
                                     , winActiveColor = "#6a4d99"
                                     , winActiveBorderColor = "#231536"
                                     }
          }
      -- ranger instances
    , tgd { taskGroupName = "ranger"
          , filterKey = "r"
          , filterPredicate = isTerm <&&> (title =? "ranger" <||> appName =? "ranger")
          , construct = \n _ -> seqn n $ runTerm "ranger" "ranger" "loader ranger"
          -- yellow
          , colorScheme = mySubTheme { winInactiveColor = "#353119"
                                     , winActiveColor = "#7f7233"
                                     , winActiveBorderColor = "#353119"
                                     }
          }
     -- zathura instances
    , tgd { taskGroupName = "zathura"
          , filterKey = "z"
          , filterPredicate = className =? "Zathura"
          , construct = \n mw ->
              case mw of
                   Just w -> do
                       t <- runQuery title w
                       seqn n $ runShell $ "zathura "++escapeQuery t
                   _ -> seqn n $ runShell "zathura"
          -- red
          , colorScheme = mySubTheme { winInactiveColor = "#371921"
                                     , winActiveColor = "#7f334a"
                                     , winActiveBorderColor = "#371921"
                                     }
          }
    , tgd { taskGroupName = "term"
          , filterKey = "t"
          , filterPredicate = isRecyclableTerm <||> appName =? "xterm"
          , construct = \n _ -> seqn n $ runTerm "" "xterm" "zsh -i"
          }
    -- mutt general instance
    , tgd { taskGroupName = "mutt"
          , filterKey = "m"
          , filterPredicate = isTerm <&&> (title =? "mutt" <||> appName =? "mutt")
          , construct = \n _ -> seqn n $ runTerm "mutt" "mutt" "loader mutt"
          }
    -- canto general instance
    , tgd { taskGroupName = "canto"
          , filterKey = "o"
          , filterPredicate = isTerm <&&> (title =? "canto" <||> appName =? "canto")
          , construct = \n _ -> seqn n $ runTerm "canto" "canto" "loader canto"
          }
      -- intellij singleton
    -- , tgd { taskGroupName = "idea"
    --       , filterKey = "j"
    --       , filterPredicate = className =? "jetbrains-idea"
    --       , localFirst = False
    --       , construct = \n _ -> runShell "intellij-idea-ultimate-edition"
    --       }
      -- gimp singleton
    , tgd { taskGroupName = "gimp"
          , filterKey = "S-m"
          , filterPredicate = className =? "Gimp"
          , localFirst = False
          , construct = \_ _ -> runShell "gimp"
          }
      -- inkscape (can have multiple documents)
    , tgd { taskGroupName = "inkscape"
          , filterKey = "k"
          , filterPredicate = className =? "Inkscape"
          , construct = \n _ -> seqn n $ runShell "inkscape"
          }
      -- libreoffice (can have multiple documents)
    , tgd { taskGroupName = "libre"
          , filterKey = ""
          , filterPredicate = fmap (isInfixOf "libreoffice") className
          , construct = \n _ -> seqn n $ runShell "libreoffice"
          }
      -- all remaining xterms can be matched in this group
    , tgd { taskGroupName = "term(...)"
          , filterKey = ""
          , filterPredicate = isTerm
          , construct = \n _ -> seqn n $ runTerm "" "" "zsh -i"
          }
      -- all remaining windows that share the same class attributes
    , tgd { filterPredicate = hasSamePropertyAsFocused className }
      -- all other remaining windows
    , tgd
    ]

--- Status bar 
dzenBar x y w h ta fg bg font = "dzen2 -x '"++(show x)++"' -y '"++(show y)++"' -h '"++(show h)++"' -w '"++(show w)++"' -ta '"++ta++"' -fg '"++fg++"' -bg '"++bg++"' -fn '"++font++"'"
conky script = "conky -qc "++script
pipe a b = a++" | "++b
trayer edge align w h tint alpha = "trayer --edge "++edge++" --align "++align++" --widthtype pixel --width "++show w++" --height "++show h++" --expand false --tint 0x"++tail tint++" --transparent true --alpha "++show alpha++"&"

myMusicBarStdWidth = 350
myStatBarWidth = 450
myDzenBarHeight = 25
myDzenBarOverlap = 5

-- the width of the xmonad bar and the status bar would be determined dynamically during boot time
-- this method will return the log bar instance for xmonad to pipe the output to
myStatusBars = do
    w <- fmap (read . head . lines) $ runProcessWithInput "screen-res" ["width"] ""
    myXMonadDir <- io getMyXMonadDir
    let xbarx = 0
        xbarw = statbarx + myDzenBarOverlap
        statbarw = w / 3
        statbarx = w - statbarw
        myDzenBar x w a = dzenBar x 0 w myDzenBarHeight a myFgColor myBgColor myDzenFont
        myLogBar = myDzenBar xbarx xbarw "l"
        myStatBar = pipe (conky $ myXMonadDir++"/.conky_dzen") (myDzenBar statbarx statbarw "r") 
    handle <- spawnPipe myLogBar
    spawn myStatBar
    return handle

myManageHook = composeAll [ 
      isFullscreen --> doFullFloat  
    -- gimp related stuff
    , ((propertyToQuery (Role "gimp-toolbox")) <||> (propertyToQuery (Role "gimp-preferences")) <||> (propertyToQuery (Role "gimp-message-dialog")) <||> (propertyToQuery (Role "gimp-dock"))) --> doFloat
    ]

otherCommands = 
    [ ("<F10>", io getMyScriptsDir >>= \myScriptsDir -> spawn $ "amixer get Master | fgrep '[on]' && amixer set Master mute || amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F11>", io getMyScriptsDir >>= \myScriptsDir -> spawn $ "amixer set Master 5-; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F12>", io getMyScriptsDir >>= \myScriptsDir -> spawn $ "amixer set Master 5+; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    ]

main = do
    dzenLogBar <- myStatusBars
    config <- viminize myTabsTheme myXPConfig myVimStatusTheme dzenLogBar taskGroups otherCommands $ 
            ewmh $ U.withUrgencyHook U.NoUrgencyHook $ def {
                  manageHook = myManageHook 
                , logHook = takeTopFocus
                , modMask = myModMask
                , focusFollowsMouse = False
                , borderWidth = 2
                , normalBorderColor = myBgColor
                , focusedBorderColor = color6
            }   
    xmonad config
