{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, ImpredicativeTypes #-}
---------------- Import statements-- {{{
import Prelude hiding (mapM)
import Control.Concurrent (threadDelay)
import Data.Char
import Data.IORef
import Data.List 
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Monoid (mempty, All(..), appEndo)
import Data.Traversable
import Control.Monad hiding (mapM)
import Control.Exception as E
import Foreign.C.Types (CLong)
import System.IO
import System.Exit
import System.Directory
import System.Time
import Text.Read
import XMonad hiding (spawn)
import XMonad.ManageHook
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import qualified XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.GroupNavigation
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.TagWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import XMonad.Actions.WindowBringer
import XMonad.Actions.Plane
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FloatNext(runLogHook)
import qualified XMonad.Hooks.UrgencyHook as U
import XMonad.Hooks.DynamicHooks
import XMonad.Layout.Fullscreen
{-import XMonad.Layout.LayoutCombinators-}
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Decoration
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.TiledTabs
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Groups.Helpers
{-import XMonad.Layout.Groups.Examples-}
import XMonad.Layout.ZoomRow
import XMonad.Operations
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FMCPrompt
import XMonad.Prompt.Shell
import XMonad.Prompt.TaskPrompt
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Image
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Stack
import XMonad.Util.Themes
import XMonad.Util.Timer
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DebugKeyEvents
import XMonad.Layout.ImageButtonDecoration
import XMonad.Hooks.XPropManage
import XMonad.Hooks.ICCCMFocus
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceHandles
import XMonad.Util.WorkspaceDirectories
import XMonad.Actions.FloatKeys
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import qualified Data.Map as M
import qualified Data.Set as S
import qualified XMonad.Layout.Groups as G
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.User
import System.Posix.Files
import Text.Regex.Posix
import Control.Applicative
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import Codec.Binary.UTF8.String
import System.Posix.Process
import System.Process (runInteractiveProcess)
-- }}}

-- redefinition
spawn s = xfork (executeFile "/bin/sh" False ["-c", s] Nothing) >> return ()

runProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
runProcessWithInput cmd args input = io $ do
    (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output == output) $ return ()
    hClose pout
    hClose perr
    -- no need to waitForProcess, we ignore SIGCHLD
    return output

---------------- Constants-- {{{

---- ModMask
myModMask = mod4Mask

---- Directories
myXMonadDir     = "/home/lingnan/.xmonad"
myBitmapsDir    = myXMonadDir++"/dzen2_retina"
myScriptsDir    = myXMonadDir++"/scripts"

---- Font
myTerminalFont          = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
{-myFont= "-isas-song ti-medium-r-*-*-*-*-*-*-*-*-iso8859-*"-}
myFont= "xft:WenQuanYi Zen Hei Mono:pixelsize=18"
myBigFont= "xft:WenQuanYi Zen Hei Mono:pixelsize=20"
myDzenFont= "WenQuanYi Zen Hei Mono:pixelsize=17"

---- Color constants
{-background= "#181512"-}
{-foreground= "#bea492"-}

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

---- Tabscheme
selectIcon' = [[1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1],
               [0,0,1,1,1,1,1,1,1,1],
               [0,0,0,1,1,1,1,1,1,1],
               [0,0,0,0,1,1,1,1,1,1],
               [0,0,0,0,0,1,1,1,1,1],
               [0,0,0,0,0,0,1,1,1,1],
               [0,0,0,0,0,0,0,1,1,1],
               [0,0,0,0,0,0,0,0,1,1],
               [0,0,0,0,0,0,0,0,0,1]]
selectIcon = convertToBool selectIcon'
visualIcon' = [[1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1]]
visualIcon = convertToBool visualIcon'

robotTheme = def { activeColor         = myBgHLight
                 , inactiveColor       = myBgDimLight
                 , activeBorderColor   = myBorderHLight
                 , inactiveBorderColor = myBorderColor
                 , activeTextColor     = myFgHLight
                 , inactiveTextColor   = myFgColor
                 , fontName            = myFont
                 , decoHeight          = 25 
                 , subThemeForWindow   = \w -> do
                        -- get the group stack
                        mgs <- G.getCurrentGStack
                        tg <- fmap (maybe mySubTheme colorScheme) $ taskGroupOfWindow taskGroups w 
                        -- we will try to produce a number for each tab (easy for keyboard shortcut and referencing stuff)
                        selected <- windowIsSelected w
                        inv <- isInVisualMode
                        -- read the tags for the given window
                        tags <- getTags w
                        let wta = case mgs of
                                        Just gs -> case fetchTabNo w gs of
                                                        Just i -> case (subgroupIndexToSymbol i, tags) of
                                                                       (Nothing, []) -> []
                                                                       (sm, tags) -> [("["++fromMaybe "" sm++(concatMap ("'"++) tags)++"] ", AlignLeft)]
                                                        _ -> []
                                        _ -> []
                            icons = case (selected, inv) of
                                         (True, True) -> [(visualIcon, CenterRight 3)]
                                         (True, False) -> [(selectIcon, CenterRight 3)]
                                         _ -> []
                        return $ Just $ tg {
                                  winTitleAddons = wta
                                , winTitleIcons = icons
                        }
                 }

{-myTabsTheme = robotTheme { subThemeForWindow = \_ -> return Nothing }-}
myTabsTheme = robotTheme
mySubTheme = SubTheme { winInactiveColor = inactiveColor myTabsTheme
                      , winInactiveBorderColor = inactiveBorderColor myTabsTheme
                      , winInactiveTextColor = inactiveTextColor myTabsTheme
                      , winActiveColor = activeColor myTabsTheme
                      , winActiveBorderColor = activeBorderColor myTabsTheme
                      , winActiveTextColor = activeTextColor myTabsTheme
                      , winTitleAddons = []
                      , winTitleIcons = []
                      }

---- Promptscheme
myXPKeymap = emacsLikeXPKeymap' (\c -> not (isAlphaNum c) && c /= '_')
myXPConfig ref = def { 
    font = myBigFont
    , bgColor               = myBgDimLight
    , fgColor               = myFgColor
    , bgHLight              = myBgHLight
    , fgHLight              = myFgHLight
    , promptBorderWidth     = 0
    , height                = decoHeight myTabsTheme
    , historyFilter         = deleteConsecutive
    , autoComplete       = Nothing
    , alwaysHighlight = False
    , searchPredicate = stdSearchPredicate
    , promptKeymap = myXPKeymap ref
}

myXPConfigWithQuitKey key r = (myXPConfig r) {
    promptKeymap = M.fromList $ (M.toList $ myXPKeymap r)++[
        (key, quit)
    ]
}

myInfoXPConfig ref = (myXPConfig ref) {
    {-fgColor = myFgHLight-}
    {-, bgColor = myBgHLight-}
    fgColor = "#d6c3b6"
    , bgColor = "#262729"
    , fgHLight = "#d6c3b6"
    , bgHLight = "#262729"
    , alwaysHighlight = False
}

--- Status bar 

dzenBar x y w h ta fg bg font = "dzen2 -x '"++(show x)++"' -y '"++(show y)++"' -h '"++(show h)++"' -w '"++(show w)++"' -ta '"++ta++"' -fg '"++fg++"' -bg '"++bg++"' -fn '"++font++"'"
conky script = "conky -qc '"++script++"'"
pipe a b = a++" | "++b
trayer edge align w h tint alpha = "trayer --edge "++edge++" --align "++align++" --widthtype pixel --width "++show w++" --height "++show h++" --expand false --tint 0x"++tail tint++" --transparent true --alpha "++show alpha++"&"

myMusicBarStdWidth = 350
myStatBarWidth = 450
myDzenBarHeight = 25
myDzenBarOverlap = 5


---- Terminal settings
myTerminal = "xterm"

-- a relaxed version of uniqueTermHasCmd that also includes terminals that are not instances of UniqueTerm
isTerm = className =? "XTerm" 
hasCmd c = fmap (any (c `isInfixOf`)) $ getQuery wM_COMMAND
isTermWithCmd c = isTerm <&&> hasCmd c
-- note that args, cmd are NOT quoted; this is for maximum flexibility - you can then use parameter expansion, etc. if you'd like
termFullCmd title name args cmd = myTerminal++prop "title" title++prop "name" name++" "++args++" -e "++cmd
    where prop s v = if null v then "" else " -"++s++" " ++escapeQuery v
-- runTerm will ALWAYS start the program with the current directory
runTerm title appname cmd = setCurrentWorkspaceDirectory >> spawn (termFullCmd title appname "" cmd)
runShell cmd = setCurrentWorkspaceDirectory >> spawn cmd

-- we give up on a dynamically typed UniqueTerm (depending on the type of Context) because it might be easy to query for command only if you don't know what type of the context is
data UniqueTerm = UniqueTerm { context :: S.Set String
                             , command :: String 
                             , args    :: String
                             } deriving (Show, Read, Eq)

uniqueTerm cons = UniqueTerm (S.fromList cons)

uniqueTermAppName :: UniqueTerm -> String
uniqueTermAppName = show

uniqueTermFullCmd ut = termFullCmd (command ut) (uniqueTermAppName ut) (args ut) (command ut)

-- return the uniqueTerm from the specified app name
toUniqueTerm :: String -> Maybe UniqueTerm
toUniqueTerm = readMaybe 

-- useful queries
uniqueTermContext = fmap (fmap context . toUniqueTerm) appName 
uniqueTermCmd = fmap (fmap command . toUniqueTerm) appName 

-- the idea of having the context to check against membership is to have some sort of polymorphism
---- example: a perWorkspaceScratchpad is also a scratchpad; therefore a window spawn in a perWorkspaceScratchpad context will contain the contexts for both applications
uniqueTermHasContext con = fmap (maybe False (S.member con)) uniqueTermContext 
uniqueTermHasContexts cons = fmap (maybe False (== cons)) uniqueTermContext
uniqueTermHasCmd cmd = fmap (maybe False (== cmd)) uniqueTermCmd

isUniqueTerm ut =  appName =? (uniqueTermAppName ut)

runUniqueTerm ut = setCurrentWorkspaceDirectory >> spawn (uniqueTermFullCmd ut)

alwaysTrue = liftX $ return True
alwaysFalse = liftX $ return False
-- query bool definition for recyclable terminals
isRecyclableTerm = className =? "XTerm" <&&> appName =? "xterm" <&&> ( title =? "zsh -i" <||> do
    mach <- stringProperty "WM_CLIENT_MACHINE" 
    user <- io $ getLoginName
    fmap (isPrefixOf $ user++"@"++mach++":") title )

-- query bool defintion to matching windows in the current workspace
isInCurrentWorkspace = ask >>= \w -> liftX $ do
    ws <- gets windowset
    return $ w `elem` W.index ws

-- query bool defintion for matching a property same as the current window
hasSamePropertyAsFocused qry = ask >>= \w -> liftX $ do
    ws <- gets windowset 
    case W.peek ws of
         Just f ->  do
             pf <- runQuery qry f 
             pw <- runQuery qry w
             return $ pf == pw
         Nothing -> return False
    
-- Context settings
scratchpadContext = "scratchpad"
singletonContext = "global"
perWSScratchpadContextKey = "perWorkspaceScratchpad"
perWSScratchpadContext tag = getWorkspaceHandle tag >>= \h -> return $ S.fromList [scratchpadContext, perWSScratchpadContextKey, h]

-- scratchpad settings
scratchpadWorkspaceTag = "NSP"
-- queries concerning the scratchpad context
isScratchpad = uniqueTermHasContext scratchpadContext
isPerWSScratchpad = uniqueTermHasContext perWSScratchpadContextKey
isPerWSScratchpadBoundToWS tag = isPerWSScratchpad <&&> (liftX (perWSScratchpadContext tag) >>= uniqueTermHasContexts)
isPerWSScratchpadBoundToCurrentWS = ask >>= \w -> liftX $ gets (W.currentTag . windowset) >>= \t -> runQuery (isPerWSScratchpadBoundToWS t) w

-- Unique and global term instances
-- uniqueMutt is used in the scratchpad application
uniqueMutt = uniqueTerm [scratchpadContext] "loader mutt" ""
-- uniqueFinch lives in comm workspace
uniqueFinch = uniqueTerm [singletonContext] "loader finch" "-xrm 'XTerm*metaSendsEscape: true'"
uniqueWeechat = uniqueTerm [singletonContext] "loader weechat-curses -r '/redraw'" ""

--

---- Workspace sorting
-- The temp buffer is the default workspace and will never be recycled (make sure that it should proceed other buffers in the sequence
tmpWorkspaceTag = "`"
quickWorkspaceTagEnd = "0"
quickWorkspaceTags = map (\c -> [c]) $ takeWhile ((>=) ei. fromSymbol) symbolStream
    where ei = fromSymbol $ head quickWorkspaceTagEnd
-- The symbolSequence replaces the originalSequence in the locale, thus enabling custom sorting
originalSequence = "-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
symbolSequence = "./46-=`12357890:;<>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
-- the workspace tags are implemented as a sliding window across the symbol stream; at any instant the tags are a list counting from a specific symbol
symbolStream = symbolFrom $ head tmpWorkspaceTag
-- the starting symbol SHOULD be the tmpWorkspaceTag
validSymbol = (<=) (fromSymbol $ head symbolSequence) . fromSymbol

--- subgroup symbol sequence: we eliminated '`' due to its ugliness from the subgroup indexing stream; and note that the sub group doesn't necessarily need an index after all
subgroupSymbolSequence = "1234567890-="
subgroupIndexToSymbol n = if n >= 0 && n < length subgroupSymbolSequence then Just [subgroupSymbolSequence !! n] else Nothing

-- validWS is the workspace that are considered open to user (not including "NSP" in the obvious sense)
validWS = (/= scratchpadWorkspaceTag) . W.tag 
validWSType = WSIs $ return validWS

-- validWindow is the counterpart to validWS -- any validWindow is a window that is in a validWS; this is implemented as a managehook to put into many conditions
validWindow = ask >>= \w -> liftX $ do
    iws <- invalidWorkspaces
    return $ not $ any (containsWin w) iws
        where containsWin w ws = case W.stack ws of
                   Just s -> w `elem` (W.focus s) : ((W.up s) ++ (W.down s))
                   Nothing -> False

myWorkspaceSort = getSymbolStringSort

---- Prompt autoCompletion matching
-- useful for window prompts , in which all workspace windows are shown
complFilteredPredicate p fun cmd compl = fun cmd compl && p compl

stdSearchPredicate cmd cpl = isInfixOf (map toLower cmd) (map toLower cpl) 

prefixSearchPredicate cmd cpl = isPrefixOf (map toLower cmd) (map toLower cpl)

-- an implementation for searchPredicate that will make sure all words typed are contained in the result
repeatedGrep cmd cpl = all (`isInfixOf` lp) (words lc) 
    where lc = map toLower cmd
          lp = map toLower cpl

---- Wallpaper Changer constants
changeWallpaperBin = "wallpaper-change"
wallpaperDirectory = "/home/lingnan/Pictures/wallpapers/"
changeWallpaperCmd = changeWallpaperBin
wallpaperOpenBin = "feh"
-- wallpaperChangeInterval = 900
-- wallpaperRecoverInterval = 1.0
-- wallpaperFullRecoverInterval = 1.5
-- wallpaperFadeInactiveAmount = 0.3
defaultFadeInactiveAmount = 0.85
wallpaperFadeFullAmount = 0.15

---- Vimb prompt constants
vbhistorySize = 10
vbMatchSize = 10

-- }}}

---------------- StartupHook -- {{{
myStartupHook = do
    {-setWMName "LG3D"-}
    {-wallpaperAutomaticStartHook-}
    -- initiate the wallpaper
    -- check if we have any ch
    d <- getWPDirectory
    spawn $ changeWallpaperCmd ++ " " ++ escapeQuery d
    -- we will only need to switch to the right workspace during startup
    curr <- currWorkspace
    if not (validWS curr) then startTimer 0.5 >>= XS.put . StartWSChangeState else return ()
-- }}}

---------------- RestartCmd -- {{{
myKillCmd = "killall dzen2 conky fmclrc trayer;"
myRestartCmd = myKillCmd ++ "xmonad --restart"
-- }}}

---------------- LayoutHook -- {{{

-- copy and paste of GroupEQ
data GroupEQ a = GroupEQ
  deriving (Show, Read)

instance Eq a => EQF GroupEQ (G.Group l a) where
    eq _ (G.G l1 _) (G.G l2 _) = G.sameID l1 l2

rectTabs = G.group3 (renamed [CutWordsRight 1] $ addTabs shrinkText myTabsTheme Simplest) (Mirror (zoomRowWith GroupEQ) ||| Full) (zoomRowWith GroupEQ ||| Full)

-- adding lessBorders Screen will avoid drawing borders for doFullFloat
myLayout = minimize . noBorders . avoidStruts $ lessBorders Screen rectTabs

-- corrected focus function that unminimize the given window
deminimizeFocus w = deminimize w >> focus w
deminimize w = routeMessageToWS ((w `elem`) . W.integrate' . W.stack) (RestoreMinimizedWin w)
routeMessageToWS fun mess = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    case find fun wss of
         Just ws
            | W.tag ws /= curr -> sendMessageWithNoRefresh mess ws 
            | otherwise -> sendMessage mess
         _ -> return ()
minimizeWindows ls = deleteWindowsSelection ls >> correctFocus (mapM_ minimizeWindow) ls
minimizeWindows' tag ls = do
    correctFocus (mapM_ (\w -> routeMessageToWS ((==tag).W.tag) (MinimizeWin w))) ls
    -- clear the focus if necessary

restoreMinimizedWindowsAndSelect' [] = return ()
restoreMinimizedWindowsAndSelect' wins = markWindowsSelection wins >> mapM_ deminimizeFocus wins 
restoreMinimizedWindowsAndSelect = getCurrentMinimizedWindows >>= restoreMinimizedWindowsAndSelect'
restoreMinimizedWindowAndSelect = getCurrentMinimizedWindows >>= restoreMinimizedWindowsAndSelect' . wrapList . head


isMinimized = ask >>= \w -> liftX $ do
    wss <- allWorkspaces
    wins <- mapM getMinimizedWindows $ fmap W.layout wss
    return $ w `elem` (concat $ wins)

    {-where tallTiledTabs = G.group (addTabs shrinkText myTabsTheme Simplest) $ -}
              {-GridRatio (4/3) ||| -}
              {-multiCol [1] 2 (3/100) (-0.5) ||| -}
              {-ThreeCol 1 (3/100) (1/2) ||| -}
              {-ThreeColMid 1 (3/100) (1/2) ||| -}
              {-Tall (1) (3/100) (1/2) |||-}
              {-Full-}
              {-vert = named "Vertical" $ Tall (1) (3/100) (1/2)-}

{-data ReorderGroups a = ReorderGroups deriving (Read, Show)-}

{-instance LayoutModifier ReorderGroups Window where-}
    {-redoLayout m _ _ wrs = do-}
           {-mgs <- G.getCurrentGStack-}
           {-let place = case fmap G.flattened mgs of -}
                             {-Just ls -> sortBy (\(w1,_) (w2,_) -> compare (elemIndex w1 ls) (elemIndex w2 ls))-}
                             {-_ -> id-}
           {-oldStack <- gets $ W.stack . W.workspace . W.current . windowset-}
           {-let ns = case mgs of -}
                        {-Just gs -> G.toZipper gs -}
                        {-_ -> oldStack-}
           {-setStack ns-}
           {-return (place wrs, Just m)-}


joinStr delimit [] = ""
joinStr delimit ls = foldl' ((++) . (++delimit)) (head ls) (tail ls)

moveGroupStackToWorkspace t = do
    -- if the t is the current stack then return
    curr <- gets (W.currentTag . windowset)
    if curr == t 
       then return ()
       else do
           mgs <- G.getCurrentGStack
           let findws tag wins = fmap W.workspace $ find ((tag==).W.tag.W.workspace) (W.visible wins)
           case mgs of
                Just gs -> do
                    qt <- quickWorkspace t
                    wss <- allWorkspaces
                    mtogs <- G.getGStackForWSTag qt
                    case (find ((==qt) . W.tag) wss, G.current gs, mtogs) of
                         (Just ws, Just gm, Just togs) -> do
                             -- the current problem is that Tabbed might draw tabs incorrectly when all windows in a group disappear, and that's reason for doing the transformation together with update at each and every single time
                             let shiftg (G.Leaf (Just s)) = do
                                    windows $ \ws -> foldr (W.shiftWin qt) ws $ tail ls
                                    windows $ W.shiftWin qt $ head ls
                                        where ls = W.integrate s
                                 shiftg (G.Node s) = mapM_ shiftg $ W.integrate s
                                 shiftg _ = return ()
                             shiftg gm
                             {-G.applyGStackForWSTag qt $ G.insertGUp gm togs-}
                             {-spawn $ "echo '"++ld++"' > ~/.xmonad/xmonad.test"-}
                             {-flip sendMessageWithNoRefresh ws $ G.Modify $ \ls mg -> fmap (\g -> G.insertGUp (G.fromZipper (W.differentiate ls) (G.level g)) g) mg-}
                         _ -> return ()
                _ -> return ()

toggleGroup = nextMatch History toggleGroupPredicate
toggleGroupPredicate = fmap not isInCurrentGroup <&&> isInCurrentWorkspace <&&> fmap not isMinimized
toggleWithinGroup = nextMatch History toggleWithinGroupPredicate
toggleWithinGroupPredicate = isInCurrentGroup

isInCurrentGroup = ask >>= \w -> liftX $ do
    gs <- G.getCurrentGStack
    return $ maybe False (elem w . maybe [] G.flattened. G.current) gs

-- sort the current group stacks and return the result as a GroupStacks format
-- to make it less complicated we've opted to only work until the first level -- beyond that it just becomes sheer madness
sortGroupStacks wgs = withFocused $ \f -> do
    mgs <- G.getCurrentGStack
    case mgs of
         Just gs@(G.Node gss) -> do
                taggs <- taggedGStack taskGroups gs
                -- determine by the percentage of task groups in each big group
                let fgs = case break (G.contains f) $ fmap (process . fst . unzip) $ groupBy (\(_,a) (_,b)->a==b) $ sortBy (\(_,a) (_,b)->compare a b) $ G.flattened taggs of
                               (bs,fs:as) -> Just $ mknode fs (bs++as)
                               ([],_) -> Nothing
                               (bs,_) -> Just $ mknode (head bs) (tail bs)
                    mknode fs os = let (bs,as) = splitAt initi os
                                       in G.Node $ W.Stack fs (reverse bs) as
                    process ws = case break (==f) ws of
                                     (bs, nf:as) -> G.Leaf $ Just $ W.Stack nf (reverse bs) as
                                     (bs, _) -> G.Leaf $ W.differentiate bs
                    initi = fromMaybe 0 $ findIndex (G.contains f) $ W.integrate gss
                if isJust fgs then
                              G.applyGStack (fromJust fgs)
                              else return ()
         _ -> return ()

-- tabbar related settings
fetchTabNo' w (h:t)
    | isJust r = r
    | otherwise = fetchTabNo' w t
        where r = fetchTabNo w h
fetchTabNo' w [] = Nothing

fetchTabNo w (G.Node s) = fetchTabNo' w $ W.integrate s
fetchTabNo w (G.Leaf ms) = elemIndex w $ W.integrate' ms

getBaseCurrentStack :: X (Maybe (W.Stack Window))
getBaseCurrentStack = do
    ins <- focusIsInGStack
    mgs <- G.getCurrentGStack 
    case (ins, mgs) of
        (True, Just gs) -> return $ G.toZipper $ G.baseCurrent gs
        _ -> return Nothing

focusIsInGStack = gets (W.peek . windowset) >>= maybe (return False) (fmap not . runQuery isFloating)
getBaseCurrentWindows = getBaseCurrentStack >>= return . W.integrate'
-- the focused stack is the base current stack, unless the current focus is on a floating window, in that case it is the stack formed by that floating group
getFocusStack = do
    mbs <- getBaseCurrentStack
    mf <- gets (W.peek . windowset)
    (_, _, _, _, float, _, _) <- getSides
    case (mbs, mf, break ((==mf) . Just) float) of
         (Just (W.Stack sfw u d), Just fw, _) | sfw == fw -> return mbs
         (_, _, (fl,f:fr)) -> return $ Just $ W.Stack f (reverse fl) fr
         _ -> return Nothing

-- the selection stack should be 
-- 1. the actively selected elements, if any  
-- 2. the passively selected elements in the basecurrent group, if any
-- 3. the current window
-- note that when the focus is on a float window it would always be case 3


-- should we only sort the current focused group? But on the other hand the
-- user might not want to manually sort each and every group
sortWindowsByTitle (G.Node (W.Stack f u d)) = do
    f' <- sortWindowsByTitle f
    u' <- mapM sortWindowsByTitle u
    d' <- mapM sortWindowsByTitle d
    return $ G.Node (W.Stack f' u' d')
sortWindowsByTitle s@(G.Leaf Nothing) = return s
sortWindowsByTitle (G.Leaf (Just s@(W.Stack f u d))) = do
    -- first get all the titles of the windows
    let wins = W.integrate s
    titles <- mapM (runQuery title) wins
    let fwins = fmap snd $ sortBy (\(a,_) (b,_) -> compare a b) $ zip titles wins
    return $ case break (==f) fwins of
                 (bs, nf:as) -> G.Leaf $ Just $ W.Stack nf (reverse bs) as
                 (bs, _) -> G.Leaf $ W.differentiate bs

-- sort the windows in the innermost group according to the title
sortWindowsWithinGroupStacks = do
    mgs <- G.getCurrentGStack
    case mgs of
         Just gs@(G.Node gss) -> do
             fgs <- sortWindowsByTitle gs
             G.applyGStack fgs
         _ -> return ()


-- sticky sorting function that stores the sticky information in a map
data AutoArrangeWorkspaces = AutoArrangeWorkspaces (M.Map String Bool) deriving (Typeable, Read, Show)
instance ExtensionClass AutoArrangeWorkspaces where
    initialValue = AutoArrangeWorkspaces M.empty
    extensionType = PersistentExtension

isAutoArrangeWorkspace h = do
    AutoArrangeWorkspaces m <- XS.get
    return $ fromMaybe False $ M.lookup h m

toggleAutoArrangeWorkspace h = do
    AutoArrangeWorkspaces m <- XS.get
    XS.put $ AutoArrangeWorkspaces $ M.insert h (not $ fromMaybe False $ M.lookup h m) m
    runLogHook

data AutoArrangeWorkspaceFlag = AutoArrangeWorkspaceFlag (Maybe G.GStack) deriving (Typeable, Read, Show)
instance ExtensionClass AutoArrangeWorkspaceFlag where
    initialValue = AutoArrangeWorkspaceFlag Nothing
    extensionType = PersistentExtension


autoTaskFocusedGStack _ s@(G.Leaf _) = s
autoTaskFocusedGStack tg (G.Node s@(W.Stack f u d))
    | n==0 = G.Node $ W.Stack (G.fromZipper emptyZ (G.level f)) u (f:d)
    | otherwise = G.Node $ W.Stack (autoTaskFocusedGStack tg nf) (reverse bef) aft
    -- determine by percentage 
    where countPercent ls = let (_, ts) = unzip ls 
                                nmat = length $ filter (==tg) ts
                            in ((fromIntegral nmat) / (fromIntegral $ length ts), nmat)
          l = W.integrate s
          (idx,(n,p)) = maximumBy (\(_,a) (_,b) -> compare a b) $ zip [0..] $ fmap (countPercent . G.flattened) l
          (bef, nf:aft) = splitAt idx l

autoArrangeHook wgs = fmap not isDialog --> ask >>= \w -> liftX $ do
    h <- getCurrentWorkspaceHandle
    isaa <- isAutoArrangeWorkspace h
    mgs <- G.getCurrentGStack
    case (isaa, mgs) of
         (True, Just gs) -> do
                tgs <- taggedGStack taskGroups gs
                t <- siftedTaskGroupOfWindow taskGroups w
                let ngs = G.insertWUp w $ fmap fst $ autoTaskFocusedGStack t tgs
                    trans ls _ = let rls = filter (/=w) ls in
                                      Just $ foldr G.insertWUp ngs rls
                -- send the new window handling function to the current layout
                sendMessage $ G.GroupsManage trans
                -- dig into the second layer of the trans
                -- the thing gets really complicated after the first level... better stops here
                mempty
         _ -> mempty

-- provding a more correct implementation of kill systems (refocusing on the window instead!
focusNextKillWindow w = focusNextKillWindows [w]
focusNextKillWindows ls = do
    deleteWindowsSelection ls
    -- now just kill all windows that 
    correctFocus (mapM_ killWindow) ls

nextFocus (G.Leaf (Just (W.Stack _ u (d:_)))) = Just d
nextFocus (G.Leaf (Just (W.Stack _ (u:_) _))) = Just u
nextFocus (G.Leaf _) = Nothing
nextFocus (G.Node (W.Stack f u d)) = 
    case (nextFocus f, u, d) of
         (Just r,_,_) -> Just r
         (Nothing,_,d:_) -> G.focal d
         (Nothing,u:_,_) -> G.focal u
         _ -> Nothing

--- correct the focus of the windows after applying the action 'a' to a list of windows
--- the rule is that the windows are supposed to 'disappear' from the stack after the action is applied
correctFocus a [] = a []
correctFocus a wins = do
    -- first extract the focus
    mf <- gets (W.peek . windowset)
    case mf of
         Just f -> do
            isf <- runQuery isFloating f
            mgs <- G.getCurrentGStack
            a wins
            case (isf, f `elem` wins, mgs) of
                 (True, True, _) -> nextMatch History $ isInCurrentWorkspace <&&> fmap not isFloating
                 (False, True, Just gs) -> let correctf = fmap G.focal $ G.filter (not . (`elem` wins)) gs
                                           in withFocused $ \nf ->
                                               case correctf of
                                                    Just (Just nw) | nw == nf -> return ()
                                                                   | otherwise -> focus nw
                                                    _ -> return ()
                 _ -> return ()
         _ -> a wins

-- this is conceptually equivalent to navigating back to the last window that's not on the same layer
switchFocusFloat = withFocused $ \f -> do
    isf <- runQuery isFloating f
    nextMatch History $ isInCurrentWorkspace <&&> fmap (if isf then not else id) isFloating
-- }}}

---------------- LogHook -- {{{
myLogHook toggleFadeSet dzenLogBar = do
    -- for java 
    takeTopFocus
    statusLogHook dzenLogBar
    historyHook
    fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount

---- fade Log hook-- {{{
------- the conditions for fading windows in normal circumstances
-- not used currently
isFloating :: Query Bool
isFloating =  ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset
-- windows that shall not be faded no matter what
doNotFadeOutWindows =  className =? "xine" <||> className =? "MPlayer"

-- windows that shall not be faded given the class name
-- disableFadingWithinClassName cn = liftM not (className =? cn <&&> (liftX  $ focusedHasProperty (ClassName cn)))
-- disableFadingWithinClassNames = disableFadingWithinClassName "jetbrains-idea"
   
-- tests if a window should be faded; floats are the windows that have been toggled by the user to not fade
defaultFadeTest floats =
    -- liftM not doNotFadeOutWindows <&&> isUnfocused <&&> disableFadingWithinClassNames <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)
    liftM not doNotFadeOutWindows <&&> fmap not isFocused <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

isFocused = ask >>= \w -> liftX $ do
    mf <- gets (W.peek . windowset)
    return $ mf == Just w
     
-- toggles whether the given window should be faded out (by toggling its reference in a set
toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s 
    | w `S.member` s = S.delete w s
    | otherwise = S.insert w s

-- }}}

----- status bar stuff-- {{{

-- pretty printing for history stack status
printLayoutInfo = do
    isAuto <- getCurrentWorkspaceHandle >>= isAutoArrangeWorkspace 
    s <- getMarkedWindowsSize
    InsertOlderToggle t <- XS.get
    XMS mode <- XS.get
    let (plus, c) = if s == 0 then (" ", myFgColor) else ("+", myNotifyColor)
        [lb,rb] = fmap (dzenColor myFgColor myBgColor) $ ["[","]"]
        autoIndicator = if isAuto then dzenColor myNotifyColor myBgColor "*" else "#"
        insertOlderIndicator = if t then dzenColor myNotifyColor myBgColor "|" else "<"
        modeIndicator = case mode of
                             Visual Win -> dzenColor myNotifyColor myBgColor "VW" 
                             Visual Row -> dzenColor myNotifyColor myBgColor "VR" 
                             Visual Col -> dzenColor myNotifyColor myBgColor "VC" 
                             _ -> "Nm"
    return $ Just $ lb++autoIndicator++insertOlderIndicator++modeIndicator++"-"++dzenColor c myBgColor plus++rb

-- pretty printing for groupNames
printGroupNames [] = ""
printGroupNames gns = joinStr " " $ fmap gn gns
    where gn tg = case tg of
                      Right n -> dzenColor myTextHLight myBgColor $ pad n
                      Left n  -> dzenColor myFgColor myBgColor $ pad n

showFilled = do
    wss <- allWorkspaces 
    return $ \name -> 
        let tag = takeWhile (/= ':') name in
        -- check if the workspace is filled with some windows
        if tag == name 
           then case find ((== tag) . W.tag) wss of
                    Just ws -> if isJust $ W.stack ws then name++":(filled)" else name
                    Nothing -> name
           else name

colorTaskGroupName spc nc bg g
    | not $ null (filterKey g) = splitFst (filterKeyList $ filterKey g) spc nc (taskGroupName g)
    | otherwise =  dzenColor nc bg (taskGroupName g) 
        where splitFst cs spc nc str = if null cs then dzenColor nc bg str 
                                                  else let h=head cs in 
                                                           case break (\ch -> toLower ch == h || toUpper ch == h) str of
                                                               (a, x:xs) -> (dzenColor nc bg a)++(dzenColor spc bg [h])++(splitFst (tail cs) spc nc xs)
                                                               _ -> dzenColor nc bg str

-- a tentative printing function for a given GStack
-- we'll first convert the gstack to a stack of strings according to the task group within each leaf, and then we can easily print it using another function
-- return the stack tagged with task groups
taggedGStack:: [TaskGroup] -> G.MultiStack Window -> X (G.MultiStack (Window, Maybe TaskGroup))
taggedGStack wgs = mapM (\w -> siftedTaskGroupOfWindow wgs w >>= \r -> return (w, r))

-- return a (MultiStack String) which contains all the human readable information regarding the task groups of the windows
infoFromTaggedGStack:: G.MultiStack (Window, Maybe TaskGroup) -> String
infoFromTaggedGStack ms@(G.Node (W.Stack f u d)) = 
    -- for the first level the wrap pairs are ("1.", "") ("2.", "") and so on
    let wl = zip (fmap ((++".") . wrapList) subgroupSymbolSequence ++ repeat "") $ repeat ""
        nl = G.level ms
    -- need to get the number of levels inside the struct
    in infoFromTaggedGStack' ("  ":repeat "") ("","") (wl:(repeat $ repeat ("[","]"))) (myNotifyColor, "/") (take (nl-1) (repeat (myFgColor, myFgColor)) ++ [(myTextHLight, myFgColor)]) myBgColor ms
infoFromTaggedGStack _ = ""

-- this takes the highlight color for the focused group, and then a function that transform a group to a string
-- delimit: the delimit string to separate the groups / task groups
-- fkeyColor: the filter key color (only used in the base case)
-- focusHL, fgColor: the focusColor for the current group, the fgColor for other groups
-- bgColor: the background color (should be uniform across)
infoFromTaggedGStack' (delimit:dls) (wrapl,wrapr) (wl:wraps) (fkeyColor,tgdelimit) ((focusHL,fgColor):cps') bgColor ms = 
     wrap (nc wrapl) (nc wrapr) $ case ms of
                                G.Leaf s@(Just (W.Stack (_,ftg) u d)) -> joinStr (nc tgdelimit) $ fmap (\g -> colorTaskGroupName fkeyColor (if g == ftg then focusHL else fgColor) bgColor (fromMaybe def g)) $ nub $ snd $ unzip $ W.integrate' s
                                G.Leaf Nothing -> ""
                                G.Node s@(W.Stack fs us dss) -> 
                                    let ncps m
                                            | m==fs = fmap (\(hl, fg)->(hl, focusHL)) cps'
                                            | otherwise = fmap (\(hl, fg)->(fgColor, fg)) cps'
                                        in joinStr (nc delimit) $ fmap (\(wp, mn) ->infoFromTaggedGStack' dls wp wraps (fkeyColor,tgdelimit) (ncps mn) bgColor mn) $ zip wl $ W.integrate s
        where nc = dzenColor fgColor bgColor

workspaceNamesPP pp = do
    names <- getWorkspaceNames
    return $
        pp {
            ppCurrent         = ppCurrent         pp . names,
            ppVisible         = ppVisible         pp . names,
            ppHidden          = ppHidden          pp . names,
            ppHiddenNoWindows = ppHiddenNoWindows pp . names,
            ppUrgent          = ppUrgent          pp . names
        }

-- DZEN LOG RULES for workspace names, layout image, current program title
statusLogHook h =  do
    -- first get the description associated with the layout (that would contain the full information regarding the group
    {-ld <- gets (description . W.layout . W.workspace . W.current . windowset)-}
    {-spawn $ "echo '"++ld++"' > ~/.xmonad/xmonad.test"-}
    pp <- workspaceNamesPP $ defaultPP {
          ppCurrent       = dzenColor myTextHLight myBgColor
        , ppVisible     = dzenColor myFgHLight myBgColor
        , ppUrgent      = dzenColor myNotifyColor myBgColor . dzenStrip
        , ppHidden      = dzenColor myFgColor myBgColor
        , ppHiddenNoWindows = dzenColor myFgDimLight myBgColor
        , ppWsSep       = "  "
        , ppSep         = "  "
        , ppLayout      = let iconify w = case w of
                                "Mirror ZoomRow"      ->      "^i(" ++ myBitmapsDir ++ "/grid.xbm)"
                                "ZoomRow"             ->      "^i(" ++ myBitmapsDir ++ "/grid.xbm)"
                                "Full"                ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                "Tabbed"              ->      ""
                                {-_                     ->      "^i(" ++ myBitmapsDir ++ "/grid.xbm)" -}
                                _                     ->      ""
                          in joinStr "" . fmap (iconify . trim) . reverse . splitOn "by"
        , ppTitle       = wrap ("^fg(#222222)^i("++myBitmapsDir++"/corner_left.xbm)^bg(#222222)^fg(#4c7899)^fn(fkp)x^fn()") ("^fg(#222222)^i("++myBitmapsDir++"/corner_right.xbm)") .  dzenColor myFgColor "#222222" . shorten 120 . pad        
        , ppOrder   =  \(ws:l:t:hist:gp:hid:xs) -> [ws,l,hist,hid,gp] ++ xs ++ [t]
        , ppSort        = fmap (.scratchpadFilterOutWorkspace) myWorkspaceSort
        , ppOutput      = hPutStrLn h
        , ppExtras      = [  printLayoutInfo
                           , do
                               mgs <- G.getCurrentGStack
                               {-spawn $ "echo '"++(show mgs)++"' > ~/.xmonad/xmonad.test"-}
                               case mgs of
                                    Just gs -> taggedGStack taskGroups gs >>= return . Just . infoFromTaggedGStack
                                    _ -> return $ Just ""
                           {-return $ Just $ joinStr "  " gns-}
                           , do
                               ls <- getCurrentMinimizedWindows
                               -- test if the current focused window is inside the list, correct that
                               withFocused $ \f -> if f `elem` ls then G.getCurrentGStack >>= maybe (return ()) focus . maybe Nothing G.focal
                                                                  else return ()
                               {-spawn $ "echo '"++(show ls)++"' > ~/.xmonad/xmonad.test"-}
                               tgs <- taggedGStack taskGroups (G.fromZipper (W.differentiate $ reverse ls) 1) 
                               {-spawn $ "echo '"++(show $ infoFromTaggedGStack tgs)++"' > ~/.xmonad/xmonad.test"-}
                               return $ Just $ infoFromTaggedGStack' (repeat "") ("","") (repeat $ repeat ("","")) (myNotifyColor, "|") (repeat (myFgColor, myFgColor)) myBgColor tgs
                           ]
        } 
    dynamicLogWithPP pp

-- the width of the xmonad bar and the status bar would be determined dynamically during boot time
-- this method will return the log bar instance for xmonad to pipe the output to
myStatusBars = do
    w <- fmap (read . head . lines) $ runProcessWithInput "screen-res" ["width"] ""
    let xbarx = 0
        xbarw = statbarx + myDzenBarOverlap
        {-trayerx = w - trayerw-}
        {-trayerw = 20-}
        {-musicx = w * 2/3-}
        {-musicw = statbarx - musicx + myDzenBarOverlap-}
        statbarw = w / 3
        statbarx = w - statbarw
        {-myTrayer = trayer "top" "right" trayerw myDzenBarHeight myBgColor 0-}
        myDzenBar x w a = dzenBar x 0 w myDzenBarHeight a myFgColor myBgColor myDzenFont
        myLogBar = myDzenBar xbarx xbarw "l"
        {-myMusicBar = pipe (conky $ myXMonadDir++"/.conky_dzen_music") (myDzenBar musicx musicw "l")-}
        myStatBar = pipe (conky $ myXMonadDir++"/.conky_dzen") (myDzenBar statbarx statbarw "r") 
    -- start the lyric client that output lyrics to the log file
    {-spawn "fmclrc -l $'\t' > /tmp/fmclrc.log"-}
    handle <- spawnPipe myLogBar
    {-spawn myMusicBar-}
    spawn myStatBar
    -- put up trayer
    {-spawn myTrayer-}
    return handle
-- }}}

-- }}}

---------------- HandleEventHook -- {{{

-- due to some reasons it appears that the startWSSwitchHook has some conflicts with the handleKeyEventForXMonadMode hook
myHandleEventHook = startWSSwitchHook <+> handleKeyEventForXMonadMode <+> debugKeyEvents

-- }}}

---------------- ManageHook -- {{{

-- a toggle to define the insertion order of newly created windows
data InsertOlderToggle = InsertOlderToggle Bool deriving (Typeable, Read, Show)
instance ExtensionClass InsertOlderToggle where
    initialValue = InsertOlderToggle False
    extensionType = PersistentExtension

-- the first bool indicate whether a backup is in action; the second one is the actual backup
data InsertOlderToggleBackUp = InsertOlderToggleBackUp (Bool,Bool) deriving (Typeable, Read, Show)
instance ExtensionClass InsertOlderToggleBackUp where
    initialValue = InsertOlderToggleBackUp (False,False)
    extensionType = PersistentExtension

toggleInsertOlderForce = do
    InsertOlderToggle t <- XS.get
    XS.put $ InsertOlderToggleBackUp (True, t)
    XS.put $ InsertOlderToggle True
    runLogHook

toggleInsertOlderRecover = do
    InsertOlderToggleBackUp (h, b) <- XS.get
    if h then do
        XS.put $ InsertOlderToggleBackUp (False, False)
        XS.put $ InsertOlderToggle b
         else return ()
    runLogHook

toggleInsertOlder = do
    XS.put $ InsertOlderToggleBackUp (False, False)
    InsertOlderToggle t <- XS.get
    XS.put $ InsertOlderToggle $ not t
    runLogHook

insertPositionHook = ask >>= \w -> do
    tog <- liftX $ XS.get >>= \(InsertOlderToggle t) -> return t
    insertPosition Above $ if tog then Older else Newer

myManageHook = composeAll [ 
      insertPositionHook
    , autoArrangeHook taskGroups
    , dynamicMasterHook
    , isFullscreen --> doFullFloat  
    -- gimp related stuff
    , ((propertyToQuery (Role "gimp-toolbox")) <||> (propertyToQuery (Role "gimp-preferences")) <||> (propertyToQuery (Role "gimp-message-dialog")) <||> (propertyToQuery (Role "gimp-dock"))) --> doFloat
    -- the taskGroup launchHook
    , launchHook currentTaskGroup
    , manageDocks
    ]
      -- }}}

---------------- Scratchpad -- {{{
mkNamedScratchpad ls n = 
    case filter ((n==) . name) ls of
         sp:_ -> ifWindows (isMinimized <&&> query sp) (\(w:_) -> do
             deminimize w
             -- if it is in the current workspace then their is no need to do namedscratchpadaction anymore
             r <- runQuery isInCurrentWorkspace w
             if r then focus w else nsc
             ) nsc
         _ -> nsc
        where nsc = namedScratchpadAction ls n

isPidginBuddyList = className =? "Pidgin" <&&> propertyToQuery (Role "buddy_list")

scratchpads :: [NamedScratchpad]
scratchpads = [
    -- run ranger in myTerminal, find it by title, use default floating window placement
    {-NS "ranger" (myTerminal++" -name sranger -T ranger -e zsh -ic ranger") (appName =? "sranger") -}
    {-(customFloating $ W.RationalRect (0) (1/2) (1) (1/2)),-}

      NS "mutt" (uniqueTermFullCmd uniqueMutt) (isUniqueTerm uniqueMutt) lowerHalfRectHook,
      NS "pidgin" "pidgin" isPidginBuddyList idHook
    ] 

---- perWorkspaceScratchpads are scratchpads that are workspace specific
lastWorkspaceTag = withWindowSet $ return . W.tag . head . filter validWS . W.hidden

currWorkspace = gets (W.workspace . W.current . windowset)

allWorkspaceTags = allWorkspaces >>= return . map W.tag
allWorkspaceNames = do
    ts <- allWorkspaceTags
    mapM getWorkspaceNameForTag ts

allWorkspaces = filterWorkspaces validWS

filterWorkspaces p = do
    ws <- gets windowset 
    sort <- myWorkspaceSort
    return $ sort . filter p $ W.workspaces ws

invalidWorkspaces = filterWorkspaces (not . validWS)

-- we need to first have a translator that translates workspaceId to unique identifier
{-mkPerWSScratchpad cmd = do-}
    {-curr <- gets (W.currentTag . windowset)-}
    {-con <- perWSScratchpadContext curr-}
    {-dir <- getCurrentWorkspaceDirectory-}
    {-let csterm = UniqueTerm con cmd ""-}
    {-mkNamedScratchpad [ NS "cs" (uniqueTermFullCmd dir csterm) (isUniqueTerm csterm) idHook ] "cs"-}

-- swap the handles in addition so as to make sure that the scratchpads match up
modSwapTo dir = findWorkspace myWorkspaceSort dir validWSType 1 >>= swapWith

swapWith t = 
    -- first ensure that t is already accessible
    quickWorkspace t >>= \t -> do
        currTag <- gets (W.currentTag . windowset)
        swapWorkspaceHandlesByTags currTag t
        windows $ swapWorkspaces t currTag

-- }}}

---------------- Remodeled dynamicWorkspace based on workspaceNames -- {{{

---- addWorkspace: a name is entered through the prompt, after which the system tries the following in turn:
-- 1. if there's already a workspace with empty WorkspaceName and there's no window inside; then rename this workspace and shift to this workspace
-- 2. add an idle workspace with the naming format {id symbol}:{name entered by the user stored in WorkspaceNames}
-- the {id symbol} follows the convention 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, -, =, {... other ramdom symbols}
-- the system will look at the current id configuration to interpolate the new symbol to add e.g. if you have 1, 2, 3, 4, 5 then the new workspace would be 6

newtype WorkspaceNames = WorkspaceNames (M.Map String String)
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceNames where
    initialValue = WorkspaceNames M.empty
    extensionType = PersistentExtension

getWorkspaceNames = do
    tags <- allWorkspaceTags
    ns <- allWorkspaceNames
    return $ \t -> case find ((==t) . fst) $ zip tags ns of
                        Just (_,n) -> if null n then t else t++":"++n
                        Nothing -> t

getWorkspaceNameForTag tag = getWorkspaceHandle tag >>= getWorkspaceName
getWorkspaceName h = do
    WorkspaceNames m <- XS.get
    case M.lookup h m of
         Nothing -> return ""
         Just s -> return s

setWorkspaceName h n = do
    WorkspaceNames m <- XS.get
    XS.put $ WorkspaceNames $ if null n then M.delete h m else M.insert h n m
    refresh

setWorkspaceNameByTag t n = getWorkspaceHandle t >>= \h -> setWorkspaceName h n
setCurrentWorkspaceName n = getCurrentWorkspaceHandle >>= \h -> setWorkspaceName h n

-- a set of positions to simulate those used in Vim as i,a,I,A respectively
data WorkspaceInsertPosition = Before | After | Head | Last
instance Show WorkspaceInsertPosition where
    show Before = "insert before"
    show After = "insert after"
    show Head = "insert head"
    show Last = "insert last"

idleWorkspace insp = do
    curr <- currWorkspace
    wss <- fmap (filter ((/=tmpWorkspaceTag) . W.tag)) allWorkspaces
    p <- isRecyclableWS
    let currTag = W.tag curr
    if p curr 
       then return currTag
       else do
           let (candi, insi, ch) = case (insp, findIndex ((== (W.tag curr)) . W.tag) wss) of
                                (Before, Just i) -> (i-1, i, head currTag)
                                (After, Just i) -> (i+1, i+1, nextSymbol $ head currTag)
                                (Last, _) -> (length wss - 1, length wss, nextSymbol $ head $ W.tag $ last wss)
                                _ -> (0, 0, nextSymbol $ head tmpWorkspaceTag)
               cand = wss !! candi
           if candi >= 0 && candi < length wss && p cand
              then return $ W.tag cand
              else do
                  renameWorkspaces $ zip (fmap W.tag $ snd $ splitAt insi wss) $ fmap wrapList $ tail $ symbolFrom ch
                  DW.addHiddenWorkspace [ch]
                  return [ch]  

findWorkspaceWithName n insp = do
    wsts <- allWorkspaceTags
    wsns <- allWorkspaceNames
    -- we need to strip the tag names before the name; what a hassle)
    case elemIndex n wsns of
         Just i -> return (wsts !! i)
         Nothing -> reuseHiddenIdleWorkspaceWithName n insp

reuseHiddenIdleWorkspaceWithName n insp = do
    t <- idleWorkspace insp
    setWorkspaceNameByTag t n
    return t

isRecyclableWS = do
          -- get the names for the workspace
          wns <- getWorkspaceNames
          return $ \w -> W.tag w /= tmpWorkspaceTag && not (':' `elem` (wns $ W.tag w)) && validWS w && isNothing (W.stack w) 

symbolFrom :: Char -> String
symbolFrom ch 
    | distl >= 0 = take disth (enumFrom ch) ++ dropWhile (/= ch) symbolSequence ++ after
    | otherwise = enumFrom ch
    where chi = fromEnum ch
          orihi = fromEnum $ head originalSequence
          orili = fromEnum $ last originalSequence
          disth = orihi - chi
          distl = orili - chi
          after = tail . enumFrom $ last originalSequence

-- return the index of the symbol in the symbol stream
fromSymbol ch =
    case elemIndex ch symbolSequence of
         Just i -> i + orihi
         Nothing -> fromEnum ch
    where orihi = fromEnum $ head originalSequence



symbolCompare s1 s2 = case (pos1, pos2) of
                           (Just a, Just b) -> a `compare` b
                           _                -> s1 `compare` s2
    where pos1 = s1 `elemIndex` symbolSequence
          pos2 = s2 `elemIndex` symbolSequence

nextSymbol = head . tail . symbolFrom

symbolStringCompare s1 s2 = case (length s1, length s2) of
                                 (0, 0) -> EQ
                                 (0, _) -> LT
                                 (_, 0) -> GT
                                 (_, _) -> case symbolCompare (head s1) (head s2) of
                                                EQ -> symbolStringCompare (tail s1) (tail s2)
                                                r  -> r

getSymbolStringSort = mkWsSort $ return symbolStringCompare

---- viewWorkspace: this is relatively easy; just show up a prompt with all workspace names; upon action split the string by : and shift to the workspace by id

quickWorkspace tag = do
    wsts <- allWorkspaceTags
    if length tag == 1 && validSymbol tc 
       then case stripPrefix (fmap head wsts) ((takeWhile (/= tc) symbolStream) ++ [tc]) of
                 Just ls -> sequence_ (map (DW.addHiddenWorkspace . \c -> [c]) ls) >> return tag
                 Nothing -> return tag
       -- this is the case where the tag is not even valid
       else return tag
    where tc = head tag

viewWorkspace tag = quickWorkspace tag >>= windows . W.greedyView
toggleTag tag = do
    curr <- gets (W.currentTag . windowset)
    l <- lastWorkspaceTag
    return $ if curr == tag then l else tag

moveToWorkspace tag = quickWorkspace tag >>= windows . W.shift

-- the workspace prompt works by first returning all the completions for the current workspace; if there are no suitable completions, it automatically gives back results from "symtag print '%t' <tag>"
-- upon successfully creating a workspace, it will set the workspaceDirectory using the path given by "symtag print '%p' <tag> | head -n 1"
data WorkspacePrompt = WorkspacePrompt String

instance XPrompt WorkspacePrompt where
    showXPrompt (WorkspacePrompt s) = s ++ ": "
    commandToComplete _ = id
    nextCompletion _ c l = if null l then "" else l !! case c `elemIndex` l of
                                                       Just i -> if i >= length l - 1 then 0 else i + 1
                                                       Nothing -> 0

tagBin = myScriptsDir++"/xtag"
prepend _ [] = []
prepend t (x:l) = t:x:(prepend t l)
tagQuery = prepend "-iwholename" . fmap (("*"++).(++"*"))

workspacePrompt :: XPConfig -> String -> (WorkspaceId -> String -> X ()) -> (String -> String -> X ()) -> X ()
workspacePrompt conf p ef f = do 
    wts <- allWorkspaceTags
    wns <- fmap (fmap (\(a,b)->if null b then a else a++":"++b) . zip wts) allWorkspaceNames
    let complFun s = if null extWns 
               then fmap lines $ runProcessWithInput tagBin ([show tagLimit, "false", "-type", "d"] ++ tagQuery (words s)) ""
               else return extWns
               where fil = filter (searchPredicate conf s)
                     extWns = fil wns 
    mkXPrompt (WorkspacePrompt p) conf complFun $ \s -> do
        -- if the string is one of the existing tags
        let (t, n) = break (== ':') s
            nm =  if null n then n else tail n
        if t `elem` wts 
           then do
               -- the tag is found and it is one of the existing workspace
               -- do something with the name
               ef t nm
           else do
            -- set the workspaceDirectory
            -- output <- runProcessWithInput "symtag" ("print" : if '/' `elem` s then ["true", "%p", "1", ".*"++s] else ["false", "%p", "1", ".*"++s++".*"]) ""
            ls <- fmap lines $ runProcessWithInput tagBin [show tagLimit, "true", s] ""
            let path = if null s || null ls then "" else trim $ head $ ls
                name = if null ltag then s 
                                    else if ' ' `elem` ltag then fmap head $ filter (not . null) $ splitOn " " ltag
                                    else if '_' `elem` tltag then fmap head $ filter (not . null) $ splitOn "_" ltag
                                    else ltag
                                    where ltag = last $ splitOn "/" path
                                          tltag = tail ltag
            -- run the specified script under the directory
            if not $ null path then spawn $ path ++ "/.ws_start" else return ()
            -- we will cut the string of any slash (shortened tag form)
            -- do something with the name
            f name path

newWorkspacePrompt conf prompt insp f = workspacePrompt conf prompt (\t n -> f t) (\n p -> do
    t <- reuseHiddenIdleWorkspaceWithName n insp
    f t 
    saveWorkspaceDirectory p t)

---- rename the current workspace
renameWorkspacePrompt conf = workspacePrompt conf "Rename workspace" (\t n -> setCurrentWorkspaceName n) (\n p -> setCurrentWorkspaceName n >> saveCurrentWorkspaceDirectory p) 

wrapList c = [c]

removeCurrentWorkspace = removeCurrentWorkspace' ""
removeCurrentWorkspace' reg = do
    -- remove all window selectinos
    -- remove all the hiden windows as well
    curr <- gets (W.currentTag . windowset)
    (l, r, gf, ml, float, _, _) <- getSides
    -- move all the windows to the tmpworkspace
    let winsToKill = float ++ l ++ gf ++ r ++ ml
    minimizeWindows' tmpWorkspaceTag winsToKill
    -- our methodology is simple, remove the current workspace and reorder the symbol stream for the tags
    -- so this involves repairing the tags with the associated handles
    -- we can savely remove the workspace if all the workspaces after this workspace are empty
    if curr /= tmpWorkspaceTag 
       then do
            wts <- allWorkspaceTags
            let (bef, _:aft) = break (==curr) wts
                nt = case aft of
                          [] -> last bef
                          h:_ -> h
            windows $ W.greedyView nt
            -- kill the scratchpads matching this workspace
            ifWindows (isPerWSScratchpadBoundToWS curr) (mapM_ killWindow) (return ())
            windows $ removeWorkspaceByTag curr
            removeWorkspaceHandleByTag curr
            renameWorkspaces (zip aft $ fmap wrapList $ symbolFrom $ head curr)
        -- we'd like to remove the name and directory setting for the temp workspace in that case
       else setCurrentWorkspaceName "" >> saveCurrentWorkspaceDirectory ""

-- assume that the workspace to be removed is in the hidden workspaces list
removeWorkspaceByTag t s@(W.StackSet { W.hidden = hs })
    = let (xs, y:ys) = break ((== t) . W.tag) hs
      in s { W.hidden = xs ++ ys }

renameWorkspaces ls = do
    WorkspaceHandles m <- XS.get
    let fixHandle (ot, nt) m' = case M.lookup ot m of
                                    Nothing -> m'
                                    Just h -> M.insert nt h m'
        nm = foldr fixHandle m ls
        -- need to remove for all the tags that have not been covered in the new tag set
        (ots, nts) = unzip ls
        diff = S.toList $ S.difference (S.fromList ots) (S.fromList nts)
        fm = foldr M.delete nm diff
    XS.put $ WorkspaceHandles fm
    windows $ \s -> 
        -- first all workspaces are condensed into a list, with the index remembered for later separating them
        let sp = 1 + (length $ W.visible s)
            wss = W.workspaces s
            rename (o, n) wss' = case findIndex ((==o) . W.tag) wss of
                                      Just i -> case splitAt i wss' of
                                                     (bs,h:as) -> bs++(h {W.tag = n} : as)
                                                     _ -> wss'
                                      _ -> wss'
            nwss = foldr rename wss ls
            (ncw:nvws, nhws) = splitAt sp nwss
            nc = (W.current s) {W.workspace = ncw}
            nvs = fmap (\(w, v) -> v{W.workspace = w}) $ zip nvws $ W.visible s
        in s { W.current = nc
             , W.visible = nvs
             , W.hidden = nhws
             }

-- }}}

---------------- Wallpaper changer-- {{{

data WallpaperPrompt = WPPrompt String

instance XPrompt WallpaperPrompt where
    showXPrompt (WPPrompt dir) = dir ++ " > "
    commandToComplete _ = id
    nextCompletion _ c l = let (cmd, arg) = splitArg c 
                           in if cmd `isCmdPrefixOf` "setch" then "setch " ++ escape (l !! exactMatchIndex (unescape arg))
                                                              else  l !! exactMatchIndex cmd
                                    where exactMatchIndex a = case a `elemIndex` l of
                                                                    Just i -> if i >= length l - 1 then 0 else i+1
                                                                    Nothing -> 0    
    -- only check on the last word
    highlightPredicate _ cl cmd = let lastArg = case splitArg cmd of
                                                    (a, "") -> a
                                                    (_, b) -> b
                                  in unescape lastArg == cl

data WallpaperChannel = WPChannel String deriving (Typeable, Show, Read)
instance ExtensionClass WallpaperChannel where
    initialValue = WPChannel "."
    extensionType = PersistentExtension

-- available commands
---- next: go to the next wallpaper
---- setch: change the wallpaper channel
---- rate: tag the current wallpaper favorite (move it to the favorite folder) (and NOT moving it anywhere)
---- ban: move the wallpaper to the trash folder
---- open: open the current wallpaper in feh
wpComplFunc conf str = 
    let (cmd, arg) = splitArg str
    in if cmd `isCmdPrefixOf` "setch"
                -- add in the . to denote the base wallpaper directory, and also to prevent auto complete 
                then fmap ((\l -> if null l then l else l++["."]) . filter (searchPredicate conf arg) . map (\s -> fromMaybe s (stripPrefix wallpaperDirectory s)) . lines) $ runProcessWithInput "find" [wallpaperDirectory, "-mindepth", "1", "-type", "d"] ""
                else return $ filter (isPrefixOf cmd) ["next", "setch", "rate", "ban", "trash"]

wpAction c ch str = 
    let (cmd, arg') = splitArg str
        arg = unescape arg'
    in if cmd `isCmdPrefixOf` "setch"
          then do
                io $ setCurrentDirectory wallpaperDirectory
                -- save the arg into the database
                e <- io $ doesDirectoryExist arg 
                if e
                   then do
                       XS.put $ WPChannel arg
                       spawn $ changeWallpaperCmd ++ " " ++ escapeQuery arg
                       mkWPPrompt' c c
                   else do
                       let searchDir = "rss/flickr/" ++ arg
                           ess = escapeQuery searchDir
                           script = "rss-image-download "++ escapeQuery ("https://api.flickr.com/services/feeds/photos_public.gne?format=rss_200_enc&tags=" ++ arg) ++ " " ++ ess ++ "; [ -d " ++ ess ++ " ] && " ++ changeWallpaperCmd ++ " " ++ ess
                       XS.put $ WPChannel searchDir
                       runProcessWithInput "/bin/sh" ["-c", script] ""
                       mkWPPrompt' c c
          else if cmd `isCmdPrefixOf` "trash"
                then if ch /= "." 
                        then do
                            XS.put $ WPChannel "."
                            spawn $  "dir="++escapeQuery (wallpaperDirectory ++ ch)++"; target=\"$HOME/.Trash/${dir##*/}\"; rm -rf \"$target\"; mv -f \"$dir\" \"$target\"" 
                            spawn $ changeWallpaperCmd ++ " " ++ wallpaperDirectory
                            -- mkWPPrompt' (c {defaultText = "setch "}) c
                            mkWPPrompt' c c
                        else
                            mkWPPrompt' c c
          else do
              case cmd of
                "next" -> spawn $ changeWallpaperCmd ++ " ."
                "rate" -> spawn $ changeWallpaperCmd ++ " -F"
                "ban" -> spawn $ changeWallpaperCmd ++ " -D ."
                _ -> return ()
                -- reopen the wp prompt (chained)
              mkWPPrompt' c c

mkWPPrompt toggleFadeSet c = do
    fadeOutLogHook $ fadeIf (return True) wallpaperFadeFullAmount
    mkWPPrompt' c c
    fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount

mkWPPrompt' c' c = do
    ch <- getWPChannel
    io $ setCurrentDirectory $ wallpaperDirectory ++ ch
    mkXPrompt (WPPrompt ch) c' (wpComplFunc c) (wpAction c ch)

getWPChannel = do
    WPChannel ch' <- XS.get 
    e <- io $ doesDirectoryExist $ wallpaperDirectory ++ ch'
    return $ if e then ch' else "."

getWPDirectory = getWPChannel >>= \c -> return $ wallpaperDirectory ++ c


---------------- Start workspace transition timer -- {{{
-- due to some weird reasons the dzen bar refuses to be detected by the avoidStruts at startup. Resolution: switch to the right workspace after some delay (assuming that dzen has started up by that time

data StartWSChangeState = StartWSChangeState TimerId deriving Typeable
instance ExtensionClass StartWSChangeState where
    initialValue = StartWSChangeState 0

startWSSwitchHook e = do
    (StartWSChangeState t) <- XS.get                 
    handleTimer t e $ do              
        windows $ W.view tmpWorkspaceTag
        return Nothing                  
    return $ All True

-- }}}

---------------- Prompt systems -- {{{

-- | Creates an autocompletion function for a programm given the program's name and a list of args to send to the command.
completionFunctionWith :: String -> [String] -> IO [String]
completionFunctionWith cmd args = do fmap lines $ runProcessWithInput cmd args ""

-- | Creates a prompt with the given modes
launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config

----- Dynamic prompt -- {{{
outputWidth = 200
outputHeight = 40
tagLibroot = "~/DB/"
dphandler = myScriptsDir++"/dphandler"

data DynamicPrompt = DPrompt String

instance XPrompt DynamicPrompt where
    showXPrompt (DPrompt dir) = dir ++ " > "
    commandToComplete _ = id
    nextCompletion _ = dpromptNextCompletion
    highlightPredicate _ = dpromptHighlightPredicate

-- helper functions
parseShellArgs str = let tk b a = if null a || (isSpace (head a) && not (null b) && head b /= '\\')
                                      then ([reverse b], a)
                                      else tk (head a:b) (tail a)
                         (arg, rest) = tk "" (dropWhile (==' ') str)
                     in if null rest then arg else arg ++ parseShellArgs rest
unescape = let tk b cked a = case (b, a, cked) of
                                (_,       [],      _) -> reverse b
                                ('\\':bs, '\\':as, False) -> tk b True as 
                                ('\\':bs, ha:as,   False) -> tk (ha:bs) False as 
                                (_,       ha:as,   _) -> tk (ha:b) False as 
           in tk "" False

stripSuffix suf s = fmap reverse $ stripPrefix (reverse suf) (reverse s)
isOutput = (>= outputWidth) . length
limitSpace l str = take l $ str ++ repeat ' '

-- search widgets
fasdLimit = 20
findLimit = 20
tagLimit = 20
grepLimit = 50
whichLimit = 20
historyLimit = 20
evalLimit = 20
topLimit = 40

isGrepOutput s = isOutput s && ':' `elem` s
stripGrepOutput = head . splitOn ":"
isShortcutOutput s = length s > 2 && (s!!1) == ':'
stripShortcutOutput = last . splitOn ":"
-- the index i is the i'th arg from the end
isSearchFilter (i, s) = i > 0 && s `elem` ["f", "a", "d", "z", "h", "l", "t", "g", "gp", "which", "diff"] || ((length s) `elem` [1, 2] && (head s) `elem` "'m")
isStatDiffOutput = (=~ " +\\| +[0-9]+ [+-]+")
stripStatDiffOutput = trim . head . splitOn "|" 
filterDiffOutput ls = case findIndex (isPrefixOf "--- ") ls of
                            Just i | i > 0 && i < length ls - 1 && (cxt (i-1) (i+1) || cxt (i+1) (i-1)) ->
                                                case stripPrefix "--- a/" (ls !! i) of
                                                     Just r -> (trim r):fr
                                                     _ -> fr
                                   | otherwise -> fr
                                where fr = filterDiffOutput $ snd $ splitAt (i+2) ls
                                      cxt n m = isPrefixOf "+++ " (ls !! n) && isPrefixOf "index " (ls !! m)
                            _ -> []

data PromptWidget = PromptWidget { promptPrefix :: String
                                 , promptCommandToComplete :: String -> String
                                 , promptNextCompletion :: String -> [String] -> String
                                 , promptComplFunction :: XPConfig -> ComplFunction
                                 , promptAction :: String -> X ()
                                 , promptHighlightPredicate :: String -> String -> Bool
                                 }
-- (prefix, commandToComplete, nextCompletion, complFunc, modeAction)
dwgt p pre cf act = PromptWidget { promptPrefix = pre
                                 , promptCommandToComplete = commandToComplete p
                                 , promptNextCompletion = nextCompletion p
                                 , promptComplFunction = cf
                                 , promptAction = act
                                 , promptHighlightPredicate = highlightPredicate p
                                 }
isWidgetFilter (i, s) = i > 0 && s `elem` (fmap promptPrefix dynamicPromptWidgets)
widgetCmd w c = let p = promptPrefix w++" " in joinStr p $ tail $ splitOn p c
findWidget pre = find ((==pre) . promptPrefix) dynamicPromptWidgets
findWidgetForAction c = fmap (\(r, w) -> (fromJust r, w)) $ find (isJust . fst) $ fmap (\w -> (stripPrefix (promptPrefix w ++ " ") c, w)) dynamicPromptWidgets

dynamicPromptWidgets = [
        -- this follow the (prefix, prompt) format
        dwgt VBPrompt "vb" (\c -> vbComplFunc) vbAction
      , dwgt defaultSDMode "sdcv-Collins" (\c -> completionFunction defaultSDMode) (flip (modeAction defaultSDMode) "")
      , dwgt mobySDMode "sdcv-Moby" (\c -> completionFunction mobySDMode) (flip (modeAction mobySDMode) "")
      , dwgt modernCHSDMode "sdcv-modernChinese" (\c -> completionFunction modernCHSDMode) (flip (modeAction modernCHSDMode) "")
      , dwgt bigCHSDMode "sdcv-bigChinese" (\c -> completionFunction bigCHSDMode) (flip (modeAction bigCHSDMode) "")
      , dwgt TaskPrompt "tk" (\c -> taskComplFunc) taskAction
      , dwgt FMCPrompt "fmc" (\c -> fmcComplFunc) fmcAction
      , dwgt CalcMode "calc" (\c -> completionFunction CalcMode) (flip (modeAction CalcMode) "")
    ]

evalStr s = let evalcomps = splitOn "`" s
                evallen = length evalcomps 
                t = tail evalcomps
            in if odd evallen && evallen >= 3 then (head evalcomps, head t, joinStr "`" $ tail t)
                                              else (s,"","")

stripIndex (a, b) = (fmap snd a, fmap snd b)
dpromptNextCompletion c l = let hl = any (flip dpromptHighlightPredicate c) l in
                case stripIndex $ break (\s -> isSearchFilter s || isWidgetFilter s) $ zip [0..] $ reverse args of
                   (_:_, "z":bef) | not hl -> joinStr " " $ reverse bef ++ ["c", escape (head l)]
                   ([], ('\'':pre):bef) -> exactMatch $ fmap stripShortcutOutput l
                   ([], ('m':pre):_) -> c
                   (_, "which":bef) | all ((`elem` "~/") . head) l && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                                    | otherwise -> exactMatch l
                   (aft, "diff":bef) | not (null fdo) -> joinStr " " $ reverse bef ++ [escape (head fdo)]
                   (_:_, w:bef) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                           in joinStr " " $ reverse bef ++ [w, promptNextCompletion wp (widgetCmd wp c) l]
                   -- we have to specify manually so the 'm' case above when there is some sort of the string on its right would sift through here
                   (_:_, f:bef) | f `elem` ["f", "a", "d", "h", "l", "t", "which"] && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                   _ | all isGrepOutput l -> joinStr " " $ takeWhile (\t -> t /="g" && t /= "gp") (init args) ++ case exactNext $ fmap stripGrepOutput l of
                                                                                          [] -> []
                                                                                          s -> [s]
                     | all isShortcutOutput l -> exactMatch $ fmap stripShortcutOutput l
                     | not (null fsdo) && length fsdo == length l - 1 -> exactMatch $ fmap stripStatDiffOutput fsdo
                     | not (null fdo) -> exactMatch fdo
                     | not (null evalstr) ->  evall ++ escape (head l) ++ evalr
                     | all isOutput l && any ("commit" `isPrefixOf`) l && any ("Author" `isPrefixOf`) l && any ("Date" `isPrefixOf`) l -> exactMatch $ fmap (fromJust . stripPrefix "commit " . trim) $ filter ("commit " `isPrefixOf`) l
                     | isOutput (head l) -> c
                     | otherwise -> exactMatch l
             where lastArg = last $ args
                   args = parseShellArgs c
                   (evall, evalstr, evalr) = evalStr c
                   fdo = filterDiffOutput l
                   fsdo = filter isStatDiffOutput l
                   exactMatch ls = fromMaybe "" (stripSuffix lastArg c) ++ exactNext ls
                   exactNext ls = let rev = reverse ls in escape $ rev !! case findIndex (== unescape lastArg) rev of
                               Just i -> if i <= 0 then length ls - 1 else (i-1)
                               Nothing -> length ls - 1 

dpromptHighlightPredicate cl cmd = case stripIndex $ break (\s -> isSearchFilter s || isWidgetFilter s) (zip [0..] $ reverse args) of
                (_:_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                      in promptHighlightPredicate wp cl (widgetCmd wp cmd)
                _ | isGrepOutput cl -> stripGrepOutput cl == unescapedLastArg
                  | isShortcutOutput cl -> stripShortcutOutput cl == unescapedLastArg
                  | not (null unescapedLastArg) && ("--- a/"++unescapedLastArg) `isPrefixOf` cl -> True
                  | isStatDiffOutput cl -> stripStatDiffOutput cl == unescapedLastArg
                  | trimcl =~ "commit [0-9a-z]{40}" -> last (words trimcl) == unescapedLastArg
                  | isOutput cl -> False
                  | otherwise -> not (null args) && unescapedLastArg == cl 
             where args = parseShellArgs cmd
                   unescapedLastArg = unescape $ last args 
                   trimcl = trim cl

whenNull ma mb = do
    l <- ma
    if null l then mb else return l

dpromptComplFunc c cmds home hist s = do
        let args = parseShellArgs s
            lastArg = if null args then "" else last args
            unescapedArgs = map unescape args
            sht = shortend home
            shtout = fmap (fmap sht)
            epd = expandd home
            sp = searchPredicate c
            fasd args = shtout $ fmap lines $ runProcessWithInput (myScriptsDir++"/xfasd") (show fasdLimit:args) ""
            sct pre = fmap lines $ runProcessWithInput (myScriptsDir++"/xshortcut") [ "print", pre ] ""
            ntailsp = length $ takeWhile isSpace (reverse s)
            output pro ags = fmap (map (limitSpace outputWidth . sht) . lines) $ runProcessWithInput pro ags ""
            (_,evalstr,_) = evalStr s
            trycmp = foldl whenNull (return [])
            scopecmp = case reverse unescapedArgs of
                                   "":fa:_ | ntailsp == 1 -> output "/home/lingnan/bin/scope" [epd fa, show outputWidth, show outputHeight, show outputHeight]
                                   _ -> return [] 
            shellcmp = let (scs, sas) = case head unescapedArgs of
                                "man" -> (cmds, [])
                                "du" -> ([], ["file"])
                                "c" -> ([], ["directory"])
                                "cd" -> ([], ["directory"])
                                _ | length args > 1 -> ([], ["file"])
                                  | otherwise -> (cmds, ["file"])
                       in shellcmp' sas scs
            shellcmp' sas scs = shtout $ getShellCompl' False sas scs $ epd lastArg
            grepcmp' cmd aft = fmap (filter (':' `elem`)) $ output (myScriptsDir++"/"++cmd) $ show grepLimit:(fmap epd aft)
            grepcmp = grepcmp' "xgrep"
            pgrepcmp = grepcmp' "xpdfgrep"
        case stripIndex $ break (\s -> isSearchFilter s || isWidgetFilter s) (zip [0..] $ reverse unescapedArgs) of
                    (af:afs, "f":_) -> fasd $ ["-f", "-B", "viminfo"] ++ (reverse $ af:afs)
                    (af:afs, "d":_) -> fasd $ "-d" : (reverse $ af:afs)
                    (af:afs, "a":_) -> fasd $ ["-a", "-B", "viminfo"] ++ (reverse $ af:afs)
                    (af:afs, "z":_) -> fasd $ "-d" : (reverse $ af:afs)
                    (af:afs, "h":_) -> return $ take historyLimit $ filter (sp $ joinStr " " $ reverse $ af:afs) $ hist
                    ([], ('\'':pre:[]):_) -> shtout $ sct [pre]
                    ([], ('m':pre:[]):_) | pre /= 'v' -> shtout $ sct [pre]
                    (af:afs, "l":_) -> shtout $ fmap lines $ runProcessWithInput (myScriptsDir++"/xfind") (show findLimit:(fmap epd $ reverse (af:afs))) ""
                    -- for g, we demand that at least SOME search term is entered, otherwise we don't generate the necessary output
                    (af:afs, "g":_) -> trycmp $ (if afs == [] then [shellcmp' ["file"] []] else []) ++ [grepcmp (reverse $ af:afs)]
                    (af:afs, "gp":_) | afs == [] -> shellcmp' ["file"] []
                                     | not (null afs) && af == "" -> pgrepcmp (reverse $ af:afs)
                    (af:afs, "t":_) -> fmap (fmap (tagLibroot++) . lines) $ runProcessWithInput tagBin ([show tagLimit, "false"] ++ tagQuery (reverse (af:afs))) ""
                    (af:afs, "which":_) -> 
                        let ha = head cas
                            cas = reverse $ af:afs
                        -- test if af is one of the commands
                        in if ha `elem` cmds then shtout $ fmap lines $ runProcessWithInput "which" cas ""
                                             else shtout $ return $ take whichLimit $ filter (sp $ joinStr " " cas) cmds
                    (af:afs, "diff":_) | ntailsp == 1 -> output (myScriptsDir++"/xdiff") (show outputWidth:show outputHeight:(reverse (af:afs)))
                    (_:_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                          in (promptComplFunction wp) c (widgetCmd wp s)
                -- grave key evaluation (evaluate the grave enclosed string in shell and show the output as autocompletion)
                    _ | not (null evalstr) -> shtout $ fmap (take evalLimit . lines) $ runProcessWithInput "bash" ["-c", evalstr] ""
                      | otherwise -> case unescapedArgs of
                            "man":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xman") $ show outputWidth:show outputHeight:pa:pai:pas
                            "du":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xdu") $ show outputHeight:pa:pai:pas
                            ["ip", pa] -> return $ filter (sp pa) ["addr", "addrlabel", "link", "maddr", "mroute", "neigh", "route", "rule", "tunnel"]
                            "ip":pas | lastArg == "" -> output "ip" $ init pas
                            ["top", ""] | ntailsp == 1-> output (myScriptsDir++"/xtop") [show topLimit]
                            ["free", ""] | ntailsp == 1 -> output "free" []
                            ["ifconfig", ""] | ntailsp == 1 -> output "ifconfig" []
                            ["git", ""] | ntailsp == 1 -> output "git" ["status"]
                            -- only gives the prime command completion on three spaces
                            ["git", pa] -> return $ filter (sp pa) ["add", "am", "archive", "bisect", "branch", "bundle", "checkout", "cherry-pick", "citool", "clean", "clone", "commit", "describe", "diff", "fetch", "format-patch", "gc", "grep", "gui", "init", "log", "merge", "mv", "notes", "pull", "rebase", "reset", "rm", "shortlog", "show", "stash", "status", "submodule", "tag"]
                            -- in all other instances we should give the log information
                            "git":pa:_ -> trycmp [output "git" $ ["log", "--grep", last unescapedArgs], scopecmp, shellcmp]
                            _ -> trycmp [scopecmp, shellcmp]

dpromptAction c cmds home hist s = 
        -- perform some special actions on some commands
        let args = parseShellArgs s
        in case (findWidgetForAction s, args) of
                (Just (rest, w), _) -> (promptAction w) rest
                (_, [('m':pre:[]), pa]) | pre /= 'v' -> spawn $ myScriptsDir++"/xshortcut mark "++[pre]++" "++pa
                (_, ha:pas) | ha `elem` ["cd", "c", "z"] -> do
                                 d <- if null pas 
                                         then return home 
                                         else case head pas of
                                                 "-" -> getCurrentWorkspaceOldDirectory
                                                 p -> return $ unescape p
                                 saveCurrentWorkspaceDirectory d
                                 dynamicPrompt' c cmds home hist
                            | otherwise -> spawn $ dphandler++" "++if null s then "." else s

divide' _ [] (r, w) = (reverse r, reverse w)
divide' p (x:xs) (r, w) = divide' p xs $ if p x then (x:r, w) else (r, x:w)
divide p l = divide' p l ([],[])

dynamicPrompt c = do
    cmds <- io getCommands
    home <- io $ env "HOME" "/home/lingnan"
    hist <- fmap (fmap unescape . nub . sort . filter (\s -> isPrefixOf "/" s || isPrefixOf "~/" s)) $ io $ historyCompletion ""
    --     relative' = filter (\s -> isPrefixOf "./" s || isPrefixOf "../" s) hist
    -- relative <- filterM (\f -> do
    --         fe <- io $ doesFileExist f 
    --         de <- io $ doesDirectoryExist f
    --         return $ fe || de) relative'
    -- to better fasd performace, we can first extract out all the values for the fasd components
    ------ let isd p = io (getFileStatus p >>= return . isDirectory) `catchX` (return False)
    ------ fasdl <- fmap lines $ runProcessWithInput (myScriptsDir++"/xfasd") [] ""
    ------ isds <- mapM isd fasdl
    -- let (fasdd', fasdf') = divide snd $ zip fasdl isds
    --     fasdd = fst $ unzip fasdd'
    --     fasdf = fst $ unzip fasdf'
    -- dynamicPrompt' c cmds home (absolute++relative)
    dynamicPrompt' c cmds home hist

dynamicPrompt' c cmds home hist = do
    d <- getCurrentWorkspaceDirectory
    io $ setCurrentDirectory d
    -- only deal with directories or files at the moment; not doing check on the file / directories to save performance
    mkXPrompt (DPrompt $ shortend home d) c (dpromptComplFunc c cmds home hist) (dpromptAction c cmds home hist)
-- }}}

----- Calculator prompt -- {{{

data CalculatorMode = CalcMode

type ExtensionActions = M.Map String (String -> X())

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
    showXPrompt CalcMode = "calc > "
    commandToComplete CalcMode = id --send the whole string to `calc`
    completionFunction CalcMode = \s -> if (length s == 0) then return [] else do
        fmap (lines . trim) $ runProcessWithInput "calc" ["--", s] ""
    modeAction CalcMode c tx = spawn $ "res=\"`calc -- '"++c++"'`\"; shopt -s extglob; res=\"${res##*([[:space:]])}\"; res=\"${res%%*([[:space:]])}\"; echo -n \"$res\" | xclip -selection clipboard"

calcMode :: XPMode
calcMode = XPT CalcMode

-- }}}

----- Dictionary / sdcv prompt -- {{{

sdcvBin = myScriptsDir++"/xsdcv"

sdLength = "250"

data StarDictMode = SDMode { prompt :: String
                           , dictName :: String
                           }

instance XPrompt StarDictMode where
    showXPrompt (SDMode p _) = p
    commandToComplete _ = id
    completionFunction (SDMode _ d) = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput sdcvBin [d, s, sdLength] ""
    modeAction _ query _ = spawn $ "vb " ++ escapeQuery ("d !dictionary "++query)
    nextCompletion _ c _ =  c
    highlightPredicate _ _ _ = False
mkSDMode p d = XPT $ SDMode p d
defaultSDMode = SDMode "Collins Cobuild 5 > " "Collins Cobuild 5"
mobySDMode = SDMode  "Moby Thesaurus II > " "Moby Thesaurus II"
modernCHSDMode = SDMode "现代汉语词典 > " "Modern Chinese Dictionary"
bigCHSDMode = SDMode "汉语大词典 > " "Chinese Big Dictionary"
defaultEngDictModes = [XPT $ defaultSDMode, XPT $ mobySDMode]
defaultChDictModes = [XPT $ modernCHSDMode, XPT $ bigCHSDMode]
defaultCalcModes = [calcMode]

-- }}}

----- Vimb prompt -- {{{

data VBPrompt = VBPrompt

vbNextCompletion cmd ls = last $ words $ ls !! ni
        where ni = case findIndex (vbIsEqualToCompletion cmd) ls of
                      Just i -> if i >= length ls - 1 then 0 else i+1
                      Nothing -> 0 
vbHighlightPredicate = flip vbIsEqualToCompletion
vbAction s = getCurrentWorkspaceDirectory >>= \d -> runShell $ "vb -C " ++ escapeQuery ("set download-path="++d) ++ " " ++ escapeQuery s
----- if it's empty then return the top 10 results from history
vbComplFunc s
    | null $ trim s = do fmap lines $ runProcessWithInput "vbhistory" [(show vbhistorySize)] "" 
    | otherwise = do fmap lines $ runProcessWithInput "vbopenurls" (["-n",(show vbMatchSize)]++(words s)) "" 


instance XPrompt VBPrompt where
    showXPrompt _ = "vimb > "
    commandToComplete _ = id
    nextCompletion _ = vbNextCompletion
    highlightPredicate _ = vbHighlightPredicate

vbIsEqualToCompletion cmd cl = cmd == (last $ words cl)

mkVBPrompt c = mkXPrompt VBPrompt c vbComplFunc vbAction

-- }}}

----- Wiki prompt -- {{{
data WikiMode = WkMode

instance XPrompt WikiMode where
    showXPrompt WkMode = "wiki > "
    commandToComplete WkMode = id
    completionFunction WkMode = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput "wiki" [s, "200"] ""
    modeAction WkMode query tx = spawn $ "vp" ++ " w " ++ escapeQuery query

wkMode :: XPMode
wkMode = XPT WkMode

-- }}}

----- Wolfram prompt -- {{{
----- due to the timeout problem, we should instead use a timer, which basically get called and waits for some time before setting a variable to 1. everytime it's recalled it resets the previous timer and starts again

data WolframMode = WAMode

escapeq "" = ""
escapeq (m:ms) = case m of
                          '\'' -> "'\"'\"'" ++ escapeq ms
                          _ -> [m] ++ escapeq ms

escapeQuery m = '\'' : ((escapeq m) ++ "'")
                          

instance XPrompt WolframMode where
    showXPrompt WAMode = "wolframAlpha > "
    commandToComplete WAMode = id
    completionFunction WAMode = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput "wa" [s] "" 
    modeAction WAMode query tx = spawn $ "vp wa " ++ escapeQuery query

waMode :: XPMode
waMode = XPT WAMode

-- }}}

----- Window search prompt -- {{{

data WindowSearchPrompt = WindowSearchPrompt String
instance XPrompt WindowSearchPrompt where
    showXPrompt (WindowSearchPrompt p) = p
    commandToComplete _ c = c
    nextCompletion _ = getNextCompletion


mkSearchPrompt config prompt predicate a = do
    wm <- windowMap
    -- filter the windows according to p
    matches <- filterM (runQuery predicate . snd) $ M.toList wm
    mkXPrompt (WindowSearchPrompt prompt) config (\s -> return $ filter (searchPredicate config s) $ map fst matches) $ \r ->
        whenJust (M.lookup r wm) $ a

-- }}}

-- }}}

---------------- History window states -- {{{

-- we need to have a extension state class to hold the windows that are 'marked' (having been navigated
data MarkedWindows = MarkedWindows (S.Set Window) deriving (Typeable, Show, Read)
instance ExtensionClass MarkedWindows where
    initialValue = MarkedWindows S.empty
    extensionType = PersistentExtension

markWindow w = XS.get >>= \(MarkedWindows s) -> XS.put $ MarkedWindows $ S.insert w s

unmarkWindow w = XS.get >>= \(MarkedWindows s) -> XS.put $ MarkedWindows $ S.delete w s

windowIsMarked w = XS.get >>= \(MarkedWindows s) -> return $ S.member w s

getMarkedWindowsSize = XS.get >>= \(MarkedWindows s) -> return $ S.size s

clearAllMarks = XS.put $ MarkedWindows S.empty

-- a query bool version of checking if window is markd
windowIsMarkedQuery = ask >>= liftX . windowIsMarked 

-- we should clear the retrace state when
--  1. the event is a keyboard event
--  2. the key triggered is not any of the retrace keys
historyBackKey = "o"
historyForwardKey = "i"
-- handle the event and clear the retrace state when needed
handleKeyEventForXMonadMode (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress = do
        withDisplay $ \dpy -> do
            sym <- io $ keycodeToKeysym dpy code 0
            let keyStr = keysymToString sym
                -- , ; . are excepted because they DO have meaning during a stack retrace
                validKeysForRetraceAllTime = (myModMask, "period") : [(mk, k) | mk <- [myModMask, myModMask .|. shiftMask, myModMask .|. controlMask, myModMask .|. controlMask .|. shiftMask, myModMask .|. mod1Mask, myModMask .|. mod1Mask .|. controlMask], k <- [historyBackKey, historyForwardKey]]
                -- validKeysForRetraceWhenJumpWindowSaved = [(myModMask, k) | k <- ["comma", "semicolon"]]
            if (m, keyStr) `elem` validKeysForRetraceAllTime 
                   then return ()
                   else clearAllMarks
        return (All True)
handleKeyEventForXMonadMode _ = return (All True)

-- we need to put the toggle in the front interface so that the subsequent triggering of m-; and m-, does not break the save toggle
-- dir is the direction to navigate through the history (Prev means going back in history and Next means going forward)
jumpWindowHistory dir p = do
    -- saveJumpWindowSavedState saved 
    -- if saved then saveFindFunction (\dr -> jumpWindowHistory (if dr == dir then Next else Prev) p True) else return ()
    let nmat dirp = nextMatch History (dirp <&&> validWindow <&&> fmap not isMinimized <&&> p) 
    case dir of
         Prev -> withFocused markWindow >> nmat (fmap not windowIsMarkedQuery)
         Next -> nmat windowIsMarkedQuery >> withFocused unmarkWindow >> runLogHook

-- we need to have a toggle to indicate whether the last jump window query is saved; this is used when we clear the modes (if the last query is saved then m-; and m-, are excepted otherwise they clear the modes

-- data JumpWindowSavedState = JumpWindowSavedState Bool deriving (Typeable, Show, Read)
-- instance ExtensionClass JumpWindowSavedState where
--     initialValue = JumpWindowSavedState False
--     extensionType = PersistentExtension
 
-- getJumpWindowSavedState = XS.get >>= \(JumpWindowSavedState s) -> return s
-- saveJumpWindowSavedState = XS.put . JumpWindowSavedState

-- }}}

---------------- Quick find window states -- {{{

-- this module tries to simulate the f <key> behavior in vim
-- the module saves just one thing: a function that takes a dir argument and can be invoked later through m-, and m-;
data QuickFindFunction = QuickFindFunction (Direction1D -> X (Maybe (Window, Bool))) deriving Typeable
instance ExtensionClass QuickFindFunction where
    initialValue = QuickFindFunction (\d -> return Nothing)

saveFindFunction q = XS.put $ QuickFindFunction q
getFindFunction = XS.get >>= \(QuickFindFunction q) -> return q

playLastFindFunction d = getFindFunction >>= \f -> f d
-- }}}

---------------- Command repeat states -- {{{
data LastCommand = LastCommand (X ()) deriving Typeable
instance ExtensionClass LastCommand where
    initialValue = LastCommand (return ())

saveLastCommand a = XS.put $ LastCommand a
getLastCommand = XS.get >>= \(LastCommand a) -> return a

-- }}}

---------------- Keyboard hotkey -- {{{
allOrderedWindows = do
    -- get all the windows before the focused and after the focused depending on the direction
    (l, r, gf, ml, float, ll, rl) <- getSides
    return $ ll ++ l ++ gf ++ r ++ float ++ ml ++ rl
orderedWindowsMatchingPredicate p = allOrderedWindows >>= filterM (runQuery p)
wrapAroundWindowsMatchingPredicate dir p = do
    -- get all the windows before the focused and after the focused depending on the direction
    (l, r, gf, ml, float, ll, rl) <- getSides
    mf <- gets (W.peek . windowset)
    let wwins = rl ++ ll
        (awinr, awinl, bwins, cwins) = case mf of
                                        Just f | [f] == gf -> (f:r, l, float, ml)
                                               | otherwise -> case break (==f) float of
                                                                    (fl, _:fr) -> (f:fr, fl, l++gf++r, ml)
                                                                    _ -> ([], [], float, ml)
                                        _ -> ([], [], float, ml)
        winl = if dir == Next then awinr++cwins++awinl++wwins else reverse $ wwins++bwins++awinr++cwins++awinl
    filterM (runQuery p) winl


getSides = do
    -- get all the windows before the focused and after the focused depending on the direction
    wss <- allWorkspaces
    winset <- gets windowset
    -- split the windows into two
    -- it's safer to get the windows for the current workspace from group stacks (it's more stable)
    gs <- G.getCurrentGStack
    ml <- fmap reverse getCurrentMinimizedWindows
    let (l, r, gf) = maybe ([],[],[]) (\(W.Stack f u d) -> (reverse u, d, [f])) $ maybe Nothing G.toZipper gs
        (b, a') = break ((==W.currentTag winset). W.tag) wss
        a = if not $ null a' then tail a' else []
        ll = nub $ concatMap (W.integrate' . W.stack) b
        rl = nub $ concatMap (W.integrate' . W.stack) a
        -- also retrieve the float windows (for comprehensiveness)
        currswins = W.integrate' $ W.stack $ W.workspace $ W.current winset
        float = filter (not . (`elem` (l++r++ml++gf))) currswins 
    -- obtain information regarding the current window: Either Window (before, window, after)
    -- the focused window is either focused in the stack or in the floats
    return (l, r, gf, ml, float, ll, rl)


data TaskGroup = TaskGroup { taskGroupName :: String
                             -- ^ the name given for this task group
                           , filterKey :: String
                             -- ^ the filter key used in key sequences to select this group, an empty string means 
                             -- this group should not be filtered using key sequence
                           , filterPredicate :: Query Bool
                             -- ^ the query bool used to filter this group 
                           , localFirst :: Bool
                             -- ^ should any filtering occur on a local workspace first order
                           , construct :: X ()
                             -- ^ hook that gets called when a new window should be replicated; returns True if successfully constructed
                           , launchHook :: ManageHook
                             -- ^ hook that gets called during the first launch of the window (in the managehook)
                           , windowStyle :: Direction1D -> ManageHook
                             -- ^ a function that returns a style (manageHook) given an index
                           , colorScheme :: SubTheme
                             -- ^ tab color definitions
                           }

instance Eq TaskGroup where
    t == t' = (taskGroupName t) == (taskGroupName t') && (filterKey t) == (filterKey t')

instance Show TaskGroup where
    show = taskGroupName

compFilterKeyList [] [] = EQ
compFilterKeyList [] _ = LT
compFilterKeyList _ [] = GT
compFilterKeyList (h1:l1) (h2:l2) = let c = compCh h1 h2 in if c == EQ then compFilterKeyList l1 l2 else c
    where compCh c1 c2 = let lc1 = toLower c1
                             lc2 = toLower c2
                         in if lc1 == lc2 then compare c2 c1
                                       else compare lc1 lc2

filterKeyList = fmap (\s-> (if "S-" `isPrefixOf` s then toUpper else toLower) $ last s) . filter (not . null) . splitOn " "

instance Ord TaskGroup where
    compare t t' = let r = compFilterKeyList (filterKeyList $ filterKey t) (filterKeyList $ filterKey t') in if r == EQ then compare (taskGroupName t) (taskGroupName t') else r

constructInDir construct dir = construct

doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)
-- doViewShift = doF . liftM2 (.) W.greedyView W.shift
                              
-- the window mappings are used to store the index of the windowStyle in the list
data WindowStyleIndexStates = WindowStyleIndexStates (M.Map Window Int) deriving (Typeable, Show, Read)
instance ExtensionClass WindowStyleIndexStates where
    initialValue = WindowStyleIndexStates M.empty 
    extensionType = PersistentExtension

windowStyleFromList :: [ManageHook] -> (Direction1D -> ManageHook)
windowStyleFromList ls dir 
    | null ls = idHook
    | otherwise = ask >>= getIndex >>= (!!) ls
        where getIndex win = liftX $ do
                    WindowStyleIndexStates m <- XS.get
                    let ni = case M.lookup win m of
                         Just i -> (i + (if dir == Next then 1 else -1)) `mod` (length ls)
                         -- reason for this: the first style is assumed to be the DEFAULT style, which means it's already applied when the windowStyle is triggered, so we should get the next style 
                         Nothing -> 1 `mod` (length ls)
                    XS.put $ WindowStyleIndexStates $ M.insert win ni m
                    return ni                  
    
lowerHalfRectHook = customFloating $ W.RationalRect (0) (1/2) (1) (1/2)
upperHalfRectHook = customFloating $ W.RationalRect (0) (0) (1) (1/2)
rightPanelHook = customFloating $ W.RationalRect (4/5) (14/900) (1/5) (1-14/900)
leftPanelHook = customFloating $ W.RationalRect (0) (14/900) (1/5) (1-14/900)

instance Default TaskGroup where 
    def = TaskGroup { taskGroupName = "Unknown"
                      , filterKey = ""
                      , filterPredicate = alwaysTrue
                      , localFirst = True
                      , construct = return ()
                      , launchHook = idHook
                      -- the default window styles involving
                      , windowStyle = windowStyleFromList [doSink, lowerHalfRectHook]
                      , colorScheme = mySubTheme
                      }

taskGroups = [ 
      -- vimb instances
      def { taskGroupName = "vimb"
          , filterKey = "b"
          , filterPredicate = className =? "Vimb"
          {-, construct = runShell "vimb \"`tail -n1 ~/.config/vimb/history | cut -d'\t' -f1`\""-}
          , construct = vbAction ""
          {-, launchHook = ask >>= doF . -}
          -- green
          , colorScheme = mySubTheme { winInactiveColor = "#1d371d"
                                     , winActiveColor = "#337f33"
                                     , winActiveBorderColor = "#1d371d"
                                     }
          }
      -- vim intances
    , def { taskGroupName = "vim"
          , filterKey = "v"
          , filterPredicate = isTerm <&&> (appName =? "vim" <||> fmap (\s -> (" - VIM" `isInfixOf` s) && (not $ isInfixOf "vimpager" s)) title)
          , construct = runShell "xvim"
          -- brown
          , colorScheme = mySubTheme { winInactiveColor = "#372517"
                                     , winActiveColor = "#7f5233"
                                     , winActiveBorderColor = "#372517"
                                     }
          }
      -- pidgin buddylist
    , def { taskGroupName = "pidgin-buddy"
          , filterPredicate = isPidginBuddyList
          , launchHook = rightPanelHook
          , construct = mkNamedScratchpad scratchpads "pidgin"
          , windowStyle = windowStyleFromList [rightPanelHook, leftPanelHook, doSink]
          }
      -- pidgin conversation windows
      -- we'd like to put the conversations (self poped one) into another workspace if they are not launched by the user
    , def { taskGroupName = "pidgin"
          , filterKey = "d"
          , filterPredicate = className =? "Pidgin" <&&> fmap not (propertyToQuery (Role "buddy_list"))
          , construct = mkNamedScratchpad scratchpads "pidgin"
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
    , def { taskGroupName = "ranger"
          , filterKey = "r"
          , filterPredicate = isTerm <&&> (title =? "ranger" <||> appName =? "ranger")
          , construct = runTerm "ranger" "ranger" "loader ranger"
          -- yellow
          , colorScheme = mySubTheme { winInactiveColor = "#353119"
                                     , winActiveColor = "#7f7233"
                                     , winActiveBorderColor = "#353119"
                                     }
          }
     -- zathura instances
    , def { taskGroupName = "zathura"
          , filterKey = "z"
          , filterPredicate = className =? "Zathura"
          , construct = runShell "zathura"
          -- red
          , colorScheme = mySubTheme { winInactiveColor = "#371921"
                                     , winActiveColor = "#7f334a"
                                     , winActiveBorderColor = "#371921"
                                     }
          }
      -- recyclable term instances (m-s-<Return> triggers the same event)
    , def { taskGroupName = "term"
          , filterKey = "t"
          , filterPredicate = isRecyclableTerm <||> appName =? "xterm"
          , construct = runTerm "" "xterm" "zsh -i"
          }
      -- notice: group selection that applies to these 'hidden' groups (namely triggered by auto-group when one of the windows is in focus), will still apply to all matched windows
      -- mutt scratchpad singleton
    {-, def { taskGroupName = "mutt"-}
          {-, filterKey = "m"-}
          {-, filterPredicate = isUniqueTerm uniqueMutt-}
          {-, localFirst = False-}
          {-, launchHook = lowerHalfRectHook-}
          {-, construct = mkNamedScratchpad scratchpads "mutt"-}
          {-, windowStyle = windowStyleFromList [lowerHalfRectHook, upperHalfRectHook, doSink]-}
          {-}-}
    -- mutt general instance
    , def { taskGroupName = "mutt"
          , filterKey = "m"
          , filterPredicate = isTerm <&&> (title =? "mutt" <||> appName =? "mutt")
          , construct = runTerm "mutt" "mutt" "loader mutt"
          }
      -- ranger scratchpads
    {-, def { taskGroupName = "scratchranger"-}
          {-, filterKey = "r"-}
          {-, filterPredicate = isPerWSScratchpadBoundToCurrentWS <&&> uniqueTermHasCmd "loader ranger"-}
          {-, localFirst = False-}
          {-, launchHook = lowerHalfRectHook-}
          {-, construct = mkPerWSScratchpad "loader ranger"-}
          {-, windowStyle = windowStyleFromList [lowerHalfRectHook, upperHalfRectHook, doSink]-}
          {-}-}
      -- weechat singleton
    {-, def { taskGroupName = "weechat"-}
          {-, filterKey = "w"-}
          {-, filterPredicate = isTerm <&&> (fmap ("weechat" `isInfixOf`) title <||> appName =? "weechat")-}
          {-, localFirst = False-}
          {-[>, launchHook = liftX (findWorkspaceWithName "comm") >>= doViewShift<]-}
          {-[>, construct = runUniqueTerm "" uniqueWeechat<]-}
          {-, construct = runTerm "weechat" "weechat" "loader weechat-curses -r '/redraw'"-}
          {-}-}
      -- finch singleton
    {-, def { taskGroupName = "finch"-}
          {-, filterKey = "f"-}
          {-, filterPredicate = isUniqueTerm uniqueFinch-}
          {-, localFirst = False-}
          {-[>, launchHook = liftX (findWorkspaceWithName "comm") >>= doViewShift<]-}
          {-, construct = runUniqueTerm "" uniqueFinch-}
          {-}-}
      -- intellij singleton
    , def { taskGroupName = "idea"
          , filterKey = "i"
          , filterPredicate = className =? "jetbrains-idea"
          , localFirst = False
          , construct = runShell "intellij-idea-ultimate-edition"
          }
      -- gimp singleton
    , def { taskGroupName = "gimp"
          , filterKey = "S-m"
          , filterPredicate = className =? "Gimp"
          , localFirst = False
          , construct = runShell "gimp"
          }
      -- inkscape (can have multiple documents)
    , def { taskGroupName = "inkscape"
          , filterKey = "k"
          , filterPredicate = className =? "Inkscape"
          , construct = runShell "inkscape"
          }
      -- libreoffice (can have multiple documents)
    , def { taskGroupName = "libre"
          , filterKey = "l"
          , filterPredicate = fmap (isInfixOf "libreoffice") className
          , construct = runShell "libreoffice"
          }
      -- all remaining xterms can be matched in this group
    , def { taskGroupName = "term(...)"
          , filterKey = "S-t"
          , filterPredicate = isTerm
          , construct = runTerm "" "" "zsh -i"
          }
      -- all remaining windows that share the same class attributes
    , def { filterPredicate = hasSamePropertyAsFocused className }
      -- all other remaining windows
    , def { filterPredicate = alwaysTrue }
    ]

siftedTaskGroups = siftTaskGroups taskGroups

-- siftTaskGroups rework on the original window group definition and ensure that there exists an exclusive relationship between all groups (by ensuring that a group matched at lower index is never matched in the later index
siftTaskGroups gs = if len <= 1 then gs else head gs : fmap loadExQueryForGroupAtIndex [1..(length gs - 1)]
    where loadExQueryForGroupAtIndex i = loadAndPredicateForGroupAtIndex i $ foldl (<&&>) alwaysTrue $ fmap (fmap not . filterPredicate) $ take i gs
          loadAndPredicateForGroupAtIndex i p = let g = gs !! i in g {filterPredicate = p <&&> (filterPredicate g)}
          len = length gs

-- this dynamic lookup on the window group should return the window group with the filter predicate SIFTED
taskGroupOfWindow :: [TaskGroup] -> Window -> X (Maybe TaskGroup)
taskGroupOfWindow gs win = taskGroupIndexOfWindow gs win >>= return . fmap (gs !!)

siftedTaskGroupOfWindow gs win = taskGroupIndexOfWindow gs win >>= return . fmap (siftTaskGroups gs !!)

-- this will cycle through the groups of windows, conceivably.
taskGroupIndexOfWindow gs win = if null gs then return Nothing else nextMatch win 0 
    where len = length gs
          predAtIndex i = filterPredicate $ gs !! i
          nextMatch w i 
              | i >= len = return Nothing
              | otherwise = do
                  re <- runQuery (predAtIndex i) w
                  if re then return (Just i) else nextMatch w (i+1)

-- return a multistack (only two level) grouped by the tasks of the windows
getCurrentTaskGStack wgs = do
    mgs <- G.getCurrentGStack
    case mgs of
         Just gs -> do
             tgs <- taggedGStack wgs gs
             return $ case G.toZipper tgs of 
                          Just s@(W.Stack (f,_) _ _) -> case break (elem f) $ fmap (fmap fst) $ groupBy (\(_,a) (_,b)->a==b) $ W.integrate s of
                                                         (bgs,fg:ags) -> Just $ G.Node $ W.Stack (pc fg) (fmap pc (reverse bgs)) (fmap pc ags)
                                                         ([],[]) -> Nothing
                                                         (bgs,_) -> Just $ G.Node $ fromJust $ W.differentiate $ fmap pc bgs
                                                        where pc ls = case break (==f) ls of
                                                                                (bs,_:as) -> G.Leaf $ Just $ W.Stack f (reverse bs) as
                                                                                (bs,_) -> G.Leaf $ W.differentiate bs
                          _ -> Nothing
         _ -> return Nothing

toggleTaskGroup = nextMatch History toggleTaskGroupPredicate
toggleTaskGroupPredicate = fmap not (filterPredicate currentTaskGroup) <&&> isInCurrentWorkspace <&&> fmap not isMinimized

isOneOfWindows ls = ask >>= \w -> do
    return $ w `elem` ls

-- for contextual window groups, they are used for determining the group the FOCUSED window is in
contextualGroup gs =
    def { filterPredicate = ask >>= \w -> liftX $ do
               mf <- gets (W.peek . windowset) 
               let matchWithFocused f = do
                   g <- siftedTaskGroupOfWindow gs f
                   runQuery (maybe (alwaysFalse) filterPredicate g) w
               maybe (return False) matchWithFocused mf
        -- the launchHook of for the current window is defined as all the matching hook in the groups
        -- therefore it would be a simple map of all the relevant hooks
        , launchHook = composeAll $ fmap (\g -> filterPredicate g --> launchHook g) gs
        , windowStyle = \dir -> ask >>= liftX . taskGroupOfWindow gs >>= maybe idHook (\g -> (windowStyle g) dir)
        -- we still need to query for the current window group and then perform on absence based upon that... EXPENSIVE!
        , construct = withFocused $ \w -> taskGroupOfWindow gs w >>= maybe (return ()) (\g -> construct g)
        }


-- the default contextualGroup 
currentTaskGroup = contextualGroup taskGroups


-- return all window groups including the currentTaskGroup one; note that window groups without filter keys are excepted
allTaskGroupsWithFilterKey mdKeys = 
    (filter (not . null . filterKey) siftedTaskGroups)++if null mdKeys then [] else [currentTaskGroup {filterKey = k} | k <- mdKeys]


-- a simplified version of appFilterKeys with correct predicates for filtering in the current workspace; if the app's definition is not useful for local context then switch to global
localFirstFilterPredicate g = let f = filterPredicate g in if localFirst g then isInCurrentWorkspace <&&> f else f

-- apply the managehook to a window
runManageHook :: ManageHook -> Window -> X ()
runManageHook mh = windows . appEndo <=< runQuery mh

runManageHookOnFocused = withFocused . runManageHook

mountDupKeys dupKeys ls = 
    ls ++ fmap (\(k, ok) -> (k, maybe (return ()) snd $ find ((== ok) . fst) ls)) dupKeys

appendMap :: (a->a) -> [a] -> [a]
appendMap fun ls = ls ++ fmap fun ls

appendActionWithException a ex ls = ex ++ fmap (\(k, ka) -> (k, ka >> a ka)) ls

-- the normal typableChars
typeables = typeablesWithoutAlphas++alphas
alphas = ['a'..'z']++['A'..'Z']
typeablesWithoutAlphas = ['0'..'9']++"!@#$%^&*()-_=+\\|`~[{]}'\",<.>?"
typeablesNoSlashNorQuote = filter (\c -> c /='/' && c /='\'') typeables
typeableKeyStrokes = fmap charToKeyStroke typeables

charToKeyStroke c = if isUpper c then "S-"++[toLower c]
                                 else case c of
                                           '!' -> "S-1"
                                           '@' -> "S-2"
                                           '#' -> "S-3"
                                           '$' -> "S-4"
                                           '%' -> "S-5"
                                           '^' -> "S-6"
                                           '&' -> "S-7"
                                           '*' -> "S-8"
                                           '(' -> "S-9"
                                           ')' -> "S-0"
                                           '_' -> "S--"
                                           '+' -> "S-="
                                           '|' -> "S-\\"
                                           '~' -> "S-`"
                                           '{' -> "S-["
                                           '}' -> "S-]"
                                           '"' -> "S-'"
                                           '<' -> "S-,"
                                           '>' -> "S-."
                                           '?' -> "S-/"
                                           _ -> [c]

-- if the focused element is inside the windows to be pasted then we can keep selecting that (is that ever useful though?)  
shiftWindowsHere = shiftWindowsHereAndFocusLast True Nothing
shiftWindowsHereAndFocusLast _ _ [] = return ()
-- we can specify whether we want to keep focus 
-- this defaults to keeping focus
shiftWindowsHereAndFocusLast insertBefore f wins = do
    -- deminimize all of them
    mf <- gets (W.peek . windowset)
    if maybeToList mf == wins 
       then return () 
       else do
            mapM deminimize wins
            curr <- gets (W.currentTag . windowset)
            -- move to the temp workspace and then move back
            -- move one by one because we can't be sure if the windows are the last one in the tab group (really annoying though)
            mapM (windows . W.shiftWin scratchpadWorkspaceTag) wins
            {-windows $ \s -> foldr (W.shiftWin scratchpadWorkspaceTag) s wins-}
            mf <- gets (W.peek . windowset)
            let fc = maybe (maybe id W.focusWindow mf) W.focusWindow f
            if insertBefore
               then windows $ \s -> fc $ foldr (W.shiftWin curr) s wins
               -- this doesn't seem to work
               else windows $ \s -> fc $ foldl (\s w -> W.swapDown $ W.shiftWin curr w s) s wins

hasTagQuery s = ask >>= \w -> liftX $ hasTag s w

-- implementing the motion keys list
-- we don't allow 0 because that is kept for use as d0
numberKeys = zip (fmap show [1..9]) [1..9]
tabKeys = [ (fromJust $ subgroupIndexToSymbol n, n) | n <- [0..(length subgroupSymbolSequence - 1)] ]
columnKeys = tabKeys
groupKeys = zip (fmap show [1..9]) [0..9]
regKeys :: [(String, (String -> X a) -> X (Maybe a))]
regKeys = [(charToKeyStroke k, feedReg [k]) | k <- typeablesNoSlashNorQuote ]
feedReg k fun = fmap Just $ fun k
regReadonlyKeys = [("'", feedReg "'")]
regPromptKey prompt = ("/", \a -> initMatches >>= \r -> tagPrompt (myXPConfig r) {
    searchPredicate = prefixSearchPredicate
} prompt a)

data Motion = Motion {
        -- the window that should be focused by navigation
          target :: Maybe (Window, Bool)
        , simpleX :: Maybe (X ())
        , toggleList :: Maybe [Window]
        , historyToggle :: Maybe (X ())
        , triggerOnNothing :: Maybe (X ())
        , searchFunction :: Maybe (Direction1D -> X (Maybe (Window, Bool)))
    }
instance Default Motion where
    def = Motion {
          target = Nothing
        , simpleX = Nothing
        , toggleList = Nothing
        , historyToggle = Nothing
        , triggerOnNothing = Nothing
        , searchFunction = Nothing
        }
-- motion keys are usually only obtained through X (). This is because there are a bunch of things to calculate
motionKeys :: [(String, String, X Motion)]
motionKeys = 
    -- M1 toggles
    [ (sfk++t, fk++t, do
             ls <- getBaseCurrentWindows
             let len = length ls
                 w = ls !! ind
                 ind = (min (len-1) n) `mod` len
                 def' = def { simpleX = Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusAt ind }
             return $ case (len, leap) of
                          (0, _) -> def
                          (_, False) -> def' { target = Just (w, True) }
                          _ | n >= len -> def
                            | otherwise -> def' { target = Just (w, True) 
                                                , toggleList = Just [w]
                                                , historyToggle = Just $ nextMatch History $ isOneOfWindows ls 
                                                }
      )
    | (sfk, fk, leap, quick) <- [("M1-", "M1-", True, False), ("f ", "M-f ", False, False), ("", "M-g ", False, True)]
    , (t, n) <- if quick then [("0", 0), ("S-4", -1)] else tabKeys
    ]
    -- C- toggles
    ++
    [ (sfk++c, fk++c, do
             gs <- G.getCurrentGStack
             let def' = def {simpleX = Just $ sendMessage $ G.Modify $ G.focusGroupAt n}
             case gs of
                  Just (G.Node s) -> let ls = W.integrate s
                                         len = length ls
                                         g = ls !! (min (len-1) n)
                                         w = G.focal g 
                                     in return $ case (len, leap) of
                                                    (0, _) -> def
                                                    (_, False) -> def' { target = fmap (\a -> (a,True)) w }
                                                    _ | n >= len -> def
                                                      | otherwise -> def' { target = fmap (\a -> (a,True)) w
                                                                          , toggleList = Just $ G.flattened g
                                                                          , historyToggle = Just $ toggleGroup
                                                                          }
                  _ -> return def
      )
    | (c, n) <- columnKeys 
    , (sfk, fk, leap) <- [("C-", "C-", True), ("S-f ", "M-S-f ", False)]
    ]
    ++
    -- perform the saved find function forward and backwards
    [ (sfk, "M-"++sfk, do
        w <- playLastFindFunction dir
        -- our rational is that if this is a find function then it is continuous
        return def { target = w }
      )
    | (sfk, dir) <- [(";", Next), (",", Prev)]
    ]
    ++
    -- word and back
    [ (sfk++wk, fk++wk,
          let move' dir = do
               mfs <- getFocusStack
               let sign' = if dir == Next then sign else sign * (-1)
               return $ case mfs of
                       Just (W.Stack f u d) -> Just (Just (ls !! ind, include), ind)
                            where ci = length u
                                  ls = W.integrate' mfs 
                                  ind = (ci + sign' * n) `mod` (length ls)
                       _ -> Nothing
              move = fmap (maybeToMaybe fst) . move'
          in move' Next >>= \mt -> return $ case mt of
                    Just (t, ind) -> def { target = t
                                         , searchFunction = Just move
                                         , simpleX = Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusAt ind 
                                         }
                    _ -> def
      )
    | (nk, n) <- numberKeys
    , (wk, sign, include) <- [("b", -1, True), ("w", 1, False)]
    , (sfk, fk) <- [(nk++" ", "M-g "++nk++" ")] ++ if n == 1 then [("", "M-")] else []
    ]
    ++
    [ (sfk, fk, do
        -- get the second level nesting counting from the top
        ins <- focusIsInGStack
        bs <- fmap (maybeToMaybe G.bases . maybeToMaybe G.current) G.getCurrentGStack
        let (t, ind) = case (ins, bs) of
                             (True, Just (G.Node s)) -> (fmap (\a -> (a, True)) $ G.focal $ ls !! ind', ind')
                                where ls = W.integrate s
                                      len = length ls
                                      ind' = (min n (len-1)) `mod` len
                             _ -> (Nothing, -1)
        return def {
                  target = t
                , simpleX = Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusGroupAt ind
            }
      )
    | (sfk, fk, n) <- [("g g", "M-g g", 0)] 
                      ++ 
                      [ (nk++" S-g", "M-g "++nk++" S-g", n')
                      | (nk, n') <- groupKeys] 
                      ++
                      [ ("S-g", "M-S-g", -1)]
    ]
    -- the h j k l's
    ++
    [ (sfk++wk, fk++wk,
          let move' dir' = do
                  let sign = if dir == dir' then 1 else -1
                  ins <- focusIsInGStack
                  bs <- fmap gfun G.getCurrentGStack
                  return $ case (ins, bs) of
                      (True, Just (G.Node s@(W.Stack _ up _))) -> Just ((fmap (\a -> (a, True)) $ G.focal $ ls !! ind), ind)
                         where ls = W.integrate s 
                               len = length ls
                               ci = length up
                               ind = (ci + sign * n) `mod` len
                      _ -> Nothing
              move = fmap (maybeToMaybe fst) . move'
          in move' Next >>= \mt -> return $ case mt of
                    Just (t, ind) -> def { target = t
                                         , searchFunction = Just move
                                         , simpleX = Just (sx ind)
                                     }
                    _ -> def
      )
    | (nk, n) <- numberKeys
    , (wk, dir, gfun, sx) <- [(wk', dir', maybeToMaybe G.bases . maybeToMaybe G.current, \n -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusGroupAt n)
                             | (wk', dir') <- [("k", Prev), ("j", Next)]]
                             ++
                             [(wk', dir', id, \n -> sendMessage $ G.Modify $ G.focusGroupAt n)
                             | (wk', dir') <- [("h", Prev), ("l", Next)]]
    , (sfk, fk) <- [(nk++" ", "M-g "++nk++" ")] ++ if n == 1 then [("", "M-")] else []
    ]
    ++
    [ ("g "++filterKey g, "M-g "++filterKey g, do
            -- the cycling and toggling would require different techniques
            let move dir = wrapAroundWindowsMatchingPredicate dir (fmap not isFocused <&&> localFirstFilterPredicate g) >>= return . fmap (\a->(a,True)) . listToMaybe
            t <- move Next 
            wins <- orderedWindowsMatchingPredicate ((filterPredicate g) <&&> isInCurrentWorkspace)
            return def {
                      target = t
                    , toggleList = Just wins
                    , searchFunction = Just move
                    , triggerOnNothing = Just $ construct g
                }
      )
    | g <- allTaskGroupsWithFilterKey ["c"]
    ]
    ++
    [ ("' "++tk, "M-g ' "++tk, do
            let move dir = do
                    re <- ta (\t -> case t of
                                         "'" -> fmap maybeToList $ nextMatched History (return True)
                                         _ -> wrapAroundWindowsMatchingPredicate dir (fmap not isFocused <&&> hasTagQuery t))
                    return $ case re of
                         Just (a:_) -> Just (a, True)
                         _ -> Nothing
            t <- move Next
            mwins <- ta (\t -> orderedWindowsMatchingPredicate (hasTagQuery t))
            return def {
                      target = t
                    , toggleList = Just $ fromMaybe [] mwins
                    , searchFunction = Just move
                }
      )
    | (tk, ta) <- regKeys ++ [regPromptKey "Select windows from register:"] ++ regReadonlyKeys
    ]

motionKeyCommands' = flip fmap motionKeys $ \(_, k, x) -> (k, do
    mf <- gets (W.peek . windowset)
    Motion mt simpx mtogls mtoggle mconstruct mfind <- x
    -- first save the function for future reference
    case mfind of
         Just fun -> saveFindFunction fun
         _ -> return ()
    -- if in visual mode then update the end marker
    inv <- isInVisualMode
    let fx t = if isJust simpx then fromJust simpx else deminimizeFocus t
    case (mt, mtogls, mtoggle, mconstruct, mf, inv) of
         (_, Just togls, _, _, _, True) -> visualModeUpdateToggleSet togls >> refresh
         (Just (t, _), _, _, _, _, True) -> visualModeUpdateEndMarker t >> fx t
         (Just (t, _), _, Just toggle, _, Just f, False) | t == f -> toggle
         (Just (t, _), _, _, _, _, False) -> fx t
         (Nothing, _, _, Just construct, _, _) -> construct
         _ -> return ())

motionKeyCommands = concatMap processKey motionKeyCommands' 

-- return the normal selection for most of the commands
motionKeyWindowSelection = flip fmap motionKeys $ \(k, _, x) -> (k, do
    mf <- gets (W.peek . windowset)
    Motion mt _ mls _ _ _ <- x
    case mls of
         Just ls -> return ls
         _ -> do
           wins <- allOrderedWindows
           return $ case (mf, mt) of
                        (Just f, Just (t, include)) -> case (elemIndex f wins, elemIndex t wins) of
                                                 (Just fi, Just ti) | fi < ti -> drop fi $ take (ti + if include then 1 else 0) wins
                                                                    | fi == ti -> [f]
                                                                    | otherwise -> drop ti $ take fi wins
                        _ -> [])

-- the first argument is the group key
struct gfun n = do
    bs <- fmap gfun G.getCurrentGStack
    let ls = case bs of
                 Just (G.Node (W.Stack f u d)) -> concatMap G.flattened $ take n $ [f]++d
                 _ -> []
    return ls
groupList = struct (maybeToMaybe G.bases . maybeToMaybe G.current)
columnList = struct id
structKeys gk = 
    [ (fks, xls)
    | (nk, n) <- numberKeys
    , (fk, xls) <- [ (gk, groupList n)
                   , ("c", columnList n)]
    , fks <- [nk++" "++fk] ++ if n == 1 then [fk] else []
    ]
wssKeys =
    [ (fks, do
            wss <- allWorkspaces
            curr <- gets (W.currentTag . windowset)
            let ls = concatMap (W.integrate' . W.stack) $ take n $ snd $ break ((==curr).W.tag) wss
            return ls)
    | (nk, n) <- numberKeys
    , fks <- [nk++" s"] ++ if n == 1 then ["s"] else [] 
    ]

cutCommands = concatMap (processKey . addPrefix) 
    [ (regk++dc, da)
    | (regk, ta) <- [("", feedReg "")]++ fmap (\(a,b) -> ("' "++a++" ", b)) (regKeys ++ [regPromptKey "Cut windows to register:"])
    , (dc, da) <- [ (fk, do
                        ls <- xls
                        ta $ \t -> cut t ls
                        return ())
                  | (fk, xls) <- [ ("d "++mk, xls')
                                 | (mk, xls') <- motionKeyWindowSelection
                                                ++
                                                structKeys "d"]
                                 ++
                                 [ ("S-q", groupList 1) ]
                                 ++
                                 [ ("C-q", columnList 1) ]
                  ]
                  ++
                  [ ("q", do
                       gs <- getSelectedWindowStack
                       ta $ \t -> cut t (W.integrate' gs)
                       return ())
                  ]
                  ++
                  [ (fks, do
                      ta $ \t -> sequence_ $ take n $ repeat $ removeCurrentWorkspace' t
                      return ())
                  | (fks, n) <- [ (wfk, n')
                                | (nk, n') <- numberKeys
                                , wfk <- ["d "++nk++" s"] ++ if n' == 1 then ["d s"] else [] ]
                                ++
                                [ ("C-S-q", 1) ]
                  ]
    ]

markCommands = concatMap (processKey . addPrefix) $
    [ (kstr, da)
    | (regk, ta) <- regKeys ++ [regPromptKey "Put windows into register:"]
    , (kstr, da) <- [ ("' "++regk++" m "++mk, do
                            ls <- xls
                            ta $ \t -> mapM_ (addTag t) ls
                            return ())
                    | (mk, xls) <- motionKeyWindowSelection
                                   ++
                                   structKeys "m"
                                   ++
                                   wssKeys]
                    ++
                    [ ("S-m "++regk, do
                         gs <- getSelectedWindowStack
                         ta $ \t -> mapM_ (addTag t) (W.integrate' gs)
                         return ())
                    ]
    ]
    ++
    [ ("u "++mk, do
        ls <- xls
        mapM_ unTag ls)
    | (mk, xls) <- motionKeyWindowSelection
                   ++
                   structKeys "u"
                   ++
                   wssKeys]
    ++
    [ ("S-u", do
           gs <- getSelectedWindowStack
           mapM_ unTag (W.integrate' gs))
    ]

pasteCommands = concatMap (processKey . addPrefix)
    [ (regk++cmd, ta (paste i np) >> return ())
    | (cmd, i, np) <- [("p", False, True)
                      -- , ("S-p", False, False)
                      , ("g p", True, True)
                      -- , ("g S-p", True, False)
                      ]
    , (regk, ta) <- [("", feedReg "")]++ fmap (\(a,b) -> ("' "++a++" ", b)) (regKeys ++ [regPromptKey "Cut windows to register:"]) 
    ]
visualCommands = 
    [ ("M-v", toggleVisualMode Win)
    , ("M-S-v", toggleVisualMode Row)
    , ("M-C-v", toggleVisualMode Col)
    ]

cloneCommands = concatMap (processKey . addPrefix) $
    [ ("c "++(filterKey g), construct g)
    | g <- allTaskGroupsWithFilterKey ["c"] ]
    ++
    -- m-c <number> <group> allows you to create multiple copies of a given task group
    -- m-c <number> <number> creates the multiple copies of the current group
    [ ("c "++nk++" "++(filterKey g), sequence_ $ take n $ repeat (construct g))
    | (nk, n) <- numberKeys
    , g <- allTaskGroupsWithFilterKey [nk]
    ]

historyCommands =
    [ ("M-"++m++dirk++ms, jumpWindowHistory dir p)
    | (dirk, dir) <- [(historyBackKey, Prev), (historyForwardKey, Next)]
    , (m, ms, p) <- [ ("", "", alwaysTrue) ] 
                    ++
                    -- the task group filtered history navigation is experimental -- it can changes the order of history in complex ways
                    [ ("S-"++sm, " "++(filterKey g), smp <&&> filterPredicate g) 
                    | g <- allTaskGroupsWithFilterKey ["M-S-o", "M-S-i"]
                    , (sm, smp) <- [("", alwaysTrue)] ]
                    ++
                    -- inter group stack level
                    [ ("C-", "", toggleGroupPredicate)
                    -- within the group stack
                    , ("M1-", "", toggleWithinGroupPredicate)
                    -- within different task group
                    , ("C-M1-", "", toggleTaskGroupPredicate)]
    ]

-- the layout commands are there to provide convenient access
layoutCommands = 
    [ ("C-S-"++k, sendMessage $ G.Modify $ G.moveToGroupAt n)
    | (k, n) <- columnKeys]
    ++
    [ ("M1-S-"++k, sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.insertAt n)
    | (k, n) <- tabKeys]
    ++
    [ ("M1-C-"++k, sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapWith n)
    | (k, n) <- tabKeys]
    ++
    [ ("M-<Space>", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage NextLayout)
    , ("M-S-<Space>", nextOuterLayout)
    , ("M-S-b", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapUp)
    , ("M-S-w", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapDown)
    , ("M-C-S-k", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupUp)
    , ("M-C-S-j", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupDown)
    , ("M-C-S-h", sendMessage $ G.Modify G.swapGroupUp)
    , ("M-C-S-l", sendMessage $ G.Modify G.swapGroupDown)
    , ("M-S-k", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsUp s)
    , ("M-S-j", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsDown s)
    , ("M-S-h", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsUp s)
    , ("M-S-l", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsDown s)

    , ("M-C-k", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsToNewGroupUp s)
    , ("M-C-j", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsToNewGroupDown s)
    , ("M-C-h", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToNewGroupUp s)
    , ("M-C-l", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToNewGroupDown s)
    , ("M-s", sortWindowsWithinGroupStacks)
    -- a permanent sticky layout that will automatically move any new window into the corresponding task group
    , ("M-S-s", do
        h <- getCurrentWorkspaceHandle 
        toggleAutoArrangeWorkspace h)
    -- toggle insert older
    , ("M-C-x", toggleInsertOlder >> runLogHook)
    -- force insert older, while remembering the old toggle
    , ("M-M1-s", toggleInsertOlderForce)
    -- recover for the old toggle
    , ("M-M1-S-s", toggleInsertOlderRecover)
    , ("M-S-,", sendMessage $ G.ToEnclosing $ SomeMessage zoomOut)
    , ("M-S-.", sendMessage $ G.ToEnclosing $ SomeMessage zoomIn)
    , ("M-\\", sendMessage $ G.ToEnclosing $ SomeMessage zoomReset)
    , ("M--", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomOut)
    , ("M-S-=", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomIn)
    , ("M-=", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomReset)
    , ("M-S-<Return>", sendMessage $ G.Modify G.swapGroupMaster)
    , ("M-<Return>", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupMaster)
    , ("M-<Tab>", switchFocusFloat)
    , ("M-S-<Tab>", switchFocusFloat)
    -- cycling of the window styles
    , ("M-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Next)
    , ("M-S-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Prev)
    ]


promptCommands = 
    [ ("M-a c", initMatches >>= \r -> renameWorkspacePrompt (myXPConfig r) {searchPredicate = repeatedGrep}) ]
    ++
    [ ("M-"++mf++"a "++af, initMatches >>= \r -> newWorkspacePrompt (myXPConfig r) {searchPredicate = repeatedGrep, autoComplete = Nothing} (prompt++" ("++(show pos)++")") pos action)
    | (mf, action, prompt) <- [ ("", windows . W.greedyView, "workspace")
                              , ("S-", windows . W.shift, "move to workspace")
                              , ("C-", moveGroupStackToWorkspace, "move group to workspace")]
    , (af, pos) <- [ ("i"  , Before)
                   , ("M-i", Before)
                   , ("S-i", Head)
                   , ("a"  , After)
                   , ("M-a", After)
                   , ("S-a", Last) ]
    ]
    ++
    [ ("M-r", initMatches >>= \r -> dynamicPrompt (myXPConfig r) { 
              changeModeKey = xK_VoidSymbol
            , searchPredicate = repeatedGrep
            , promptKeymap = M.fromList $ (M.toList $ myXPKeymap r)++[
                  ((myModMask, xK_r), quit)
                , ((myModMask, xK_b), setInput "vb " >> endOfLine)
                , ((myModMask, xK_c), setInput "calc " >> endOfLine)
                , ((myModMask, xK_z), setInput "tk " >> endOfLine)
                , ((myModMask, xK_d), do
                        str <- getInput
                        let ls = ["sdcv-Collins ", "sdcv-Moby ", "sdcv-modernChinese", "sdcv-bigChinese"]
                            nstr = case findIndex (`isPrefixOf` str) ls of 
                                       Just i | (ls !! i) == str -> ls !! ((i+1) `mod` (length ls))
                                              | otherwise -> ls !! i
                                       _ -> head ls
                        setInput nstr
                        endOfLine
                   )
            ]})
    , ("M-y", initMatches >>= \r -> mkFMCPrompt (myXPConfigWithQuitKey (myModMask, xK_y) r) {
            autoComplete = Just 0
      })
    -- xmonad commands
    , ("M-C-,", initMatches >>= xmonadPrompt . myXPConfig)
    ]
    -- search interface
    ++
    [ ("M-"++mf++"/", initMatches >>= \r -> mkSearchPrompt (myXPConfig r) {searchPredicate = repeatedGrep} p validWindow a)
    | (mf, p, a) <- [ ("", "Go to window: ", deminimizeFocus)
                    , ("S-", "Bring window: ", shiftWindowsHere . wrapList)
                    , ("C-", "Delete window: ", focusNextKillWindow)
                    ]
    ]


miscCommands toggleFadeSet = 
    [ ("M-<Esc>", exitToNormalMode)
    , ("M-<L>", withFocused $ keysMoveWindow (-10,0))
    , ("M-<R>", withFocused $ keysMoveWindow (10,0))
    , ("M-<U>", withFocused $ keysMoveWindow (0,-10))
    , ("M-<D>", withFocused $ keysMoveWindow (0,10))
    , ("M-S-<L>", withFocused $ keysAbsResizeWindow (-10,0) (0,0))
    , ("M-S-<R>", withFocused $ keysAbsResizeWindow (10,0) (0,0))
    , ("M-S-<U>", withFocused $ keysAbsResizeWindow (0,-10) (0,0))
    , ("M-S-<D>", withFocused $ keysAbsResizeWindow (0,10) (0,0))
    -- }}}
    , ("M-g u", U.withUrgents $ flip whenJust deminimizeFocus . listToMaybe)
    , ("M-g u", U.withUrgents $ flip whenJust deminimizeFocus . listToMaybe)
    ----- wallpaper invoke -- {{{
    , ("M-z", initMatches >>= \r -> mkWPPrompt toggleFadeSet (myXPConfigWithQuitKey (myModMask .|. controlMask, xK_Return) r) {
          autoComplete = Just 0
        , searchPredicate = repeatedGrep
    })
    -- kill command (note this is different from the standard cut
    , ("M-x", getSelectedWindowStack >>= focusNextKillWindows . W.integrate')
    , ("M-M1-q", spawn myRestartCmd)
    -- we should also kill the processes and then exit
    , ("M-M1-S-q", spawn myKillCmd >> io (exitWith ExitSuccess))
    , ("<F10>", spawn "amixer get Master | fgrep '[on]' && amixer set Master mute || amixer set Master unmute")
    , ("<F11>", spawn $ "amixer set Master 5-; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F12>", spawn $ "amixer set Master 5+; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    ]

workspaceCommands = 
    [ ("M-6", lastWorkspaceTag >>= windows . W.view )
    , ("M-S-6", lastWorkspaceTag >>= windows . W.shift)
    , ("M-C-6", lastWorkspaceTag >>= swapWith)
    , ("M-4", allWorkspaceTags >>= toggleTag . last >>= windows . W.view)
    , ("M-S-4", allWorkspaceTags >>= toggleTag . last >>= windows . W.shift)
    , ("M-C-4", allWorkspaceTags >>= toggleTag . last >>= swapWith)
    , ("M-[", doTo Prev validWSType myWorkspaceSort (windows . W.greedyView))
    , ("M-]", doTo Next validWSType myWorkspaceSort (windows . W.greedyView))
    , ("M-S-[", doTo Prev validWSType myWorkspaceSort (windows . W.shift))
    , ("M-S-]", doTo Next validWSType myWorkspaceSort (windows . W.shift))
    , ("M-C-[", modSwapTo Prev)
    , ("M-C-]", modSwapTo Next)
    -- }}}
    ----- Loghook-- {{{
    {-, ("M-S-f", withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)-}
    -- }}}
    ]
----- QuickWorkspace-- {{{
    ++
    [ ("M-"++m++t, f t)
    | (m, f) <- [ ("", \t -> toggleTag t >>= viewWorkspace)
                , ("S-", moveToWorkspace)
                , ("C-", \t -> toggleTag t >>= swapWith)
                , ("C-S-", moveGroupStackToWorkspace)] 
    , t <- quickWorkspaceTags]

-- visual mode we need a new datastructure to store the selections
-- the first bool stores the active selection while the second stores the passive ones
-- we need to pass a selection mode into the function to select the right windows
data VisualMode = Win | Row | Col deriving (Show, Read, Eq)
data XMonadMode = Normal | Visual VisualMode deriving (Show, Read, Eq)
data XMonadModeStorage = XMS XMonadMode deriving Typeable
instance ExtensionClass XMonadModeStorage where
    initialValue = XMS Normal

exitToNormalMode = do
    -- clear the m-o m-i marks
    clearAllMarks
    inv <- isInVisualMode
    if inv then exitVisualMode
           else clearAllSelections >> refresh

data VisualSelections = VisualSelections (S.Set Window, S.Set Window) deriving (Typeable, Read, Show)
instance ExtensionClass VisualSelections where
    initialValue = VisualSelections (S.empty, S.empty)
    extensionType = PersistentExtension

-- the beginning of the window and the end of the window
data VisualModeMarker = VisualModeMarker (Maybe ((Window, Window), (Window, Window))) deriving (Typeable, Read, Show)
instance ExtensionClass VisualModeMarker where
    initialValue = VisualModeMarker Nothing
    extensionType = PersistentExtension

toggleVisualMode mode = do
    XMS m <- XS.get
    case m of
         Visual mode' | mode' == mode -> exitVisualMode
         _ -> enterVisualMode mode

allDiff a b = S.difference  (S.union a b) (S.intersection a b)
enterVisualMode mode = withFocused $ \f -> do
    XS.put (XMS (Visual mode)) 
    -- clear marker
    XS.put $ VisualModeMarker Nothing
    -- clear active selections first
    VisualSelections (ac, pass) <- XS.get
    XS.put $ VisualSelections (S.empty, pass)
    -- update 
    visualModeUpdateEndMarker f
    refresh
visualModeUpdateToggleSet ls = do
    VisualSelections (ac, pass) <- XS.get
    XS.put $ VisualSelections $ (allDiff ac $ S.fromList ls, pass)
maybeToMaybe = maybe Nothing
visualModeUpdateEndMarker w = do
    XMS mode <- XS.get
    case mode of
         Visual vm -> do
                -- we have to obtain the new selection range
                wins <- allOrderedWindows
                let rangeInGStacks (G.Leaf s) = (w, w)
                    rangeInGStacks (G.Node s) = 
                        case find (w `elem`) $ fmap G.flattened ls of 
                              Just fls | not (null fls) -> (head fls, last fls)
                              _ -> (w, w) 
                            where ls = W.integrate s
                    computeDiff (a, b) (c, d) =
                        let gi = flip elemIndex wins
                        in case (gi a, gi b, gi c, gi d) of
                             (Just ai, Just bi, Just ci, Just di) -> 
                                let (fbi', fei') = head $ sortBy (\(xi, yi) (wi, zi) -> compare (abs $ wi - zi) (abs $ xi - yi)) $ [(ai, ci), (ai, di), (bi, ci), (bi, di)]
                                    (fbi, fei) = if fei' >= fbi' then (fbi', fei') else (fei', fbi')
                                in S.fromList $ drop fbi $ take (fei + 1) wins
                             _ -> S.empty
                (wb, we) <- case vm of
                                 Win -> return (w, w)
                                 -- row uses the bases function
                                 Row -> fmap (maybeToMaybe G.bases) G.getCurrentGStack >>= return . maybe (w, w) rangeInGStacks
                                 Col -> G.getCurrentGStack >>= return . maybe (w, w) rangeInGStacks
                VisualSelections (ac, pass) <- XS.get
                VisualModeMarker mm <- XS.get 
                case mm of
                     Just ((bb, be), (eb, ee)) -> do
                         XS.put $ VisualSelections (allDiff ac $ allDiff nws ows, pass)
                         XS.put $ VisualModeMarker $ Just ((bb, be), (wb, we)) 
                        where ows = computeDiff (bb, be) (eb, ee)
                              nws = computeDiff (bb, be) (wb, we)
                     Nothing -> do
                         XS.put $ VisualSelections (computeDiff (wb, we) (wb, we), pass)
                         XS.put $ VisualModeMarker $ Just ((wb, we), (wb, we)) 
         _ -> return ()

exitVisualMode = do
    XS.put (XMS Normal) 
    -- lets commit all active selections to the passive ones
    VisualSelections (ac, pass) <- XS.get
    XS.put $ VisualSelections (S.empty, allDiff ac pass)
    refresh
isInVisualMode = XS.get >>= \(XMS m) -> case m of
                                             Visual _ -> return True
                                             _ -> return False
ifInVisualMode a b = isInVisualMode >>= \inv -> if inv then a else b
windowIsSelected w = do
    VisualSelections (ac, pass) <- XS.get 
    return $ S.member w $ allDiff ac pass
windowIsSelectedQuery = ask >>= liftX . windowIsSelected
deleteWindowsSelection [] = return ()
deleteWindowsSelection ls = do
    VisualSelections (ac, pass) <- XS.get 
    let ft = S.filter (not . (`elem` ls))
    XS.put $ VisualSelections (ft ac, ft pass)
markWindowsSelection ls = do
    VisualSelections (ac, pass) <- XS.get 
    XS.put $ VisualSelections (S.filter (not . (`elem` ls)) ac, foldr S.insert pass ls)
clearAllSelections = XS.put $ VisualSelections (S.empty, S.empty)

getSelectedWindowStack' = do
    mf <- gets (W.peek . windowset)
    inv <- isInVisualMode
    VisualSelections (ac, pass) <- XS.get 
    let currs = allDiff ac pass
        ft = flip S.member currs
        filterBase fil os@(G.Leaf (Just (W.Stack f u d)))
           | fil f = if length fu == length u && length fd == length d && length fu + length fd > 0 
                       then (Just os, Just (init ols, last ols))
                       else (Just $ G.Leaf $ Just $ W.Stack f fu fd, Nothing)
           | not (null fd) = (Just $ G.Leaf $ Just $ W.Stack (head fd) fu (tail fd), Nothing)
           | not (null fu) = (Just $ G.Leaf $ Just $ W.Stack (head fu) (tail fu) [], Nothing)
           | otherwise = (Nothing, Nothing)
              where fu = filter fil u
                    fd = filter fil d
                    ols = G.flattened os
        filterBase fil (G.Node (W.Stack f u d)) = 
            let f' = pl [f]
                u' = pl u
                ugs = fst $ unzip u'
                d' = pl d
                dgs = fst $ unzip d'
                pl = fmap (\(a, b) -> (fromJust a, b)) . filter (isJust . fst) . fmap (filterBase fil)
                mvs = catMaybes $ snd $ unzip $ reverse $ (reverse u')++f'++d'
                mvws = concatMap fst mvs
                lw = snd $ head mvs
                mops = if null mvws then Nothing else Just (mvws, lw)
            in case (f', ugs, dgs) of
                    ([(fg, _)], _, _) -> (Just $ G.Node $ W.Stack fg ugs dgs, mops)
                    ([], _, dh:dl) -> (Just $ G.Node $ W.Stack dh ugs dl, mops)
                    ([], uh:ul, _) -> (Just $ G.Node $ W.Stack uh ul dgs, mops)
                    _ -> (Nothing, mops)
        filterBase _ _ = (Nothing, Nothing)
    mbases <- fmap (maybeToMaybe G.bases) G.getCurrentGStack 
    let mb = if inv then mbases else maybeToMaybe G.current mbases
    return $ if isNothing mb 
               then (Nothing, Nothing)
               else let bs = fromJust mb
                    in case (filterBase ft bs, mf) of
                        ((Just gs, ops), _) -> (G.toZipper gs, ops)
                        (_, Just f) -> (W.differentiate [f], Nothing)
                        _ -> (Nothing, Nothing)

getSelectedWindowStack = fmap fst getSelectedWindowStack'

applySelectedWindowStack :: Bool -> (Maybe (W.Stack Window) -> X ()) -> X ()
applySelectedWindowStack reversal fun = do
    ss <- getSelectedWindowStack'
    let rev (W.Stack f u d) = if reversal then W.Stack f d u else W.Stack f u d
        deselect (W.Stack f [] []) = deleteWindowsSelection [f]
        deselect _ = return ()
    case ss of
         (Just os@(W.Stack f _ _), Just (mvs, mf)) -> do
                                           let wins = W.integrate os
                                               remains = filter (not . (`elem` mvs)) wins
                                           -- temporarily move those windows to the scratchpad wokr
                                           curr <- gets (W.currentTag . windowset)
                                           windows $ \s -> W.focusWindow mf $ foldr (W.shiftWin scratchpadWorkspaceTag) s mvs
                                           fun $ Just $ W.Stack mf [] [] 
                                           -- move the remaining windows to the temp workspace
                                           windows $ \s -> 
                                                let s' = foldr (W.shiftWin scratchpadWorkspaceTag) s remains
                                                    s'' = foldr (W.shiftWin curr) s' wins
                                                in W.focusWindow f s''
         (Just os, _) -> let s = rev os in deselect s >> fun (Just s)
         _ -> return ()
    -- refresh 
    refresh

paste invertInsert normalP register = do
    InsertOlderToggle t <- XS.get
    wins <- case register of 
                 -- use the default register
                 "" -> do
                     -- first get the register content
                     -- we need to push the windows in the reverse direction
                     let mv t rep nrep = case t of
                             "\"" -> if not (null rep) && filter (not . (`elem` nrep)) rep == [] 
                                        -- keep on moving
                                        then mvWindows "9" mv []
                                        else return rep
                             "1" -> mvWindows "\"" mv rep
                             _ -> mvWindows (show $ (read t)-1) mv rep
                     mvWindows "9" mv []
                 _ -> orderedWindowsMatchingPredicate (hasTagQuery register) 
    if not (null wins)
       then do
            markWindowsSelection wins
            let t' = if invertInsert then not t else t
            case (t', normalP) of
                 (False, True) -> shiftWindowsHereAndFocusLast True (Just (head wins)) wins
                 (False, False) -> shiftWindowsHereAndFocusLast False (Just (head wins)) wins
                 (True, True) -> shiftWindowsHereAndFocusLast True Nothing wins
                 (True, False) -> shiftWindowsHereAndFocusLast False Nothing wins
       else return ()

-- a register of "" means no register shall be used (in that case we just push into the default list of registers
cut register wins = do
    minimizeWindows wins
    if register /= ""
       -- put these windows into that register
       then putWindowsIntoRegister register wins 
       else let mv t rep _ = case t of
                    -- we should check if this is the only tag the windows have
                     "9" -> filterM (\w -> do
                            ts <- getTags w
                            return $ ts == ["9"] 
                         ) rep >>= focusNextKillWindows
                     _ -> mvWindows (show $ (read t)+1) mv rep 
            -- pushing these windows into 1, 1->2, 9 out of the memory
            in mvWindows "1" mv wins 
    putWindowsIntoRegister "\"" wins 

mvWindows nr fun wins = do
    rep <- orderedWindowsMatchingPredicate (hasTagQuery nr) 
    mapM_ (delTag nr) rep
    mapM_ (addTag nr) wins
    fun nr rep wins

putWindowsIntoRegister register wins = do
    orderedWindowsMatchingPredicate (hasTagQuery register) >>= mapM_ (delTag register) 
    mapM_ (addTag register) wins


-- make the keystrokes easier to type
processKey (k, a) = 
    let keys = words k
        -- find all adjacent pairs which have the same key (and append them with M-)
        pks ks = case findIndex (isPrefixOf "M-") ks of
                     Just i | i + 1 < length ks -> case (fromJust $ stripPrefix "M-" wi, fromMaybe wa $ stripPrefix "M-" wa, splitAt i ks) of
                                                        -- attach both keys with the prefix
                                                        (a, b, (bf, _:_:af)) | a == b -> bf ++ (wi:(pks (wi:af)))
                                                                             | otherwise -> bf ++ (wi:(pks (wa:af)))
                                                        _ -> ks
                                        where wi = ks !! i
                                              wa = ks !! (i+1)
                     _ -> ks
        p = joinStr " " (pks keys)
    in [(k,a)] ++ if p /= k then [(p,a)] else []

addPrefix (k, a) = ("M-"++k, a)
 

myKeys toggleFadeSet = 
    visualCommands
    ++
   (appendActionWithException (\_ -> ifInVisualMode exitVisualMode (return ()))  motionKeyCommands
    $
    appendActionWithException saveLastCommand [ ("M-.", getLastCommand >>= id) ] 
    $
    cutCommands
    ++
    pasteCommands
    ++
    markCommands
    ++
    cloneCommands
    ++
    layoutCommands
    ++
    promptCommands
    ++
    historyCommands
    ++
    miscCommands toggleFadeSet
    ++
    workspaceCommands)
-- }}}

myXMonadConfig = defaultConfig { 
        manageHook = myManageHook 
        , terminal = myTerminal
        , workspaces = [scratchpadWorkspaceTag, tmpWorkspaceTag]
        , startupHook = myStartupHook
        , modMask = myModMask
        , layoutHook = myLayout
        , focusFollowsMouse = False
        , borderWidth = 2
        , normalBorderColor = myBgColor
        , focusedBorderColor = color6
        , handleEventHook = myHandleEventHook
        }

---------------- Main -- {{{
main = do
    toggleFadeSet <- newIORef S.empty
    dzenLogBar <- myStatusBars
    -- urgencyhook is not used currently due to conflict with the wallpaper system
    xmonad $ ewmh $ U.withUrgencyHook U.NoUrgencyHook $ myXMonadConfig {
            logHook = myLogHook toggleFadeSet dzenLogBar 
        } `additionalKeysP` myKeys toggleFadeSet

-- }}}
