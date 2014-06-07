{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, ImpredicativeTypes #-}
---------------- Import statements-- {{{
import Prelude hiding (mapM)
import Control.Concurrent (threadDelay)
import Data.Char
import Data.IORef
import Data.List hiding (delete)
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Monoid (mempty, All(..), appEndo)
import Data.Traversable hiding (sequence)
import Control.Monad hiding (mapM)
import Control.Exception.Extensible as E
import Foreign.C.Types (CLong)
import System.IO
import System.Exit
import System.Directory
import System.Time
import Text.Read
import XMonad
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
import XMonad.Hooks.OnWindowsInserted
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
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Stack
import XMonad.Util.Themes
import XMonad.Util.Timer
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
import XMonad.Actions.PhysicalScreens
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
-- spawn s = xfork (executeFile "/bin/sh" False ["-c", s] Nothing) >> return ()

-- runProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
-- runProcessWithInput cmd args input = io $ do
--     (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
--     hPutStr pin input
--     hClose pin
--     output <- hGetContents pout
--     when (output == output) $ return ()
--     hClose pout
--     hClose perr
--     -- no need to waitForProcess, we ignore SIGCHLD
--     return output

-- debug
logger s = spawn $ "echo \"`date`: \"" ++ escapeQuery s ++ " >> ~/.xmonad/xmonad.log"

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
    , searchPredicate = infixSearchPredicate
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
tmpWorkspaceTag = [head quickWorkspaceSequence]
quickWorkspaceTags = map wrapList quickWorkspaceSequence
quickWorkspaceSequence = "`12345789"
-- The symbolSequence replaces the originalSequence in the locale, thus enabling custom sorting
symbolSequence   = filter (/= '6') $ fullSequence quickWorkspaceSequence
-- the workspace tags are implemented as a sliding window across the symbol stream; at any instant the tags are a list counting from a specific symbol
symbolStream = symbolSequence ++ filter (not . (`elem` symbolSequence)) (enumFrom (toEnum 0))

--- subgroup symbol sequence: we eliminated '`' due to its ugliness from the subgroup indexing stream; and note that the sub group doesn't necessarily need an index after all
-- this is for tab keys
subgroupSymbolSequence = "1234567890-="
extendedSequence s = filter (not . (`elem` s)) typeables
fullSequence s = s ++ extendedSequence s
columngroupSymbolSequence = "123456789"
indexToSymbol s n = if n >= 0 && n < length s then Just [s !! n] else Nothing
subgroupIndexToSymbol = indexToSymbol $ fullSequence subgroupSymbolSequence
-- columngroupIndexToSymbol = indexToSymbol columngroupSymbolSequence

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
deminimizeFocus w = do
    deminimize w 
    -- we will check if w is an Intellij window, if yes we need to refresh after focusing it
    -- b <- runQuery (filterPredicate intellijTaskGroup) w
    focus w
    -- if b then do
    --         gs <- G.getCurrentGStack
    --         case gs of
    --              Just (G.Node (W.Stack _ u _)) -> sendMessage $ G.Modify $ G.focusGroupAt $ length u 
    --              _ -> return ()
    --      else return ()
deminimize w = routeMessageToWS ((w `elem`) . W.integrate' . W.stack) (RestoreMinimizedWin w)
routeMessageToWS fun mess = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    case find fun wss of
         Just ws
            | W.tag ws /= curr -> sendMessageWithNoRefresh mess ws 
            | otherwise -> sendMessage mess
         _ -> return ()



isMinimized = ask >>= \w -> liftX $ do
    wss <- allWorkspaces
    wins <- mapM getMinimizedWindows $ fmap W.layout wss
    return $ w `elem` (concat $ wins)

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

-- a fuzzy kind of comparator that handles case such as f5.pdf vs f10.pdf
compareFileNames [] [] = EQ
compareFileNames [] b = LT
compareFileNames a [] = GT
compareFileNames a b = 
    case (span isDigit a, span isDigit b) of
         ((ab, aa), ([], _)) -> let ((ab', aa'), (bb', ba')) = (break isDigit aa, break isDigit b)
                                    ta' = ab ++ ab'
                                in if ta' == bb' then compareFileNames aa' ba'
                                                 else compare a b
         (([], _), (bb, ba)) -> compare a b
         ((ab, aa), (bb, ba)) -> case compare ai bi of
                                      EQ -> compareFileNames aa ba
                                      r -> r
                where ai = read ab :: Int
                      bi = read bb :: Int

-- sort the windows in the innermost group according to the title
sortBaseCurrentWindows = do
    wins <- gets (W.allWindows . windowset)
    titles <- mapM (runQuery title) wins
    let pairs = zip wins titles
        sort s@(W.Stack f u d) = st
            where wins = sortBy (\a b -> compareFileNames (tit a) (tit b)) $ W.integrate s
                  st = case break (==f) wins of
                            (bs, nf:as) -> W.Stack nf (reverse bs) as
                            (bs, _) -> fromJust $ W.differentiate bs 
                  tit w = case find ((==w) . fst) pairs of
                                    Just (_, t) -> t
                                    _ -> ""
    sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.modifyFocusedStack sort
    


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

killWindows ls = flip correctFocus ls $ \wins -> do
    deleteWindowsSelection wins
    -- now just kill all windows that 
    mapM_ killWindow wins

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
            case (isf, f `elem` wins, mgs) of
                 (True, True, _) -> do 
                    -- nextMatch History $ isInCurrentWorkspace <&&> fmap not isFloating
                    maybe (return ()) focus $ maybeToMaybe G.focal mgs 
                    -- focus to the current focus in the stack
                    a wins 
                 (False, True, Just gs) -> let correctf = maybeToMaybe G.focal $ G.filter (not . (`elem` wins)) gs
                                           in maybe (return ()) focus correctf >> a wins
                 _ -> a wins
         _ -> a wins

-- this is conceptually equivalent to navigating back to the last window that's not on the same layer
-- switchFocusFloat = withFocused $ \f -> do
--     isf <- runQuery isFloating f
--     nextMatch History $ isInCurrentWorkspace <&&> fmap (if isf then not else id) isFloating
switchFocusFloat = switchLayer
-- }}}

---------------- LogHook -- {{{

---- fade Log hook-- {{{
------- the conditions for fading windows in normal circumstances
-- not used currently
isFloating :: Query Bool
isFloating =  ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset
-- windows that shall not be faded no matter what
doNotFadeOutWindows =  className =? "xine" <||> className =? "MPlayer"

myLogHook toggleFadeSet dzenLogBar = do
    -- for java 
    takeTopFocus
    statusLogHook dzenLogBar
    historyHook
    fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount
    onWindowsInsertedLogHook

-- windows that shall not be faded given the class name
-- disableFadingWithinClassName cn = liftM not (className =? cn <&&> (liftX  $ focusedHasProperty (ClassName cn)))
-- disableFadingWithinClassNames = disableFadingWithinClassName "jetbrains-idea"
   
-- tests if a window should be faded; floats are the windows that have been toggled by the user to not fade
defaultFadeTest floats =
    -- liftM not doNotFadeOutWindows <&&> isUnfocused <&&> disableFadingWithinClassNames <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)
    liftM not doNotFadeOutWindows <&&> fmap not isFocused <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

isFocused = ask >>= \w -> liftX $ do
    fs <- getFocusStack
    return $ fmap W.focus fs == Just w
     
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
    MMS m <- XS.get
    let (plus, c) = if s == 0 then (" ", myFgColor) else ("+", myNotifyColor)
        [lb,rb] = fmap (dzenColor myFgColor myBgColor) $ ["[","]"]
        autoIndicator = if isAuto then dzenColor myNotifyColor myBgColor "*" else "#"
        insertOlderIndicator = if t then dzenColor myNotifyColor myBgColor "|" else "<"
        modeIndicator = case mode of
                             Visual Win -> dzenColor myNotifyColor myBgColor "VW" 
                             Visual Row -> dzenColor myNotifyColor myBgColor "VR" 
                             Visual Col -> dzenColor myNotifyColor myBgColor "VC" 
                             _ -> "Nm"
        recordingIndicator = case m of
                                  Just (name, _) -> dzenColor myNotifyColor myBgColor $ "● " ++ name
                                  _ -> "■  "
    return $ Just $ recordingIndicator ++ " | " ++ modeIndicator++ " " ++ lb ++ autoIndicator++insertOlderIndicator++ rb ++ lb ++ "-" ++dzenColor c myBgColor plus ++ rb

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

colorTaskGroupName shorten spc nc bg g
    | not $ null (filterKey g) = splitFst (filterKeyList $ filterKey g) spc nc (taskGroupName g)
    | otherwise =  dzenColor nc bg (taskGroupName g) 
        where splitFst cs spc nc str = if null cs then dzenColor nc bg (if shorten then [head str] else str)
                                                  else let h = head cs 
                                                           fspc = dzenColor spc bg [h]
                                                           in if shorten 
                                                                 then fspc 
                                                                 else case break (\ch -> toLower ch == h || toUpper ch == h) str of
                                                                               (a, x:xs) -> (dzenColor nc bg a)++fspc++(splitFst (tail cs) spc nc xs)
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
    let wl = zip (fmap ((++".") . wrapList) (fullSequence columngroupSymbolSequence) ++ repeat "") $ repeat ""
        nl = G.level ms
    -- need to get the number of levels inside the struct
    in infoFromTaggedGStack' False ("  ":repeat "") ("","") (wl:(repeat $ repeat ("[","]"))) (myNotifyColor, "/") (take (nl-1) (repeat (myFgColor, myFgColor)) ++ [(myTextHLight, myFgColor)]) myBgColor ms
infoFromTaggedGStack _ = ""

-- this takes the highlight color for the focused group, and then a function that transform a group to a string
-- delimit: the delimit string to separate the groups / task groups
-- fkeyColor: the filter key color (only used in the base case)
-- focusHL, fgColor: the focusColor for the current group, the fgColor for other groups
-- bgColor: the background color (should be uniform across)
infoFromTaggedGStack' shorten (delimit:dls) (wrapl,wrapr) (wl:wraps) (fkeyColor,tgdelimit) ((focusHL,fgColor):cps') bgColor ms = 
     wrap (nc wrapl) (nc wrapr) $ case ms of
                                G.Leaf s@(Just (W.Stack (_,ftg) u d)) -> joinStr (nc tgdelimit) $ fmap (\g -> colorTaskGroupName shorten fkeyColor (if g == ftg then focusHL else fgColor) bgColor (fromMaybe def g)) $ nub $ snd $ unzip $ W.integrate' s
                                G.Leaf Nothing -> ""
                                G.Node s@(W.Stack fs us dss) -> 
                                    let ncps m
                                            | m==fs = fmap (\(hl, fg)->(hl, focusHL)) cps'
                                            | otherwise = fmap (\(hl, fg)->(fgColor, fg)) cps'
                                        in joinStr (nc delimit) $ fmap (\(wp, mn) ->infoFromTaggedGStack' shorten dls wp wraps (fkeyColor,tgdelimit) (ncps mn) bgColor mn) $ zip wl $ W.integrate s
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
        , ppOrder   =  \(ws:l:t:hist:gp:hid:xs) -> [hist,ws,l,hid,gp] ++ xs ++ [t]
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
                               ls <- fmap reverse getCurrentMinimizedWindows
                               -- we first check for all the tags they have
                               regs <- fmap (reverse . sortRegs . S.toList . S.fromList . concat) $ mapM getTags ls
                               -- now for each tag we scan through the list to get all the windows
                               let pinfo = infoFromTaggedGStack' True (repeat "") ("","") (repeat $ repeat ("","")) (myNotifyColor, "/") (repeat (myFgColor, myFgColor)) myBgColor
                                   toTg l = taggedGStack taskGroups (G.fromZipper (W.differentiate $ reverse l) 1) 
                                   prinfo (r, info) = "'"++r++":"++info
                               rwins <- mapM (\r -> filterM (hasTag r) ls) $ regs
                               starwins <- filterM (fmap null . getTags) ls
                               let pairs = (if not (null starwins) then [("*", starwins)] else [])++zip regs rwins
                                   (rls, winls) = unzip pairs
                               tgs <- mapM toTg winls
                               return $ Just $ joinStr "|" $ fmap prinfo $ zip rls (fmap pinfo tgs)
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
      onWindowsInsertedManageHook
    , insertPositionHook
    -- , autoArrangeHook taskGroups
    -- , dynamicMasterHook
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
workspaceStack = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    return $ case break ((==curr) . W.tag) wss of
                    (bf, c:af) -> Just $ W.Stack c (reverse bf) af
                    _ -> Nothing

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
getCurrentWorkspaceName = getCurrentWorkspaceHandle >>= getWorkspaceName

-- a set of positions to simulate those used in Vim as i,a,I,A respectively
data InsertPosition = Before | After | Head | Last deriving Eq
instance Show InsertPosition where
    show Before = "insert before"
    show After = "insert after"
    show Head = "insert head"
    show Last = "insert last"
insertPositionKeys = [ ("i"  , Before)
                     , ("S-i", Head)
                     , ("a"  , After)
                     , ("S-a", Last) ]

idleWorkspace allowIdle insp = do
    curr <- currWorkspace
    wss <- fmap (filter ((/=tmpWorkspaceTag) . W.tag)) allWorkspaces
    p <- isRecyclableWS
    let currTag = W.tag curr
    if p curr && allowIdle
       then return currTag
       else do
           let (candi, insi, ch) = case (insp, findIndex ((== (W.tag curr)) . W.tag) wss) of
                                (Before, Just i) -> (i-1, i, head currTag)
                                (After, Just i) -> (i+1, i+1, nextSymbol $ head currTag)
                                (Last, _) -> (length wss - 1, length wss, nextSymbol $ head $ W.tag $ last wss)
                                _ -> (0, 0, nextSymbol $ head tmpWorkspaceTag)
               cand = wss !! candi
           if candi >= 0 && candi < length wss && p cand && allowIdle
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
    t <- idleWorkspace (n /= "") insp
    setWorkspaceNameByTag t n
    return t

isRecyclableWS = do
          -- get the names for the workspace
          wns <- getWorkspaceNames
          return $ \w -> W.tag w /= tmpWorkspaceTag && not (':' `elem` (wns $ W.tag w)) && validWS w && isNothing (W.stack w) 

-- the symbol stream is defined by (fullsequence quickworkspace)
symbolFrom :: Char -> String
symbolFrom ch = dropWhile (/=ch) symbolStream

compareSymbol a b
    | a == b = EQ
    | otherwise = case dropWhile (\s -> s /= a && s /= b) symbolStream of
                        (h:_) | h == a -> LT
                              | otherwise -> GT
                        _ -> EQ

nextSymbol = head . tail . symbolFrom

compareLists _ [] [] = EQ
compareLists _ [] _ = LT
compareLists _ _ [] = GT
compareLists f (a:as) (b:bs) = case f a b of
                                    EQ -> compareLists f as bs
                                    r -> r

-- a version that ranks empty lists last
compareLists' _ [] [] = EQ
compareLists' _ [] _ = GT
compareLists' _ _ [] = LT
compareLists' f a b = compareLists f a b

compareSymbolString = compareLists compareSymbol
getSymbolStringSort = mkWsSort $ return compareSymbolString

---- viewWorkspace: this is relatively easy; just show up a prompt with all workspace names; upon action split the string by : and shift to the workspace by id

quickWorkspace tag = do
    wsts <- allWorkspaceTags
    if length tag == 1
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

-- the onselectedwindows protocol expects: the windows disappear after the action applies
-- the after moving to tmpspace is useful if you want to apply a one-step operation (like shifting)
onSelectedWindowsAfterMovingToTmpSpace a = do
    (st, opts) <- getSelectedWindowStack' False
    flip correctFocus (W.integrate' st) $ \wins -> do
        case opts of
             Just (mvs, lw) -> windows $ \s -> foldr (W.shiftWin scratchpadWorkspaceTag) s mvs
             _ -> return ()
        a wins
onSelectedWindows a = do
    (st, _) <- getSelectedWindowStack' False
    a $ W.integrate' st

shiftWins t wins s = foldr (W.shiftWin t) s wins

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

newWorkspacePrompt conf prompt insp iter f = workspacePrompt conf prompt (\t n -> f t) (\n p -> insertWorkspace n insp iter p f)

-- insert iter number of workspaces at the specified location and then apply a given function to the last
-- one
insertWorkspace name insp iter path fun = do
    let cw = do
        t <- reuseHiddenIdleWorkspaceWithName name insp 
        saveWorkspaceDirectory path t 
        return t
    res <- sequence $ take iter $ repeat $ cw
    case res of
         [] -> return ()
         h:_ -> fun $ last res

---- rename the current workspace
-- for the time being because we want to allow the user some freedom of actually renaming the workspace after assigning a workspace directory to it, so what we do is that we'll only reassign the directory when necessary
renameWorkspacePrompt conf immi final = workspacePrompt conf "Rename workspace" (\t n -> setCurrentWorkspaceName n) (\n p -> do
    if not (null p) then saveCurrentWorkspaceDirectory p
                    else return ()
    setCurrentWorkspaceName n
    immi
    final) 

wrapList c = [c]

removeCurrentWorkspace = gets (W.currentTag . windowset) >>= removeWorkspace
removeWorkspace = removeWorkspaces . wrapList
removeAllWorkspaces = allWorkspaceTags >>= removeWorkspaces
removeWorkspaces tags = do
    -- remove all windows
    wst <- workspaceStack
    curr <- gets (W.currentTag . windowset)
    let todel = filter (dp tags) $ W.integrate' wst
        ntags = filter (/= tmpWorkspaceTag) tags
        dp ts = (`elem` ts) . W.tag
        nwst = maybeToMaybe (W.filter (not . dp ntags)) wst
    killWindows $ concatMap (W.integrate' . W.stack) todel
    case nwst of
         Just (W.Stack nf _ _) | nft /= curr -> windows $ W.greedyView nft
                where nft = W.tag nf
         _ -> return ()
    -- kill the scratchpads matching this workspace
    mapM_ (\t -> ifWindows (isPerWSScratchpadBoundToWS t) (mapM_ killWindow) (return ())) ntags
    windows $ \s -> foldr removeWorkspaceByTag s ntags
    renameWorkspaces (zip (fmap W.tag (W.integrate' nwst)) $ fmap wrapList $ symbolStream)
    if length ntags < length tags then setWorkspaceNameByTag tmpWorkspaceTag "" >> saveWorkspaceDirectory "" tmpWorkspaceTag
                                  else return ()

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

defaultWPChannel = "#top"

data WallpaperPrompt = WPPrompt String

instance XPrompt WallpaperPrompt where
    showXPrompt (WPPrompt dir) = dir ++ " > "
    commandToComplete _ = id
    nextCompletion _ c l = let (cmd, arg) = splitArg c 
                           in if cmd `isCmdPrefixOf` "setch"
                                 then "setch " ++ (l !! exactMatchIndex (unescape arg))
                                 else if cmd `isCmdPrefixOf` "wallbase"
                                 then "wallbase " ++ (l !! exactMatchIndex (unescape arg))
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
    initialValue = WPChannel defaultWPChannel
    extensionType = PersistentExtension

compareWithPriorList fun ls a b = 
    let r = reverse ls
    in case compare (fun b r) (fun a r) of
        EQ -> compare a b
        c -> c
compareWithPostList fun ls a b =
    case compare (fun a ls) (fun b ls) of
        EQ -> compare a b
        c -> c

-- available commands
---- next: go to the next wallpaper
---- setch: change the wallpaper channel
---- rate: tag the current wallpaper favorite (move it to the favorite folder) (and NOT moving it anywhere)
---- ban: move the wallpaper to the trash folder
---- open: open the current wallpaper in feh
wpComplFunc conf str = 
    let (cmd, arg) = splitArg str
    -- the setch command default to use the wallbase
    in if cmd `isCmdPrefixOf` "setch" || cmd `isCmdPrefixOf` "wallbase"
                -- add in the . to denote the base wallpaper directory, and also to prevent auto complete 
                then fmap ((\l -> if null l then l else l++["."]) 
                   . filter (searchPredicate conf arg) 
                   . sortBy (compareWithPriorList elemIndex ["#top", "#new", "#rand", "#favorites"]) 
                   . map (\s -> fromMaybe s (stripPrefix wallpaperDirectory s)) . lines) 
                   $ runProcessWithInput "find" [wallpaperDirectory, "-mindepth", "1", "-type", "d"] ""
                else if cmd `isCmdPrefixOf` "flickr"
                    then return []
                    else return $ filter (isPrefixOf cmd) ["next", "setch", "flickr", "wallbase", "rate", "ban", "trash"]

replace st rep string = joinStr rep $ splitOn st string

wpAction c ch str = 
    let (cmd, arg) = splitArg str
        downloadch ess downloadscript = do
               let changescript = "while [ -d "++ ess ++" ] && [ -z \"`find " ++ ess ++ " \\( -name '*.jpg' -o -name '*.png' \\) -print -quit 2>/dev/null`\" ]; do sleep 1; done"
               runProcessWithInput "/bin/sh" ["-c", "mkdir -p " ++ ess] ""
               spawn $ downloadscript ++ "; rmdir " ++ ess
               runProcessWithInput "/bin/sh" ["-c", changescript] ""
    in if cmd `isCmdPrefixOf` "setch" || cmd `isCmdPrefixOf` "wallbase"
          then do
                io $ setCurrentDirectory wallpaperDirectory
                -- save the arg into the database
                let ess = escapeQuery arg
                    exists = io $ doesDirectoryExist arg
                e <- exists
                if not e || "#" `isPrefixOf` arg || cmd `isCmdPrefixOf` "wallbase"
                        -- transform the wallbase query 
                       then let query = reverse $ takeWhile (/='/') $ reverse arg in
                            downloadch ess $ "wallbase "++ escapeQuery query ++ " " ++ ess
                       else return arg
                whenX (io $ doesDirectoryExist arg) $ do
                   XS.put $ WPChannel arg
                   spawn $ changeWallpaperCmd ++ " " ++ ess
                mkWPPrompt' c c
          else if cmd `isCmdPrefixOf` "flickr"
                then do
                       io $ setCurrentDirectory wallpaperDirectory
                       let searchDir = "#rss/flickr/" ++ arg
                           ess = escapeQuery searchDir
                       downloadch ess $ "rss-image-download "++ escapeQuery ("https://api.flickr.com/services/feeds/photos_public.gne?format=rss_200_enc&tags=" ++ arg) ++ " " ++ ess
                       whenX (io $ doesDirectoryExist searchDir) $ do
                          XS.put $ WPChannel searchDir
                          spawn $ changeWallpaperCmd ++ " " ++ ess
                       mkWPPrompt' c c
          else if cmd `isCmdPrefixOf` "trash"
                then if ch /= "." 
                        then do
                            XS.put $ WPChannel defaultWPChannel
                            spawn $  "dir="++escapeQuery (wallpaperDirectory ++ ch)++"; target=\"$HOME/.Trash/${dir##*/}\"; rm -rf \"$target\"; mv -f \"$dir\" \"$target\"" 
                            spawn $ changeWallpaperCmd ++ " " ++ (escapeQuery $ wallpaperDirectory++defaultWPChannel)
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
    return $ if e then ch' else defaultWPChannel

getWPDirectory = getWPChannel >>= \c -> return $ wallpaperDirectory ++ c

--}}}

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
isSearchFilter (i, s)
    -- just not performance-wise up to the standard for fasd right now.
    -- I have the feeling that the author hasn't done a terribly good job at optimizing this
    -- | i > 0 && s `elem` ["f", "a", "d", "z"] = True
    | i > 0 && s `elem` ["h", "l", "t", "which", "diff"] = True 
    -- we let the application to decide what to do after that
    | i > 0 && s `elem` ["g", "gp"] = True
    | i == 0 && (length s) `elem` [1, 2] && head s == '\'' && not ((last s) `elem` "v") = True
    | i > 0 && length s == 2 && head s == 'm' && not ((last s) `elem` "v") = True
    | i > 0 && s == "diff" = True
    | otherwise = False
-- isStatDiffOutput = (=~ " +\\| +[0-9]+ [+-]+")
isStatDiffOutput = (=~ " +\\| +[^ ]+ [^ ]+")
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
                                 , promptAction :: X () -> X () -> OnWindowsInserted -> String -> X ()
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

dwgtMode mode prompt = dwgt mode prompt (\c -> completionFunction mode) (\immi final _ s -> (modeAction mode) s "" >> immi >> final)
dwgtDictMode mode prompt = dwgt mode prompt (\c -> completionFunction mode) (\immi final owi s -> do
    applyOnWindowsInserted owi {
              numberOfWindows = 1
            , logFinished = \a b -> do
                (logFinished owi) a b
                final
        }
    (modeAction mode) s "" 
    immi)
dynamicPromptWidgets = [
        -- this follow the (prefix, prompt) format
        dwgt VBPrompt "vb" (\c -> vbComplFunc) (\immi final owi s -> do
            -- wait until the vb is completely spawned
            applyOnWindowsInserted owi {
                      numberOfWindows = 1
                    , logFinished = \a b -> do
                        (logFinished owi) a b
                        final
                }
            vbAction s
            immi
        )
      , dwgtDictMode defaultSDMode "sdcv-Collins"
      , dwgtDictMode mobySDMode "sdcv-Moby"
      , dwgtDictMode modernCHSDMode "sdcv-modernChinese"
      , dwgtDictMode bigCHSDMode "sdcv-bigChinese"
      , dwgt TaskPrompt "tk" (\c -> taskComplFunc) taskAction'
      , dwgt FMCPrompt "fmc" (\c -> fmcComplFunc) fmcAction'
      , dwgtMode CalcMode "calc"
    ]

evalStr s = let evalcomps = splitOn "`" s
                evallen = length evalcomps 
                t = tail evalcomps
            in if odd evallen && evallen >= 3 then (head evalcomps, head t, joinStr "`" $ tail t)
                                              else (s,"","")

dpromptNextCompletion c l = let hl = any (flip dpromptHighlightPredicate c) l in
                case dpromptBreak args of
                   (_, "z":bef) | not hl -> joinStr " " $ reverse bef ++ ["c", escape (head l)]
                   ([], ('\'':_):_) -> exactMatch $ fmap stripShortcutOutput l
                   (_, "which":bef) | all ((`elem` "~/") . head) l && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                                    | otherwise -> exactMatch l
                   (aft, "diff":bef) | not (null fdo) -> joinStr " " $ (case bef of
                                                                            "git":bef' -> reverse bef'
                                                                            _ -> reverse bef) ++ [escape (head fdo)]
                   (_, w:bef) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                           in joinStr " " $ reverse bef ++ [w, promptNextCompletion wp (widgetCmd wp c) l]
                   -- we have to specify manually so the 'm' case above when there is some sort of the string on its right would sift through here
                   (_, f:bef) | f `elem` ["f", "a", "d", "h", "l", "t", "which"] && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                   _ | all isGrepOutput l -> joinStr " " $ takeWhile (\t -> t /="g" && t /= "gp") (init args) ++ case exactNext $ fmap stripGrepOutput l of
                                                                                          [] -> []
                                                                                          s -> [s]
                     | all isShortcutOutput l -> exactMatch $ fmap stripShortcutOutput l
                     | not (null fsdo) && (length fsdo) `elem` [length l, length l - 1] -> exactMatch $ fmap stripStatDiffOutput fsdo
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

dpromptHighlightPredicate cl cmd = case dpromptBreak args of
                (_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
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

-- given an array of arguments break it in such a way that it divides the necessary keyword with what is after 
dpromptBreak args = 
    -- we would first break from the beginning to see if any widget matches up
    let stripIndex (a, b) = (fmap snd a, fmap snd b)
        rev = zip [0..] $ reverse args
    in case break isWidgetFilter rev of
         (a, []) -> stripIndex $ break isSearchFilter rev
         b -> stripIndex b

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
            gitcmdcmp args = return $ filter (sp args) ["add", "am", "archive", "bisect", "branch", "bundle", "checkout", "cherry-pick", "citool", "clean", "clone", "commit", "describe", "diff", "fetch", "format-patch", "gc", "grep", "gui", "init", "log", "merge", "mv", "notes", "pull", "rebase", "reset", "rm", "shortlog", "show", "stash", "status", "submodule", "tag"]
        case dpromptBreak unescapedArgs of
                    (afs, "f":_) -> fasd $ ["-f", "-B", "viminfo"] ++ (reverse afs)
                    (afs, "d":_) -> fasd $ "-d" : (reverse afs)
                    (afs, "a":_) -> fasd $ ["-a", "-B", "viminfo"] ++ (reverse afs)
                    (afs, "z":_) -> fasd $ "-d" : (reverse afs)
                    (afs, "h":_) -> return $ take historyLimit $ filter (sp $ joinStr " " $ reverse afs) $ hist
                    ([], ('\'':pre):_) -> shtout $ sct pre
                    (afs, ('m':pre):_) -> shellcmp' ["directory"] []
                    (afs, "l":_) -> shtout $ fmap lines $ runProcessWithInput (myScriptsDir++"/xfind") (show findLimit:(fmap epd $ reverse afs)) ""
                    -- for g, we demand that at least SOME search term is entered, otherwise we don't generate the necessary output
                    (af:afs, "g":_) -> trycmp $ (if afs == [] then [shellcmp' ["file"] []] else []) ++ [grepcmp (reverse $ af:afs)]
                    (af:afs, "gp":_) | afs == [] -> shellcmp' ["file"] []
                                     | not (null afs) && af == "" -> pgrepcmp (reverse $ af:afs)
                    (afs, "t":_) -> fmap (fmap (tagLibroot++) . lines) $ runProcessWithInput tagBin ([show tagLimit, "false"] ++ tagQuery (reverse afs)) ""
                    (afs, "which":_) -> 
                        let ha = head cas
                            cas = reverse afs
                        -- test if af is one of the commands
                        in trycmp [shtout $ fmap lines $ runProcessWithInput "which" cas "", shtout $ return $ take whichLimit $ filter (sp $ joinStr " " cas) cmds]
                    -- we need to make sure that we are using the current directory as a starting point
                    (afs, "diff":_) | ntailsp <= 1 -> trycmp [output (myScriptsDir++"/xdiff") (show outputWidth:show outputHeight:diffargs), shellcmp' ["file"] []]
                            where diffargs = if afs == [""] then [] else reverse afs
                    (_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                          in (promptComplFunction wp) c (widgetCmd wp s)
                -- grave key evaluation (evaluate the grave enclosed string in shell and show the output as autocompletion)
                    _ | not (null evalstr) -> shtout $ fmap (take evalLimit . lines) $ runProcessWithInput "/bin/sh" ["-c", "loader " ++ escapeQuery evalstr] ""
                      | otherwise -> case unescapedArgs of
                            "man":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xman") $ show outputWidth:show outputHeight:pa:pai:pas
                            "du":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xdu") $ show outputHeight:pa:pai:pas
                            ["ip", pa] -> return $ filter (sp pa) ["addr", "addrlabel", "link", "maddr", "mroute", "neigh", "route", "rule", "tunnel"]
                            "ip":pas | lastArg == "" -> output "ip" $ init pas
                            ["top", ""] | ntailsp == 1-> output (myScriptsDir++"/xtop") [show topLimit]
                            ["free", ""] | ntailsp == 1 -> output "free" []
                            ["ifconfig", ""] | ntailsp == 1 -> output "ifconfig" []
                            ("git":gitcmds) | gitcmds `elem` [[""], ["status", ""]] && ntailsp == 1 -> trycmp [output "git" ["status"], gitcmdcmp ""]
                            -- only gives the prime command completion on three spaces
                            ["git", pa] -> gitcmdcmp pa
                            -- in all other instances we should give the log information
                            "git":pa:_ -> trycmp [output "git" $ ["log", "--grep", last unescapedArgs], scopecmp, shellcmp]
                            _ -> trycmp [scopecmp, shellcmp]


cmdsWithGUI = ["xterm", "retroarch", "gimp", "inkscape", "libreoffice", "xvim", "xmutt", "zathura", "vimb", "vb", "intellij-idea-ultimate-edition"]
dpromptAction c cmds home history hist immi final owi s = 
        -- perform some special actions on some commands
        let args = parseShellArgs s
            follow = immi >> final
        in case (findWidgetForAction s, args) of
                (Just (rest, w), _) -> (promptAction w) immi final owi rest
                (_, [('m':pre:[]), pa]) -> spawn (myScriptsDir++"/xshortcut mark "++[pre]++" "++pa) >> follow
                (_, "reboot":_) -> removeAllWorkspaces >> spawn "reboot"
                (_, "systemctl":"poweroff":_) -> removeAllWorkspaces >> spawn "systemctl poweroff"
                (_, ha:pas) | ha `elem` ["cd", "c", "z"] -> do
                                 d <- if null pas 
                                         then return home 
                                         else case head pas of
                                                 "-" -> getCurrentWorkspaceOldDirectory
                                                 p -> return $ unescape p
                                 saveCurrentWorkspaceDirectory d
                                 dynamicPrompt' c cmds home history hist immi final owi
                            | otherwise -> do
                                -- we need to determine the sort of thing to do with the dphandler
                                fe <- io (doesFileExist ha)
                                de <- io (doesDirectoryExist ha)
                                let ha' = if null s then "." else s
                                    run = spawn $ "loader " ++ dphandler++" "++ha'
                                if fe || de || '/' `elem` ha' || ('.' `elem` ha' && pas == []) || ha `elem` cmdsWithGUI
                                   then do
                                        applyOnWindowsInserted owi {
                                                  numberOfWindows = 1
                                                , logFinished = \a b -> do
                                                    (logFinished owi) a b
                                                    final
                                            }
                                        run 
                                        immi
                                   else run >> follow
                                

divide' _ [] (r, w) = (reverse r, reverse w)
divide' p (x:xs) (r, w) = divide' p xs $ if p x then (x:r, w) else (r, x:w)
divide p l = divide' p l ([],[])

dynamicPrompt c immi final owi = do
    cmds <- io getCommands
    home <- io $ env "HOME" "/home/lingnan"
    history <- io readHistory
    let histp = (=~ "^~?/")
        histf a z = (filter histp a) ++ z
        hist = fmap unescape $ nub $ sort $ M.foldr histf [] history
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
    dynamicPrompt' c cmds home history hist immi final owi

dynamicPrompt' c cmds home history hist immi final owi = do
    d <- getCurrentWorkspaceDirectory
    io $ setCurrentDirectory d
    -- only deal with directories or files at the moment; not doing check on the file / directories to save performance
    mkXPromptWithHistoryAndReturn (DPrompt $ shortend home d) c (dpromptComplFunc c cmds home hist) (dpromptAction c cmds home history hist immi final owi) history
    return ()
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
    modeAction _ query _ = vbAction $ (if length (words query) <= 1 then "wt " else "d !tr ") ++ query
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

-- cycling between different dictionary
defaultDictionaries = ["sdcv-Collins", "sdcv-Moby", "sdcv-modernChinese", "sdcv-bigChinese"]
dpromptPrefices = defaultDictionaries ++ ["vb", "calc", "tk", "fmc"]
cycleDictionaryForDPrompt dir = do
    str <- getInput
    let ls = (if dir == Next then id else reverse) defaultDictionaries
        nstr = case findIndex (`isPrefixOf` str) ls of 
                    Just i -> (ls !! ((i+1) `mod` (length ls))) ++ (maybe "" ((" "++) . dropWhile isSpace) $ stripPrefix (ls !! i) str)
                    _ -> head ls ++ " " ++ removePrefix str dpromptPrefices
    setInput nstr
    endOfLine

removePrefix str pres = if null args then str
                                     else case break (`elem` pres) args of
                                               (_, _:afs) -> joinStr " " afs
                                               _ -> str
            where args = words str

addOrTruncateTillPrefix prefix = do
    str <- getInput
    if prefix `isPrefixOf` str
       then setInput prefix
       else setInput $ prefix ++ removePrefix str dpromptPrefices
    endOfLine

setInputAndDone str = setInput str >> setSuccess True >> setDone True
changeInputAndDone fun = getInput >>= setInput . fun >> setSuccess True >> setDone True

-- }}}

----- Vimb prompt -- {{{

data VBPrompt = VBPrompt

vbNextCompletion cmd ls = last $ words $ ls !! ni
        where ni = case findIndex (vbIsEqualToCompletion cmd) ls of
                      Just i -> if i >= length ls - 1 then 0 else i+1
                      Nothing -> 0 
vbHighlightPredicate = flip vbIsEqualToCompletion
vbAction s = runShell $ "vb " ++ escapeQuery s
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
    -- we'd probably just get all the workspaces and then the windows inside
    wss <- allWorkspaces
    curr <- gets (W.workspace . W.current . windowset)
    (l, r, gf, ml, float, _, _) <- getSides
    names <- getWorkspaceNames
    -- get the last window
    lm <- nextMatched History (return True)
    let wswins = W.integrate' . W.stack
        -- remove duplicate windows
        cwins = nub $ l++gf++r++float++ml
        winfo :: Window -> X (([String], String), Window)
        winfo w = do
            t <- runQuery title w
            tags'' <- getTags w
            let tags = sortRegs $ if Just w == lm then tags'++["'"] else tags'
                tags' = case tags'' of
                         [] | w `elem` ml -> ["*"]
                            | otherwise -> []
                         ts -> ts
            return ((tags, t), w)
        -- we must make sure there aren't duplicated entries
        wp ws = fmap (concatMap (fmap (\(s, ((a,t,b), w)) -> ((a,t,if s == "" then b else b++" ["++s++"]"), w)) . zip ([""] ++ fmap show [1..])) . groupBy (\(a,_) (b,_) -> a == b) . fmap (attag (W.tag ws))) $ mapM winfo $ wswins ws
        wr = fmap concat . mapM wp
        attag t ((a,b),w) = ((a,names t,b),w)
    (winfos, winls) <- case break ((==(W.tag curr)) . W.tag) wss of
         (bf, c:af) -> do 
                bis <- wr bf
                ais <- wr af
                cis <- wp curr
                return $ unzip $ bis ++ cis ++ ais
         (bf, _) -> fmap unzip $ wr bf
    let fillsp ls = let l = maximum $ fmap length ls
                        fsp s = (take (l - length s) $ repeat ' ')++s
                    in fmap fsp ls
        (is, wins) = unzip $ flip sortBy (zip winfos winls) $ \((a,b,c),_) ((d,e,f),_) -> 
            case (compareLists' compareRegs a d, compareSymbolString b e, compare c f) of
                 (EQ, EQ, z) -> z
                 (EQ, y, _) -> y
                 (x, _, _) -> x
        (regs', tags', tits) = unzip3 is
        regsToStr regs = (if null regs then id else wrap "[" "]") $ concatMap ("'"++) regs
        finfos = fmap (\(a,b,c)-> a++" ["++b++"]: "++c) $ zip3 (fillsp (fmap regsToStr regs')) (fillsp tags') tits
    -- filter the windows according to p
    mkXPrompt (WindowSearchPrompt prompt) config (\s -> return $ filter (searchPredicate config s) finfos) $ \r ->
        whenJust (fmap (wins !!) $ elemIndex r finfos) $ a

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
    ml <- getCurrentMinimizedWindows
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
                           , construct :: Int -> Maybe Window -> X ()
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

compFilterKeyList = compareLists compCh
    where compCh c1 c2 = let lc1 = toLower c1
                             lc2 = toLower c2
                         in if lc1 == lc2 then compare c2 c1
                                          else compare lc1 lc2

filterKeyList = fmap (\s-> (if "S-" `isPrefixOf` s then toUpper else toLower) $ last s) . filter (not . null) . splitOn " "

instance Ord TaskGroup where
    compare t t' = let r = compFilterKeyList (filterKeyList $ filterKey t) (filterKeyList $ filterKey t') in if r == EQ then compare (taskGroupName t) (taskGroupName t') else r

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

seqn n = sequence_ . take n . repeat 
instance Default TaskGroup where 
    def = TaskGroup { taskGroupName = "Unknown"
                      , filterKey = ""
                      , filterPredicate = alwaysTrue
                      , localFirst = True
                      , construct = \_ _ -> return ()
                      , launchHook = idHook
                      -- the default window styles involving
                      , windowStyle = windowStyleFromList [doSink, lowerHalfRectHook]
                      , colorScheme = mySubTheme
                      }

intellijTaskGroup = def { taskGroupName = "idea"
                        , filterKey = "j"
                        , filterPredicate = className =? "jetbrains-idea"
                        , localFirst = False
                        , construct = \n _ -> runShell "intellij-idea-ultimate-edition"
                        }
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
taskGroups = [ 
      -- vimb instances
      def { taskGroupName = "vimb"
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
    , def { taskGroupName = "vim"
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
    , def { taskGroupName = "pidgin-buddy"
          , filterPredicate = isPidginBuddyList
          , launchHook = rightPanelHook
          , construct = \n _ -> mkNamedScratchpad scratchpads "pidgin"
          , windowStyle = windowStyleFromList [rightPanelHook, leftPanelHook, doSink]
          }
      -- pidgin conversation windows
      -- we'd like to put the conversations (self poped one) into another workspace if they are not launched by the user
    , def { taskGroupName = "pidgin"
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
    , def { taskGroupName = "ranger"
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
    , def { taskGroupName = "zathura"
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
      -- recyclable term instances (m-s-<Return> triggers the same event)
    , def { taskGroupName = "term"
          , filterKey = "t"
          , filterPredicate = isRecyclableTerm <||> appName =? "xterm"
          , construct = \n _ -> seqn n $ runTerm "" "xterm" "zsh -i"
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
          , construct = \n _ -> seqn n $ runTerm "mutt" "mutt" "loader mutt"
          }
    -- canto general instance
    , def { taskGroupName = "canto"
          , filterKey = "o"
          , filterPredicate = isTerm <&&> (title =? "canto" <||> appName =? "canto")
          , construct = \n _ -> seqn n $ runTerm "canto" "canto" "loader canto"
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
    , intellijTaskGroup
      -- gimp singleton
    , def { taskGroupName = "gimp"
          , filterKey = "S-m"
          , filterPredicate = className =? "Gimp"
          , localFirst = False
          , construct = \_ _ -> runShell "gimp"
          }
      -- inkscape (can have multiple documents)
    , def { taskGroupName = "inkscape"
          , filterKey = "k"
          , filterPredicate = className =? "Inkscape"
          , construct = \n _ -> seqn n $ runShell "inkscape"
          }
      -- libreoffice (can have multiple documents)
    , def { taskGroupName = "libre"
          , filterKey = "l"
          , filterPredicate = fmap (isInfixOf "libreoffice") className
          , construct = \n _ -> seqn n $ runShell "libreoffice"
          }
      -- all remaining xterms can be matched in this group
    , def { taskGroupName = "term(...)"
          , filterKey = "S-t"
          , filterPredicate = isTerm
          , construct = \n _ -> seqn n $ runTerm "" "" "zsh -i"
          }
      -- all remaining windows that share the same class attributes
    , def { filterPredicate = hasSamePropertyAsFocused className }
      -- all other remaining windows
    , def
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
        , construct = \n _ -> withFocused $ \w -> taskGroupOfWindow gs w >>= maybe (return ()) (\g -> (construct g) n (Just w))
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

-- the normal typableChars
typeables = nums ++ easierSyms ++ lowerAlphas ++ mediumSyms ++ upperAlphas ++ restSyms
alphas = lowerAlphas ++ upperAlphas
lowerAlphas = ['a'..'z']
upperAlphas = fmap toUpper lowerAlphas
nums = ['1'..'9'] ++ "0"
easierSyms = "-="
mediumSyms = "[];',./"
restSyms = "\\!@#$%^&*()_+{}:\"<>?|" --`~"
syms = easierSyms ++ restSyms

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
                                           ':' -> "S-;"
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
numberKeys = flip zip [1..] $ [""] ++ fmap ((++" ") . joinStr " " . fmap charToKeyStroke . show) [2..9]
numberKeyToMotion a = if null a then a else "g "++a
numberMotionKeys = fmap (\(a,b) -> (numberKeyToMotion a, b)) numberKeys
tabKeys = zip (fmap wrapList subgroupSymbolSequence) [0..]
extendedTabKeys = zip (fmap charToKeyStroke $ extendedSequence subgroupSymbolSequence) [length tabKeys..]
columnKeys = zip (fmap wrapList columngroupSymbolSequence) [0..]
extendedColumnKeys = zip (fmap charToKeyStroke $ extendedSequence columngroupSymbolSequence) [length columnKeys..]
feedReg k fun = fmap Just $ fun k
wrapInclude = fmap (\a -> (a, True))
getCurrentMinimizedWindowsWithoutRegs = getCurrentMinimizedWindows >>= filterM (fmap null . getTags)

targetForReg r d = wrapAroundWindowsMatchingPredicate d (fmap not isFocused <&&> hasTagQuery r) >>= return . wrapInclude . listToMaybe
regKeys :: [(String, (String -> X a) -> X (Maybe a), Bool, String -> Direction1D -> X (Maybe (Window, Bool)), String -> X [Window])]
regKeys = [( charToKeyStroke k
           , feedReg [toLower k]
           -- this determines if the windows are added into the register
           , isUpper k
           , targetForReg
           , \t -> orderedWindowsMatchingPredicate (hasTagQuery t)) | k <- alphas ++ nums ]
quoteRefed = fmap (\(a,b,c,d,e)->("' "++a++" ",b,c,d,e))
regReadonlyKeys = [ (charToKeyStroke '\''
                    , feedReg "'"
                    , False
                    , \_ _ -> nextMatched History (return True) >>= return . wrapInclude
                    , \_ -> fmap maybeToList $ nextMatched History (return True))
                  , (charToKeyStroke '*'
                    , feedReg "*"
                    , False
                    , \_ d -> fmap (wrapInclude . listToMaybe . if d == Prev then reverse else id) getCurrentMinimizedWindowsWithoutRegs
                    , \_ -> getCurrentMinimizedWindowsWithoutRegs)]
regUnnamedKey = (""
                , feedReg ""
                , False
                , \_ _ -> return Nothing
                , \_ -> return [])
regTagPrompt prompt a = initMatches >>= \r -> tagPrompt (myXPConfig r) {
    searchPredicate = prefixSearchPredicate
} prompt a
regPromptKeys p = regPromptKeys' p p
regPromptKeys' promptForCut promptForAdd = 
    [ ( charToKeyStroke c
      , regTagPrompt pr
      , add
      , targetForReg
      , \t -> orderedWindowsMatchingPredicate (hasTagQuery t))
    | (c, add, pr) <- [('/', False, promptForCut), ('?', True, promptForAdd)] ]

-- make sure that the star register is sorted last
compareRegs a b =
    case compare (elemIndex a ["*", ""]) (elemIndex b ["*", ""]) of
        EQ -> compareWithPriorList elemIndex (fmap wrapList "'\"123456789") a b
        r -> r
sortRegs = sortBy compareRegs

data Motion = Motion {
        -- the window that should be focused by navigation
          target :: Maybe (Window, Bool)
        , simpleX :: Maybe (X ())
        , toggleList :: Maybe [Window]
        , implicitToggleList :: Maybe [Window]
        , historyToggle :: Maybe (X ())
        , triggerOnNothing :: Maybe (X ())
        , searchFunction :: Maybe (Direction1D -> X (Maybe (Window, Bool)))
    }
instance Default Motion where
    def = Motion {
          target = Nothing
        , simpleX = Nothing
        , toggleList = Nothing
        -- an implicitToggleList is different from toggleList in that it captures the range of windows of the 
        , implicitToggleList = Nothing
        , historyToggle = Nothing
        , triggerOnNothing = Nothing
        , searchFunction = Nothing
        }
-- motion keys are usually only obtained through X (). This is because there are a bunch of things to calculate
clamp i a b = if a <= b then Just $ max (min i b) a else Nothing
motionKeys :: [(String, String, X Motion)]
motionKeys = 
    -- M1 toggles
    [ (sfk++t, fk++t, do
             st <- getBaseCurrentStack
             case st of
                  Just (W.Stack _ u _) | n < len && (leap || ind /= length u || useSingleCurrent) -> 
                        return def { target = t
                                   , simpleX = Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusAt ind
                                   , toggleList = if leap then Just [w] else Nothing
                                   , historyToggle = if leap 
                                                        then Just $ nextMatch History $ isOneOfWindows $ ls
                                                        else Nothing
                                   }
                             where ls = W.integrate' st
                                   len = length ls
                                   w = ls !! ind
                                   t = Just (ls !! ind, True)
                                   ind = n `mod` len
                  _ -> return def
      )
    | (sfk, fk, leap, quick) <- [("M1-", "M1-", True, False), ("f ", "M-f ", False, False), ("", "M-g ", False, True)]
    , (t, n, useSingleCurrent) <- if quick then [("0", 0, False), ("S-4", -1, True)] 
                                           else fmap (\(a,b)->(a,b,False)) $ tabKeys ++ if leap then [] else extendedTabKeys
    ]
    -- C- toggles
    ++
    [ (sfk++c, fk++c, do
             gs <- G.getCurrentGStack
             case gs of
                  Just (G.Node s@(W.Stack _ u _)) | n < len && (leap || ind /= length u || useSingleCurrent) -> 
                        return def { target = t
                                   , simpleX = Just $ sendMessage $ G.Modify $ G.focusGroupAt ind
                                   , toggleList = if leap then Just $ G.flattened g else Nothing
                                   , implicitToggleList = if leap then Nothing else Just $ concatMap G.flattened $ 
                                                                    case () of
                                                                        _ | ci < ind -> drop ci $ take (ind+1) ls
                                                                          | ci == ind -> [g]
                                                                          | otherwise -> drop ind $ take ci ls
                                   , historyToggle = if leap 
                                                    then Just $ toggleGroup
                                                    else Nothing
                               }
                            where ls = W.integrate s
                                  len = length ls
                                  ind = n `mod` len
                                  ci = length u
                                  (si, ei) = if ci < ind then (ci, ind) else (ind, ci)
                                  g = ls !! ind
                                  w = G.focal g 
                                  t = fmap (\a -> (a, True)) w
                  _ -> return def
      )
    | (sfk, fk, leap, quick) <- [("C-", "C-", True, False), ("f C-", "M-f C-", False, False), ("", "M-g ", False, True)]
    , (c, n, useSingleCurrent) <- if quick then [("C-0", 0, False), ("C-S-4", -1, True)] 
                                           else fmap (\(a,b)->(a,b,False)) $ columnKeys ++ if leap then [] else extendedColumnKeys
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
    [ (nk++wk, "M-"++numberKeyToMotion nk++wk,
          let move' dir = do
               mfs <- getFocusStack
               let sign' = if dir == Next then sign else sign * (-1)
               return $ case mfs of
                       Just (W.Stack f u d) | isJust mind && (ind /= length u || not include) -> Just (Just (ls !! ind, oind < 0 || oind >= length ls || include), ind)
                            where ci = length u
                                  ls = W.integrate' mfs 
                                  -- turning off the wrapping function
                                  oind = ci + sign' * n
                                  mind = clamp oind 0 (length ls - 1)
                                  ind = fromJust mind
                       _ -> Nothing
              move = fmap (maybeToMaybe fst) . move'
          in move' Next >>= \mt -> return $ case mt of
                    Just (t, ind) -> def { target = t
                                         -- , searchFunction = Just move
                                         , simpleX = Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusAt ind 
                                         }
                    _ -> def
      )
    | (nk, n) <- numberKeys
    , (wk, sign, include) <- [("b", -1, True), ("w", 1, False)]
    ]
    -- gg and G
    ++
    [ (sfk, fk, do
        -- get the second level nesting counting from the top
        ins <- focusIsInGStack
        bs <- fmap (maybeToMaybe G.bases . maybeToMaybe G.current) G.getCurrentGStack
        let (t, ind, tls) = case (ins, bs) of
                             (True, Just (G.Node s@(W.Stack _ u _))) -> (fmap (\a -> (a, True)) $ G.focal $ ls !! ind', ind', concatMap G.flattened $ drop si $ take (ei+1) ls)
                                where ls = W.integrate s
                                      len = length ls
                                      ind' = (min n (len-1)) `mod` len
                                      ci = length u
                                      (si, ei) = if ci < ind' then (ci, ind') else (ind', ci)
                             _ -> (Nothing, -1, [])
        return def {
                  target = t
                , simpleX = if isJust t then Just $ sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusGroupAt ind else Nothing
                -- select everything in between
                , implicitToggleList = Just tls
            }
      )
    | (sfk, fk, n) <- [("g g", "M-g g", 0)] 
                      ++ 
                      [ (nk++"S-g", "M-"++numberKeyToMotion nk++"S-g", n'-1)
                      | (nk, n') <- tail numberKeys] 
                      ++
                      [ ("S-g", "M-g S-g", -1)]
    ]
    -- the h j k l's
    ++
    [ (nk++wk, "M-"++numberKeyToMotion nk++wk,
          let move' dir' = do
                  let sign = if dir == dir' then 1 else -1
                  ins <- focusIsInGStack
                  bs <- fmap gfun G.getCurrentGStack
                  return $ case (ins, bs) of
                      (True, Just (G.Node s@(W.Stack _ up _))) | isJust mind && ind /= ci -> 
                                (Just ((fmap (\a -> (a, True)) $ G.focal $ ls !! ind), ind), concatMap G.flattened $ drop si $ take (ei+1) ls)
                         where ls = W.integrate s 
                               len = length ls
                               ci = length up
                               -- turning off the wrapping
                               mind = clamp (ci + sign * n) 0 (length ls - 1)
                               ind = fromJust mind
                               (si, ei) = if ci < ind then (ci, ind) else (ind, ci)
                      _ -> (Nothing, [])
              move = fmap (maybeToMaybe fst . fst) . move'
          in move' Next >>= \mt -> do
              return $ case mt of
                    (Just (t, ind), ls) -> 
                                def { target = t
                                    -- , searchFunction = Just move
                                    , simpleX = Just $ sx ind
                                    -- select everything in the consecutive row/col
                                    , implicitToggleList = Just ls
                                    }
                    _ -> def
      )
    | (nk, n) <- numberKeys
    , (wk, dir, gfun, sx) <- [(wk', dir', maybeToMaybe G.bases . maybeToMaybe G.current, \n -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusGroupAt n)
                             | (wk', dir') <- [("k", Prev), ("j", Next)]]
                             ++
                             [(wk', dir', id, \n -> sendMessage $ G.Modify $ G.focusGroupAt n)
                             | (wk', dir') <- [("h", Prev), ("l", Next)]]
    ]
    ++
    -- group toggling
    [ ("g "++filterKey g, "M-"++px++"g "++filterKey g, do
            -- the cycling and toggling would require different techniques
            let move dir = wrapAroundWindowsMatchingPredicate (if dir == d then Next else Prev) (fmap not isFocused <&&> localFirstFilterPredicate g) 
                            >>= return . fmap (\a->(a,True)) . listToMaybe
            t <- move Next
            wins <- orderedWindowsMatchingPredicate ((filterPredicate g) <&&> isInCurrentWorkspace)
            return def {
                      target = t
                    , toggleList = Just wins
                    , searchFunction = Just move
                    , triggerOnNothing = Just $ (construct g) 1 Nothing
                }
      )
    | g <- allTaskGroupsWithFilterKey ["c"]
    , (px, d) <- [("", Next), ("S-", Prev)]
    ]
    ++
    -- tag handling
    [ ("' "++tk, "M-g ' "++tk, do
            res <- ta $ \t -> do
                ls <- xls t
                return (xt t, ls)
            let find = fmap fst res
            t <- maybe (return Nothing) (\f -> f Next) find
            return def {
                      target = t
                    , toggleList = Just (maybe [] snd res)
                    , searchFunction = find
                }
      )
    | (tk, ta, _, xt, xls) <- regKeys ++ regPromptKeys "Select windows from register: " ++ regReadonlyKeys
    ]

motionKeyCommands' = flip fmap (nubBy (\(_,a,_) (_,b,_) -> a == b) motionKeys) $ \(_, k, x) -> (k, do
    mf <- gets (W.peek . windowset)
    Motion mt simpx mtogls _ mtoggle mconstruct mfind <- x
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
windowSelectionForMotionKey key = maybe (return []) snd $ find ((==key) . fst) motionKeyWindowSelection
windowSelectionFromMotion mt = do
    mf <- gets (W.peek . windowset)
    case (toggleList mt, implicitToggleList mt) of
         (_, Just ls) -> return ls
         (Just ls, _) -> return ls
         _ -> do
           wins <- allOrderedWindows
           return $ case (mf, target mt) of
                        (Just f, Just (t, include)) -> case (elemIndex f wins, elemIndex t wins) of
                                                 (Just fi, Just ti) | fi < ti -> drop fi $ take (ti + if include then 1 else 0) wins
                                                                    | fi == ti -> [f]
                                                                    | otherwise -> drop ti $ take fi wins
                        _ -> []                                                                      
motionKeyWindowSelection = flip fmap (nubBy (\(a,_,_) (b,_,_) -> a == b) motionKeys) $ \(k, _, x) -> (k, x >>= windowSelectionFromMotion)

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
    [ (nk++fk, xls)
    | (nk, n) <- numberKeys
    , (fk, xls) <- [ (gk, groupList n)
                   , ("r", columnList n)]
    ]
wssKeys =
    [ (nk++"s", do
            wss <- allWorkspaces
            curr <- gets (W.currentTag . windowset)
            let ls = concatMap (W.integrate' . W.stack) $ take n $ snd $ break ((==curr).W.tag) wss
            return ls)
    | (nk, n) <- numberKeys
    ]

cutCommands = concatMap (processKey . addPrefix) $
    [ (regk++dc, da)
    | (regk, ta, add, _, _) <- [regUnnamedKey]++ quoteRefed (regKeys ++ regPromptKeys' "Cut windows to register: " "Add windows to register: ")
    , (dc, da) <- -- deletion
                  [ (fk, do
                        ls <- xls
                        ta $ \t -> cutcmd add t ls
                        refresh)
                  | (fk, xls, cutcmd) <- [ (pk++" "++mk, xls', cutcmd')
                                         | (pk, cutcmd') <- [("d", delete), ("m", cut)]
                                         , (mk, xls') <- motionKeyWindowSelection
                                                        ++
                                                        structKeys pk]
                                         ++
                                         [ (pk, windowSelectionForMotionKey "S-4", cutcmd')
                                         | (pk, cutcmd') <- [("S-d", delete), ("S-m", cut)] ]
                                 -- ++
                                 -- [ ("S-q", groupList 1) ]
                                 -- ++
                                 -- [ ("C-q", columnList 1) ]
                  ]
                  ++
                  [ (pk++"x", onSelectedWindows $ \wins -> ta (\t -> cutcmd add t wins) >> refresh)
                  | (pk, cutcmd) <- [("", delete), ("S-", cut) ]
                  ]
                  ++
                  [ (fks, do
                      -- ta $ \t -> sequence_ $ take n $ repeat $ removeCurrentWorkspace' t
                      wst <- workspaceStack
                      let ls = case wst of
                                  Just (W.Stack f _ d) -> [f]++d
                                  _ -> []
                      removeWorkspaces $ fmap W.tag $ take n ls)
                  | (fks, n) <- [ (pk++" "++nk++"s", n')
                                | (nk, n') <- numberKeys
                                , pk <- ["d"{- , "m" -}] ]
                                -- ++
                                -- [ ("C-S-q", 1) ]
                  ]
    ]
    ++
    [ ("d M-"++t, removeWorkspace t)
    | t <- quickWorkspaceTags]
    -- the find motion for workspaces
    ++
    [ ("d "++mk++"M-"++ch, do
        ts <- allWorkspaceTags
        curr <- gets (W.currentTag . windowset)
        -- first we should try to locate the indices
        case (elemIndex curr ts, ati ts) of
             (Just ci, Just ti) | ci < ti -> removeWorkspaces $ drop ci $ take (ti+1) ts
                                | ci == ti -> if useSingleCurrent then removeWorkspaces $ [ts !! ci] else return ()
                                | otherwise -> removeWorkspaces $ drop ti $ take ci ts
             _ -> return ())
    | (mk, ch, ati, useSingleCurrent) <- 
                       [ ("f ", charToKeyStroke t, elemIndex [t], False) | t <- symbolSequence]
                       ++
                       [ ("", "0", \_ -> Just 0, False), ("", "S-4", \ts -> Just $ length ts - 1, True) ]
    ]
    -- the [ and ] motion
    ++
    [ ("d "++nk++dk, do
            wst <- workspaceStack
            let ls = case wst of
                         Just (W.Stack f u d) | dir == Prev -> if null u then [] else [f]++u
                                              | otherwise -> if null d then [] else [f]++d
                         _ -> []
            removeWorkspaces $ fmap W.tag $ take (n+1) ls)
    | (nk, n) <- numberKeys
    , (dk, dir) <- [("[", Prev), ("]", Next)]
    ]

yankCommands = concatMap (processKey . addPrefix) $
    [ (kstr, da)
    | (regk, ta, add, _, _) <- regKeys ++ regPromptKeys' "Yank windows to register:" "Add windows to register: "
    , (kstr, da) <- [ ("' "++regk++" y "++mk, do
                            ls <- xls
                            ta $ \t -> yank add t ls
                            return ())
                    | (mk, xls) <- motionKeyWindowSelection
                                   ++
                                   structKeys "y"
                                   ++
                                   wssKeys]
                    ++
                    [ ("y "++regk, do
                         gs <- getSelectedWindowStack
                         ta $ \t -> yank add t (W.integrate' gs)
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

constructionKeys = [ (filterKey g, \n ls lf -> do
                            onWindowsInserted n (\_ _ -> return ()) ls (\_ _ -> lf)
                            (construct g) n Nothing)
                   | g <- allTaskGroupsWithFilterKey ["c"]]
                   ++
                   [ ("/", \n l lf -> do
                           let base x = mkDynamicPrompt' (return ()) (return ()) $ OnWindowsInserted 1 (\_ _ -> return ()) (\_ _ -> return ()) (\mf ls -> do
                               if not (null ls) then l mf $ head ls
                                                else return ()
                               x)
                           (!! n ) $ (iterate base lf)
                     ) ]

-- the change commands allow an extra X () that will be run AFTER the windows has been created
changeCommands = concatMap (processKey . addPrefix) $
    [ (regk++mk++"c "++mt++ck, \immi final -> do
        -- first get the selection to be replaced
        (tgx, ls) <- xlst
        -- then travel to the place
        case tgx of
             Just tx -> tx
             _ -> return ()
        -- get the register
        mt <- ta $ return . id
        construct 1 (\_ _ -> return ()) $ do
            whenJust mt (\t -> delete add t ls)
            final
        immi
      )
    | (ck, construct) <- constructionKeys
    -- removed the numbers because that just explodes the number of keys enough to crash the system
    -- , (nk, n) <- numberKeys
    , (mk, mt, xlst) <- [ (mk', k', do
                             x <- xmt
                             let simpx = simpleX x
                                 tg = fmap fst $ target x
                             ls <- windowSelectionFromMotion x
                             return (Just $ if isJust simpx then fromJust simpx
                                                            else if isJust tg then deminimizeFocus $ fromJust tg
                                                            else return ()
                                    , ls)
                          )
                        | (k, _, xmt) <- motionKeys
                        , (mk', k') <- [ ("", k++" ") ] ++ if k == "S-4" then [ ("S-", "") ] else []
                        ]
                        ++
                        [ ("", mt'++" ", fmap ((,) Nothing) xls') 
                        | (mt', xls') <- structKeys "c"]
    , (regk, ta, add, _, _) <- [regUnnamedKey] ++ quoteRefed (regKeys ++ regPromptKeys' "Cut windows to register: " "Add windows to register: ")
    ]
    -- dedicated construct method that allows insertion in different locations
    ++
    [ (regk++"c "++nk++gk++ipk++" "++ck, \immi final -> do
            t <- btio
            (repls, mt) <- flip ifInVisualMode (return ([], Nothing)) $ do
                        -- we should get the visually selected windows and remove them if necessary
                        wins <- onSelectedWindows return
                        -- go to the top of the selected windows (if any)
                        case wins of
                             h:_ -> deminimizeFocus h
                             _ -> return ()
                        mt <- ta return
                        return (wins, mt)
            construct n (\mpf w -> do
                let trans _ (Just s@(W.Stack (G.G l (Just st@(W.Stack f u d))) _ _)) = 
                        let st' = W.filter (/= w) st
                            ls = case (break ((== mpf) . Just) (W.integrate' st'), inp) of
                                      ((bf, pf:aft), After) -> bf++(pf:w:aft)
                                      ((bf, aft), Head) -> w:(bf++aft)
                                      ((bf, aft), _) -> bf++aft++[w]
                        in case (break (==f) ls, inp) of
                            (_, Before) -> Just s
                            ((bf, af:aft), _) -> Just s { W.focus = G.G l (Just $ W.Stack af (reverse bf) aft)}
                            _ -> Just s { W.focus = G.G l (W.differentiate ls)}
                    trans _ s = s
                if inp == Before 
                   then return () 
                   else do
                       sendMessage $ G.ToFocused $ SomeMessage $ G.Modify trans 
                ) $ do
                    atio t
                    -- check if we need to remove visually selected windows
                    case (repls, mt) of
                         (a:as, Just t) -> delete add t (a:as) >> refresh
                         _ -> return ()
                    final
            immi
      )
    | (nk, n) <- numberKeys
    , (gk, btio, atio) <- [ ("", return False, \_ -> return ())
                          , ("g ", do
                                XS.put $ InsertOlderToggleBackUp (False, False)
                                InsertOlderToggle t <- XS.get
                                XS.put $ InsertOlderToggle $ not t 
                                return t
                             , \t -> do
                                 XS.put $ InsertOlderToggle t
                                 runLogHook)]
    , (ipk, inp) <- insertPositionKeys
    , (ck, construct) <- constructionKeys
    , (regk, ta, add, _, _) <- [regUnnamedKey] ++ quoteRefed (regKeys ++ regPromptKeys' "Cut windows to register: " "Add windows to register: ")
    ]

pasteCommands = concatMap (processKey . addPrefix)
    [ (regk++cmd, ta (paste i np xls) >> return ())
    | (cmd, i, np) <- [("p", False, True)
                      -- , ("S-p", False, False)
                      , ("g p", True, True)
                      -- , ("g S-p", True, False)
                      ]
    , (regk, ta, _, _, xls) <- [regUnnamedKey]++ quoteRefed (regKeys ++ regPromptKeys "Paste windows from register: " ++ regReadonlyKeys)
    ]
visualCommands = 
    [ ("M-v", enterVisualMode Win)
    , ("M-S-v", enterVisualMode Row)
    , ("M-C-v", enterVisualMode Col)
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
    [ ("C-S-"++k, applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToGroupAt n s)
    | (k, n) <- columnKeys]
    ++
    [ ("M1-S-"++k, sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.insertAt n)
    | (k, n) <- tabKeys]
    ++
    [ ("M1-C-"++k, sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapWith n)
    | (k, n) <- tabKeys]
    ++
    [ ("M-<Space>", sendMessage (G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage NextLayout) >> refresh)
    , ("M-S-<Space>", nextOuterLayout >> refresh)]
    -- swap interface (using the new countable interface)
    ++
    concatMap (processKey . addPrefix)
    [ (nk++ck++"S-"++fk, sendMessage msg)
    | (nk, n) <- numberMotionKeys
    , (fk, ck, msg) <- [ ("b", "", G.ToFocused $ SomeMessage $ G.Modify $ G.swapUpN n)
                       , ("w", "", G.ToFocused $ SomeMessage $ G.Modify $ G.swapDownN n)
                       , ("k", "C-", G.ToFocused $ SomeMessage $ G.Modify $ G.swapGroupUpN n)
                       , ("j", "C-", G.ToFocused $ SomeMessage $ G.Modify $ G.swapGroupDownN n)
                       , ("h", "C-", G.Modify $ G.swapGroupUpN n)
                       , ("l", "C-", G.Modify $ G.swapGroupDownN n)
                       ]
    ]
    ++
    [ ("M-S-k", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsUp False s)
    , ("M-S-j", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsDown False s)
    , ("M-S-h", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsUp False s)
    , ("M-S-l", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsDown False s)

    , ("M-C-k", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsToNewGroupUp s)
    , ("M-C-j", applySelectedWindowStack False $ \s -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveWindowsToNewGroupDown s)
    , ("M-C-h", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToNewGroupUp s)
    , ("M-C-l", applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToNewGroupDown s)
    , ("M-C-s", sortBaseCurrentWindows)
    -- a permanent sticky layout that will automatically move any new window into the corresponding task group
    -- , ("M-S-s", do
    --     h <- getCurrentWorkspaceHandle 
    --     toggleAutoArrangeWorkspace h)
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
    -- , ("M-S-<Tab>", switchFocusFloat)
    -- cycling of the window styles
    , ("M-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Next)
    , ("M-S-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Prev)
    ]

mkDynamicPrompt' immediate final owi = initMatches >>= \r -> dynamicPrompt (myXPConfig r) { 
              changeModeKey = xK_VoidSymbol
            , searchPredicate = repeatedGrep
            , promptKeymap = M.fromList $ (M.toList $ myXPKeymap r)++[
                  ((myModMask, xK_b), addOrTruncateTillPrefix "vb ")
                , ((myModMask, xK_c), addOrTruncateTillPrefix "calc ")
                , ((myModMask, xK_k), addOrTruncateTillPrefix "tk ")
                , ((myModMask .|. controlMask, xK_d), cycleDictionaryForDPrompt Prev)
                , ((myModMask, xK_d), cycleDictionaryForDPrompt Next)
                -- fmd related
                , ((myModMask, xK_f), addOrTruncateTillPrefix "fmc ")
                , ((myModMask, xK_n), setInputAndDone "fmc next")
                , ((myModMask, xK_a), setInputAndDone "fmc rate")
                , ((myModMask, xK_t), setInputAndDone "fmc toggle")
                , ((myModMask, xK_s), setInput "fmc setch " >> endOfLine)
                , ((myModMask, xK_w), setInputAndDone "fmc webpage")
                -- system related
                -- suspend after one sec to avoid keyboard-mashing to wake up the machine again
                , ((myModMask, xK_l), setInputAndDone "sleep 1; systemctl suspend")
                -- hot restart
                , ((myModMask, xK_h), setInputAndDone "reboot")
                , ((myModMask, xK_q), setInputAndDone myRestartCmd)
                , ((myModMask, xK_x), setInputAndDone "systemctl poweroff")
                , ((myModMask, xK_z), setInputAndDone "sleep 1; xset dpms force off")
                -- run stuff on a terminal
                , ((myModMask, xK_Return), changeInputAndDone $ \str -> "xterm -e zsh -ic " ++ escapeQuery (str ++ "; zsh"))
            ]} immediate final owi
mkDynamicPrompt i f = mkDynamicPrompt' i f def

dynamicPromptCommand = ("M-r", mkDynamicPrompt) 

promptCommands =
    [ ("M-"++mf++"/", \immi final -> initMatches >>= \r -> mkSearchPrompt (myXPConfig r) {searchPredicate = repeatedGrep} p validWindow (a immi final))
    | (mf, p, a) <- [ ("", "Go to window: ", \i f w -> deminimizeFocus w >> i >> f)
                    , ("S-", "Bring window: ", \i f w -> shiftWindowsHere [w] >> i >> f)
                    ]
    ]
    ++
    [ ("M-s " ++ af, \immi final -> do
        r <- initMatches
        -- get the current workspace name
        name <- getCurrentWorkspaceName
        renameWorkspacePrompt (myXPConfig r) {
              searchPredicate = repeatedGrep
            , defaultText = name
        } immi final)
    | af <- ["S-c"] ]
    ++
    [ ("M-"++mf++"s "++nk++mk++af, fun)
    | (mf, action, prompt) <- [ ("", windows . W.greedyView, "workspace")
                              , ("S-", \t -> onSelectedWindowsAfterMovingToTmpSpace $ \wins ->  windows (shiftWins t wins), "move to workspace") ]
    , (af, pos) <- insertPositionKeys
    , (nk, n) <- numberKeys
    , (mk, fun) <- [ ("", \immi final -> initMatches >>= \r -> newWorkspacePrompt (myXPConfig r) {searchPredicate = repeatedGrep, autoComplete = Nothing} (prompt++" ("++(show pos)++")") pos n $ \t -> do
                                    action t
                                    immi
                                    final)
                   , ("M-", \immi final -> do
                       nm <- getCurrentWorkspaceName
                       p <- getCurrentWorkspaceDirectory 
                       insertWorkspace nm pos n p action
                       immi
                       final)
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
    , ("M-g u", U.withUrgents $ flip whenJust deminimizeFocus . listToMaybe)
    -- ugly interface to window activate, which tries to activate the window given by the title under ~/.xmonad/.winactivate
    , ("M-C-M1-x", do
            lns <- fmap lines $ io (readFile "/home/lingnan/.xmonad/.winactivate") `catchX` (return "")
            case lns of
                 h:_ -> deminimizeFocus (read h) `catchX` (return ())
                 _ -> return ()
      )
    ----- wallpaper invoke -- {{{
    , ("M-z", initMatches >>= \r -> mkWPPrompt toggleFadeSet (myXPConfigWithQuitKey (myModMask .|. controlMask, xK_Return) r) {
          autoComplete = Just 0
        , searchPredicate = repeatedGrep
    })
    , ("<F10>", spawn $ "amixer get Master | fgrep '[on]' && amixer set Master mute || amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F11>", spawn $ "amixer set Master 5-; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F12>", spawn $ "amixer set Master 5+; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    ]

workspaceCommands = concatMap (processKey . addPrefix) $
    -- workspace prompt interface
    [ (pk++mk++wk, gt >>= flip whenJust ta) 
    | (mk, ta) <- [ ("", windows . W.greedyView)
                  , ("S-", \t -> onSelectedWindowsAfterMovingToTmpSpace $ \wins ->  windows (shiftWins t wins)) 
                  , ("C-", swapWith)
                  ]  
    , (pk, wk, gt) <- [("", "6", fmap Just lastWorkspaceTag)]
                      ++
                      [ (nk, dk, do
                          st <- workspaceStack
                          let (ls, mct) = case st of
                                               Just (W.Stack f us ds) -> (if d == Prev then us else ds, Just $ W.tag f)
                                               _ -> ([], Nothing)
                              mn' = clamp (n - 1) 0 (length ls - 1)
                              mft = fmap (W.tag . (ls !!)) mn'
                          return $ if mft == mct then Nothing else mft)
                      | (dk, d) <- [("[", Prev), ("]", Next)]
                      , (nk, n) <- numberMotionKeys
                      ]
                      ++
                      [ ("", t, fmap Just $ (if mk /= "S-" then toggleTag else quickWorkspace) t)
                      | t <- quickWorkspaceTags]
    ]
    -- the find motion (just for completeness)
    ++
    [ ("f M-"++charToKeyStroke t, quickWorkspace [t] >>= windows . W.greedyView) 
    | t <- symbolSequence ]
    ++
    -- workspace motion key g M-S-0 and g M-S-4
    [ ("g M-0", allWorkspaceTags >>= windows . W.greedyView . head)
    , ("g M-S-4", allWorkspaceTags >>= windows . W.greedyView . last) ]
    ++
    [ (mk++"e", f)
    | (mk, f) <- [ ("", onNextNeighbour W.view)
                 , ("S-", onSelectedWindowsAfterMovingToTmpSpace $ \wins -> onNextNeighbour $ \t -> shiftWins t wins)
                 , ("C-", onNextNeighbour W.greedyView)]
    ]
    -- }}}
    ----- Loghook-- {{{
    {-, ("M-S-f", withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)-}
    -- }}}

------------- marco recording

-- requirement for macro names
-- - no number (number will be interpreted as number of times instead)
-- - no two macros shall have the same prefix (limitation of the keyboard system)
-- - it should not be "/" or anything with that prefix (it's not really like that you can make that in the file system out as well)
myMacrosDir = "/home/lingnan/.macros"
writeData file d = io $ do
  writeFile file (show d) `E.catch` \(SomeException e) ->
                          hPutStrLn stderr ("error writing data: "++show e)
  setFileMode file mode
    where mode = ownerReadMode .|. ownerWriteMode

readData file = io $ readData `E.catch` \(SomeException _) -> return Nothing
 where
    readData = do
        xs <- bracket (openFile file ReadMode) hClose hGetLine
        return $ readMaybe xs

data MacroModeStorage = MMS (Maybe (String, [String])) deriving Typeable
instance ExtensionClass MacroModeStorage where
    initialValue = MMS Nothing

appendMacroKey key = do
    MMS a <- XS.get
    case a of
         Just (n, ls) -> XS.put $ MMS $ Just (n, ls++[key])
         _ -> return ()

retrieveMacro :: [(String, X () -> X () -> X ())] -> [String] -> X () -> X ()
retrieveMacro bindingList list = foldl g id list
            where g z k a = do
                      let x = maybe (>>) snd $ find ((==k) . fst) bindingList
                      z (x (return ()) a)

toggleMacroMode name = do
    MMS m <- XS.get
    case m of
         Just (n, a:as) -> writeData (myMacrosDir ++ "/" ++ n) (a:as)
         _ -> return ()
    case name of
         Just n -> XS.put $ MMS $ Just (n, [])
         _ -> XS.put $ MMS Nothing
    runLogHook

retrieveMacroCommands keybindings = flip E.catch (\(SomeException e) -> return []) $ do
    fs <- fmap (filter (not . (=~ "^\\."))) $ getDirectoryContents myMacrosDir
    let allMacroNames = nubBy (\a b -> let (s, l) = if length a < length b then (a, b) else (a, b)
                                       in s `isPrefixOf` l
                              ) $ fs 
                                ++
                                fmap fst defaultMacrosMap
                                ++ 
                                fmap wrapList (alphas ++ "._-")
        allMacroKeys = zip (fmap (joinStr " " . fmap charToKeyStroke) allMacroNames) (fmap (return . Just) allMacroNames)
                       ++
                       [ ("/", do
                            r <- initMatches
                            let conf = (myXPConfig r) {
                                    searchPredicate = prefixSearchPredicate
                                }
                                compl s = return $ filter ((searchPredicate conf) s) allMacroNames
                            inputPromptWithCompl conf "Enter the name of the macro" compl
                         )
                       ]
    return $ concatMap (processKey . addPrefix) $
        [ ("a "++nk++sk, do
            -- first try to play that as a xmonad macro
            s' <- ms
            case s' of
                 Just s -> do
                        mx <- readData (myMacrosDir ++ "/" ++ s) 
                              >>= return . maybe (fmap snd $ find ((==s) . fst) defaultMacrosMap) Just 
                              >>= return . fmap (retrieveMacro keybindings)
                        case mx of
                             Just x -> (iterate x (return ())) !! n
                             _ -> do
                                 let script = "str=\"`cat "++escapeQuery (myMacrosDir ++ "/" ++ s)++"`\"; [ -z \"$str\" ] && exit; res=\"$str\"; for ((i=1; i<"++show n++"; i++)); do res=\"$res $str\"; done; sleep 0.1; xdotool type \"$res\""
                                 spawn script
                 _ -> return ()
          )
        | (sk, ms) <- allMacroKeys
        , (nk, n) <- numberKeys
        ]
        ++
        [ ( "q " ++ k, n >>= toggleMacroMode) 
        | (k, n) <- [ ("<Esc>", return Nothing) ]
                    ++
                    allMacroKeys
        ]

-- provides some macros in code that do not need the user to specify one by one
-- each is a pair of (name, list of keybindings)
defaultMacrosMap:: [(String, [String])]
defaultMacrosMap =
    [
    ]
    -- split a new window in a given direction
    -- [ (d++tk, [trigger, "M-C-"++d]) 
    -- | d <- ["h", "l", "j", "k"]
    -- , (tk, trigger) <- [ (filterKeyList $ filterKey g, "M-c i "++(filterKey g)) 
    --                    | g <- allTaskGroupsWithFilterKey ["c"]]
    --                    ++
    --                    [ ("/", "M-r") ]
    -- ]

-- macroRemapCommands are a complement of the defualt Macros Map; they use the macro mechanism 
-- but are more flexible in terms of their mappings
macroRemapCommands keybindings =
    [ (k, (iterate (retrieveMacro keybindings macrols) (return ())) !! n) 
    | (nk, n) <- numberKeys
    , (k, macrols) <- [ ("M-c "++nk++"C-"++d++" "++tk, ["M-<Esc>", trigger, "M-C-"++d])
                      | d <- ["h", "l", "j", "k"]
                      , (tk, trigger) <- [ (filterKey g, "M-c i "++(filterKey g)) 
                                         | g <- allTaskGroupsWithFilterKey ["c"]]
                                         ++
                                         [ ("/", "M-r") ]
                      ]
    ]

        

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
data VisualModeMarker = VisualModeMarker (Maybe ((Window, Window), (Window, Window), Window)) deriving (Typeable, Read, Show)
instance ExtensionClass VisualModeMarker where
    initialValue = VisualModeMarker Nothing
    extensionType = PersistentExtension

allDiff a b = S.difference  (S.union a b) (S.intersection a b)
enterVisualMode mode = withFocused $ \f -> do
    XMS m <- XS.get
    let process win clearls = do
            XS.put (XMS (Visual mode)) 
            -- clear marker
            XS.put $ VisualModeMarker Nothing
            -- clear active selections first
            VisualSelections (ac, pass) <- XS.get
            XS.put $ VisualSelections (S.empty, foldr S.delete pass clearls)
            -- update 
            visualModeUpdateEndMarker win
        clear = do
            -- depending on the mode we will clear other selections
            gs <- fmap (if mode == Col then id else maybeToMaybe G.bases) G.getCurrentGStack
            case gs of
                 Just (G.Node (W.Stack f u d)) -> return $ concatMap G.flattened $ u++d
                 _ -> return []
    case m of
         Visual mode' | mode' == mode -> exitVisualMode
                      | otherwise -> do
                          -- first get the original window
                          VisualModeMarker m <- XS.get
                          case m of
                               Just (_, _, fo) -> return [] >>= process fo >> visualModeUpdateEndMarker f
                               _ -> clear >>= process f
         _ -> clear >>= process f
    refresh

-- with the toggle set we check if any of the windows are already selected, only when all of them are selected then we will revert
visualModeUpdateToggleSet ls = do
    VisualSelections (ac, pass) <- XS.get
    let ss = S.fromList ls
        nac = if S.null (S.difference ss ac)
                 then S.difference ac ss
                 else S.union ac ss
    XS.put $ VisualSelections $ (nac, pass)
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
                     Just ((bb, be), (eb, ee), fo) -> do
                         XS.put $ VisualSelections (allDiff ac $ allDiff nws ows, pass)
                         XS.put $ VisualModeMarker $ Just ((bb, be), (wb, we), fo) 
                        where ows = computeDiff (bb, be) (eb, ee)
                              nws = computeDiff (bb, be) (wb, we)
                     Nothing -> do
                         XS.put $ VisualSelections (computeDiff (wb, we) (wb, we), pass)
                         XS.put $ VisualModeMarker $ Just ((wb, we), (wb, we), w) 
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

-- note that due to some restriction the apply selected window stack is now only used on the focused stack 
-- whereas in the visual mode other operations will still apply to the whole
getSelectedWindowStack' useFocusStackOnly = do
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
    let mb = if inv && not useFocusStackOnly then mbases else maybeToMaybe G.current mbases
    return $ if isNothing mb 
               then (Nothing, Nothing)
               else let bs = fromJust mb
                    in case (filterBase ft bs, mf) of
                        ((Just gs, ops), _) -> (G.toZipper gs, ops)
                        (_, Just f) -> (W.differentiate [f], Nothing)
                        _ -> (Nothing, Nothing)

getSelectedWindowStack = fmap fst $ getSelectedWindowStack' False

applySelectedWindowStack :: Bool -> (Maybe (W.Stack Window) -> X ()) -> X ()
applySelectedWindowStack reversal fun = do
    ss <- getSelectedWindowStack' True
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

paste invertInsert normalP xls register = do
    InsertOlderToggle t <- XS.get
    ws <- case register of 
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
                 _ -> xls register
    -- here's the tricky part about this: when we order windows, we prioritise the more recently minimized windows; however, when we paste windows from registers, we need to reverse that such that the old windows come first
    let wins = reverse ws
    if not (null wins)
       then do
            markWindowsSelection wins
            let t' = if invertInsert then not t else t
            case (t', normalP) of
                 -- focus the last window (as in vim which places cursor on the last element)
                 (False, True) -> shiftWindowsHereAndFocusLast True (Just (last wins)) wins
                 (False, False) -> shiftWindowsHereAndFocusLast False (Just (last wins)) wins
                 (True, True) -> shiftWindowsHereAndFocusLast True Nothing wins
                 (True, False) -> shiftWindowsHereAndFocusLast False Nothing wins
       else return ()

-- restoreMinimizedWindowsAndSelect' [] = return ()
-- restoreMinimizedWindowsAndSelect' wins = markWindowsSelection wins >> mapM_ deminimizeFocus wins 
-- restoreMinimizedWindowsAndSelect = getCurrentMinimizedWindows >>= restoreMinimizedWindowsAndSelect'
-- restoreMinimizedWindowAndSelect = getCurrentMinimizedWindows >>= restoreMinimizedWindowsAndSelect' . wrapList . head
--
-- calling minimize windows assume that you've already handled the focus problem gracefully
-- minimizeWindows' tag ls = mapM_ (\w -> routeMessageToWS ((==tag).W.tag) (MinimizeWin w)) ls
    -- clear the focus if necessary
-- a register of "" means no register shall be used (in that case we just push into the default list of registers

-- we will now simulate the aspect of the easyclip (d defaults to kill; and m becomes move)
-- the move will remember the windows and in addition the windows wouldn't expire

cut add register ls = flip correctFocus ls $ \wins -> do
                        deleteWindowsSelection wins
                        mapM_ minimizeWindow wins
                        if register /= ""
                           then do
                               -- clensing the tags for these windows (when we move/cut windows into a register, we'd expect the original tag lost)
                               mapM_ unTag wins
                               -- put these windows into that register
                               putWindowsIntoRegister add register wins 
                           else let mv t rep _ = case t of
                                        -- we should check if this is the only tag the windows have
                                         -- "9" -> filterM (\w -> do
                                         --        ts <- getTags w
                                         --        return $ ts == []
                                         --     ) rep >>= killWindows
                                         "9" -> return ()
                                         _ -> mvWindows (show $ (read t)+1) mv rep 
                                -- pushing these windows into 1, 1->2, 9 out of the memory
                                in mvWindows "1" mv wins 
                        putWindowsIntoRegister False "\"" wins 

delete add register ls = if register == "" then killWindows ls else cut add register ls
yank add register ls = do
    putWindowsIntoRegister add register ls 
    putWindowsIntoRegister False "\"" ls 

mvWindows nr fun wins = do
    rep <- orderedWindowsMatchingPredicate (hasTagQuery nr) 
    mapM_ (delTag nr) rep
    mapM_ (addTag nr) wins
    fun nr rep wins

putWindowsIntoRegister add register wins = do
    if not add then orderedWindowsMatchingPredicate (hasTagQuery register) >>= mapM_ (delTag register) else return ()
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
 

appendablize (k, ka) = (k, \immi final -> ka >> immi >> final)
appendImmediate f (k, ka) = (k, \immi final -> ka (f >> immi) final)
normalize = fmap (\(k, ka) -> (k, ka (return ()) (return ())))
-- this is the version that allows for appending an action when the designed action has finished
myAppendableKeys :: IORef (S.Set Window) -> [(String, X () -> X () -> X ())]
myAppendableKeys toggleFadeSet =
    fmap (\(k, ka) -> flip appendImmediate (k, ka) $ appendMacroKey k)
       (fmap appendablize (visualCommands ++ motionKeyCommands)
        ++
        fmap (appendImmediate $ ifInVisualMode exitVisualMode $ return ())
           (fmap appendablize (layoutCommands ++ historyCommands ++ miscCommands toggleFadeSet ++ workspaceCommands)
            -- dynamic prompt
            ++
            [dynamicPromptCommand]
            ++
            promptCommands
            ++
            fmap appendablize (concatMap (processKey . addPrefix)
            [ (nk++".", getLastCommand >>= sequence_ . take n . repeat . id) 
            | (nk, n) <- numberMotionKeys])
            ++
            fmap (\(k, ka) -> flip appendImmediate (k, ka) $ saveLastCommand $ ka (return ()) (return ()))
               (fmap appendablize (cutCommands ++ pasteCommands ++ yankCommands)
                ++
                changeCommands
               )
           )
       )

myKeys toggleFadeSet = do
    let keys = myAppendableKeys toggleFadeSet
    macroCommands <- retrieveMacroCommands keys
    return $ (normalize keys) ++ macroCommands ++ macroRemapCommands keys

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
    allKeys <- myKeys toggleFadeSet
    -- urgencyhook is not used currently due to conflict with the wallpaper system
    xmonad $ ewmh $ U.withUrgencyHook U.NoUrgencyHook $ myXMonadConfig {
            logHook = myLogHook toggleFadeSet dzenLogBar 
        } `additionalKeysP` allKeys

-- }}}
