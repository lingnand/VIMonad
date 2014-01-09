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
import Data.String.Utils (replace)
import Foreign.C.Types (CLong)
import System.IO
import System.Exit
import System.Directory
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
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.Stack
import XMonad.Util.Themes
import XMonad.Util.Timer
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.InsertPosition
import XMonad.Layout.ImageButtonDecoration
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.XPropManage
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceHandles
import XMonad.Util.WorkspaceDirectories
import XMonad.Actions.FloatKeys
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import qualified Data.Set as S
import qualified XMonad.Layout.Groups as G
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.User
import Text.Regex.Posix
import Control.Applicative
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
-- }}}

---------------- Constants-- {{{

---- ModMask
myModMask = mod5Mask

---- Directories
myXMonadDir     = "/home/lingnan/.xmonad"
myBitmapsDir    = myXMonadDir++"/dzen2"
myScriptsDir    = myXMonadDir++"/scripts"

---- Font
myTerminalFont          = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
{-myFont= "-isas-song ti-medium-r-*-*-*-*-*-*-*-*-iso8859-*"-}
myFont= "xft:WenQuanYi Zen Hei Mono:pixelsize=11"
myBigFont= "xft:WenQuanYi Zen Hei Mono:pixelsize=12"
myDzenFont= "WenQuanYi Zen Hei Mono:pixelsize=10"

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
robotTheme = def { activeColor         = myBgHLight
                 , inactiveColor       = myBgDimLight
                 , activeBorderColor   = myBorderHLight
                 , inactiveBorderColor = myBorderColor
                 , activeTextColor     = myFgHLight
                 , inactiveTextColor   = myFgColor
                 , fontName            = myFont
                 , decoHeight          = 16 
                 , subThemeForWindow   = \w -> taskGroupOfWindow taskGroups w >>= return . fmap colorScheme
                 }

{-myTabsTheme = robotTheme { subThemeForWindow = \_ -> return Nothing }-}
myTabsTheme = robotTheme
mySubTheme = SubTheme { winInactiveColor = inactiveColor myTabsTheme
                      , winInactiveBorderColor = inactiveBorderColor myTabsTheme
                      , winInactiveTextColor = inactiveTextColor myTabsTheme
                      , winActiveColor = activeColor myTabsTheme
                      , winActiveBorderColor = activeBorderColor myTabsTheme
                      , winActiveTextColor = activeTextColor myTabsTheme
                      }

---- Promptscheme
myXPConfig = defaultXPConfig { 
    font = myBigFont
    , bgColor               = myBgDimLight
    , fgColor               = myFgColor
    , bgHLight              = myBgHLight
    , fgHLight              = myFgHLight
    , promptBorderWidth     = 0
    , height                = decoHeight myTabsTheme
    , historyFilter         = deleteConsecutive
    , autoComplete       = Just 250000
    , alwaysHighlight = False
    , searchPredicate = stdSearchPredicate
    , promptKeymap = emacsLikeXPKeymap
}

myInfoXPConfig = myXPConfig {
    {-fgColor = myFgHLight-}
    {-, bgColor = myBgHLight-}
    fgColor = "#d6c3b6"
    , bgColor = "#262729"
    , fgHLight = "#d6c3b6"
    , bgHLight = "#262729"
    , autoComplete = Nothing
    , alwaysHighlight = False
}

--- Status bar 

dzenBar x y w h ta fg bg font = "dzen2 -x '"++(show x)++"' -y '"++(show y)++"' -h '"++(show h)++"' -w '"++(show w)++"' -ta '"++ta++"' -fg '"++fg++"' -bg '"++bg++"' -fn '"++font++"'"
conky script = "conky -qc '"++script++"'"
pipe a b = a++" | "++b
trayer edge align w h tint alpha = "trayer --edge "++edge++" --align "++align++" --widthtype pixel --width "++show w++" --height "++show h++" --expand false --tint 0x"++tail tint++" --transparent true --alpha "++show alpha++"&"

myMusicBarStdWidth = 350
myStatBarWidth = 450
myDzenBarHeight = 14
myDzenBarOverlap = 5


---- Terminal settings
myTerminal = "xterm"

-- a relaxed version of uniqueTermHasCmd that also includes terminals that are not instances of UniqueTerm
isTerm = className =? "XTerm" 
hasCmd c = fmap (any (c `isInfixOf`)) $ getQuery wM_COMMAND
isTermWithCmd c = isTerm <&&> hasCmd c
-- note that args, cmd are NOT quoted; this is for maximum flexibility - you can then use parameter expansion, etc. if you'd like
termFullCmd title name args cmd = shellFullCmd $ myTerminal++prop "title" title++prop "name" name++" "++args++" -e "++cmd
    where escapeQuote = replace "'" "\\'"
          prop s v = if null v then "" else " -"++s++" $'"++(escapeQuote v)++"'"
shellFullCmd cmd dir = "cd $'"++(escapeQuote dir)++"'; "++cmd
    where escapeQuote = replace "'" "\\'"
-- runTerm will ALWAYS start the program with the current directory
runTerm title appname cmd = getCurrentWorkspaceDirectory >>= spawn . termFullCmd title appname "" cmd
runShell cmd = getCurrentWorkspaceDirectory >>= spawn . shellFullCmd cmd

-- we give up on a dynamically typed UniqueTerm (depending on the type of Context) because it might be easy to query for command only if you don't know what type of the context is
data UniqueTerm = UniqueTerm { context :: S.Set String
                             , command :: String 
                             , args    :: String
                             } deriving (Show, Read, Eq)

uniqueTerm cons = UniqueTerm (S.fromList cons)

uniqueTermAppName :: UniqueTerm -> String
uniqueTermAppName = show

uniqueTermFullCmd dir ut = termFullCmd (command ut) (uniqueTermAppName ut) (args ut) (command ut) dir

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

runUniqueTerm dir = spawn . uniqueTermFullCmd dir
runUniqueTermWithCurrentWSDir ut = getCurrentWorkspaceDirectory >>= \d -> runUniqueTerm d ut

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
changeWallpaperLastCmd = changeWallpaperBin++" -L"
changeWallpaperCmd = changeWallpaperBin++" -H"
wallpaperChangeInterval = 900
wallpaperRecoverInterval = 1.0
defaultFadeInactiveAmount = 0.85
wallpaperFadeInactiveAmount = 0.3
wallpaperFadeFullAmount = 0.15
wallpaperFullRecoverInterval = 1.5

---- Vimb prompt constants
vbhistorySize = 10
vbMatchSize = 10

-- }}}

---------------- StartupHook -- {{{
myStartupHook = do
    {-setWMName "LG3D"-}
    {-wallpaperAutomaticStartHook-}
    -- initiate the wallpaper
    spawn changeWallpaperLastCmd
    -- we will only need to switch to the right workspace during startup
    curr <- currWorkspace
    if not (validWS curr) then startTimer 0.5 >>= XS.put . StartWSChangeState else return ()
-- }}}

---------------- RestartCmd -- {{{
myKillCmd = "killall dzen2 conky fmclrc trayer;"
myRestartCmd = myKillCmd ++ "xmonad --recompile; xmonad --restart"
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
deminimize w = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    case find ((w `elem`) . W.integrate' . W.stack) wss of
         Just ws
            | W.tag ws /= curr -> sendMessageWithNoRefresh (RestoreMinimizedWin w) ws 
            | otherwise -> sendMessage (RestoreMinimizedWin w)
         _ -> return ()

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
focusNextKillWindow w = withFocused $ \f -> if w == f then correctFocus kill else killWindow w

nextFocus (G.Leaf (Just (W.Stack _ u (d:_)))) = Just d
nextFocus (G.Leaf (Just (W.Stack _ (u:_) _))) = Just u
nextFocus (G.Leaf _) = Nothing
nextFocus (G.Node (W.Stack f u d)) = 
    case (nextFocus f, u, d) of
         (Just r,_,_) -> Just r
         (Nothing,_,d:_) -> G.focal d
         (Nothing,u:_,_) -> G.focal u
         _ -> Nothing

--- correct the focus of the window after doing the action 'a'; the rule for correcting is to assume that the current window gets closed after a is applied and the next window in the stack is selected after that
correctFocus a = withFocused $ \f -> do
        isf <- runQuery isFloating f
        mgs <- G.getCurrentGStack
        a
        if isf then nextMatch History $ isInCurrentWorkspace <&&> fmap not isFloating
               else case fmap nextFocus mgs of
                        Just (Just nw) -> withFocused $ \nf -> if nf == nw then return () else focus nw 
                        _ -> return ()

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
    WallPaperToggleState ts <- XS.get
    if not ts then fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount else return ()
    historyHook

---- fade Log hook-- {{{
------- the conditions for fading windows in normal circumstances
-- not used currently
isFloating :: Query Bool
isFloating =  ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset
-- windows that shall not be faded no matter what
doNotFadeOutWindows =  className =? "xine" <||> className =? "MPlayer"

-- windows that shall not be faded given the class name
disableFadingWithinClassName cn = liftM not (className =? cn <&&> (liftX  $ focusedHasProperty (ClassName cn)))
disableFadingWithinClassNames = disableFadingWithinClassName "jetbrains-idea"
   
-- tests if a window should be faded; floats are the windows that have been toggled by the user to not fade
defaultFadeTest floats =
    liftM not doNotFadeOutWindows <&&> isUnfocused <&&> disableFadingWithinClassNames <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)
     
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
    let (plus, c) = if s == 0 then (" ", myFgColor) else ("+", myNotifyColor)
        [lb,rb] = fmap (dzenColor myFgColor myBgColor) $ ["[","]"]
        autoIndicator = if isAuto then dzenColor myNotifyColor myBgColor "A" else "M"
        insertOlderIndicator = if t then dzenColor myNotifyColor myBgColor "O" else "N"
    return $ Just $ lb++autoIndicator++insertOlderIndicator++"-"++dzenColor c myBgColor plus++rb

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
    let wl = zip (fmap ((++".").show) (enumFrom 1)) $ repeat ""
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
        , ppTitle       = wrap ("^fg(#222222)^i("++myBitmapsDir++"/corner_left.xbm)^bg(#222222)^fg(#4c7899)^fn(fkp)x^fn()") ("^fg(#222222)^i("++myBitmapsDir++"/corner_right.xbm)") .  dzenColor myFgColor "#222222" . shorten 50 . pad        
        , ppOrder   =  \(ws:l:t:hist:gp:hid:xs) -> [ws,l,hist,hid,gp] ++ xs
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
        statbarw = 576
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
myHandleEventHook toggleFadeSet e = do
    startWSSwitchHook e 
    handleKeyEventForXMonadMode e

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

toggleInsertOlderRecover = do
    InsertOlderToggleBackUp (h, b) <- XS.get
    if h then do
        XS.put $ InsertOlderToggleBackUp (False, False)
        XS.put $ InsertOlderToggle b
         else return ()

toggleInsertOlder = do
    XS.put $ InsertOlderToggleBackUp (False, False)
    InsertOlderToggle t <- XS.get
    XS.put $ InsertOlderToggle $ not t

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

      NS "mutt" (uniqueTermFullCmd "" uniqueMutt) (isUniqueTerm uniqueMutt) lowerHalfRectHook,
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
mkPerWSScratchpad cmd = do
    curr <- gets (W.currentTag . windowset)
    con <- perWSScratchpadContext curr
    dir <- getCurrentWorkspaceDirectory
    let csterm = UniqueTerm con cmd ""
    mkNamedScratchpad [ NS "cs" (uniqueTermFullCmd dir csterm) (isUniqueTerm csterm) idHook ] "cs"

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

workspacePrompt :: XPConfig -> String -> (WorkspaceId -> String -> X ()) -> (String -> String -> X ()) -> X ()
workspacePrompt conf p ef f = do 
    wts <- allWorkspaceTags
    wns <- fmap (fmap (\(a,b)->if null b then a else a++":"++b) . zip wts) allWorkspaceNames
    let complFun s = if null extWns 
               then fmap lines $ runProcessWithInput "symtag" ["print", "false", "%t", show tagLimit, ".*"++s++".*"] ""
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
            output <- runProcessWithInput "symtag" ("print" : if '/' `elem` s then ["true", "%p", "1", ".*"++s] else ["false", "%p", "1", ".*"++s++".*"]) ""
            let ls = lines output
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

removeCurrentWorkspace = do
    -- our methodology is simple, remove the current workspace and reorder the symbol stream for the tags
    -- so this involves repairing the tags with the associated handles
    -- we can savely remove the workspace if all the workspaces after this workspace are empty
    killAll
    curr <- gets (W.currentTag . windowset)
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

data WallPaperTidState = WallPaperTidState TimerId deriving Typeable
instance ExtensionClass WallPaperTidState where
    initialValue = WallPaperTidState 0

-- wallpaperToggle is designed to toggle the window opacity and STAYS
data WallPaperToggleState = WallPaperToggleState Bool deriving Typeable
instance ExtensionClass WallPaperToggleState where
    initialValue = WallPaperToggleState False

-- the hook tries to execute the command (presumably related to changing the wallpaper) and fade ALL windows for wallpaper to shine through if they are not already faded
wallpaperChangeFadeFullHook cmd = do
        spawn cmd
        fadeOutLogHook $ fadeIf (return True) wallpaperFadeFullAmount
        XS.put $ WallPaperToggleState True

-- toggle the wallpaper toggle state; toggle the wallpaper gallery mode
wallpaperToggleHook toggleFadeSet = do 
        WallPaperToggleState ts <- XS.get
        XS.put $ WallPaperToggleState $ not ts
        fadeOutLogHook $ if ts then fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount else fadeIf (return True) wallpaperFadeFullAmount
-- }}}

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
myFasdBin = "fasd"
symtaglibroot = "~/DB/"
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

shortend home s = maybe s ("~"++) $ stripPrefix home s
expandd home s = maybe s (home++) $ stripPrefix "~" s
correctDir home d = io (canonicalizePath (if null d then home else expandd home d))
stripSuffix suf s = fmap reverse $ stripPrefix (reverse suf) (reverse s)
isOutput = (>= outputWidth) . length
limitSpace l = take l . fillSpace l

-- search widgets
fasdLimit = 20
findLimit = 20
tagLimit = 20
grepLimit = 20
whichLimit = 20
evalLimit = 20
isGrepOutput s = isOutput s && ':' `elem` s
stripGrepOutput = head . splitOn ":"
shortcutArgs args = case reverse args of
                           (ha:a):pas | ha `elem` "sm" && length a <= 1 -> (reverse pas, [ha:a])
                           _ -> (args, [])
isShortcutOutput s = length s > 2 && (s!!1) == ':'
stripShortcutOutput = last . splitOn ":"
isSearchFilter s = s `elem` ["f", "a", "d", "z", "l", "t", "g", "which"] || ((length s) `elem` [1, 2] && (head s) `elem` "sm")
-- dirty hack // check if the completion starts with path
isValidPath s = (head s) `elem` "~/"

data PromptWidget = PromptWidget { promptPrefix :: String
                                 , promptCommandToComplete :: String -> String
                                 , promptNextCompletion :: String -> [String] -> String
                                 , promptComplFunction :: ComplFunction
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
isWidgetFilter = flip elem (fmap promptPrefix dynamicPromptWidgets)
widgetCmd w c = let p = promptPrefix w++" " in joinStr p $ tail $ splitOn p c
findWidget pre = find ((==pre) . promptPrefix) dynamicPromptWidgets
findWidgetForAction c = fmap (\(r, w) -> (fromJust r, w)) $ find (isJust . fst) $ fmap (\w -> (stripPrefix (promptPrefix w ++ " ") c, w)) dynamicPromptWidgets

dynamicPromptWidgets = [
        -- this follow the (prefix, prompt) format
        dwgt VBPrompt "vb" vbComplFunc vbAction
      , dwgt defaultSDMode "sdcv" (completionFunction defaultSDMode) (flip (modeAction defaultSDMode) "")
      , dwgt TaskPrompt "tk" taskComplFunc taskAction
      , dwgt FMCPrompt "fmc" fmcComplFunc fmcAction
      , dwgt CalcMode "calc" (completionFunction CalcMode) (flip (modeAction CalcMode) "")
    ]

evalStr s = let evalcomps = splitOn "`" s
                evallen = length evalcomps 
                t = tail evalcomps
            in if odd evallen && evallen >= 3 then (head evalcomps, head t, joinStr "`" $ tail t)
                                              else (s,"","")

dpromptNextCompletion c l = case break (\s -> isSearchFilter s || isWidgetFilter s) args of
                   (bef, "z":_:_) -> joinStr " " $ bef ++ ["c", escape (head l)]
                   (bef, "g":_:_) -> joinStr " " $ bef ++ [stripGrepOutput $ escape (head l)]
                   (bef, ['s':pre]) -> exactMatch $ fmap stripShortcutOutput l
                   (bef, ('m':pre):_) -> c
                   (bef, "which":_) | all isValidPath l -> joinStr " " $ bef ++ [escape (head l)]
                                    | otherwise -> exactMatch l
                   (bef, w:_:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                           in joinStr " " $ bef ++ [w, promptNextCompletion wp (widgetCmd wp c) l]
                   (bef, _:_:_) -> joinStr " " $ bef ++ [escape (head l)]
                   _ | all isGrepOutput l -> exactMatch $ fmap stripGrepOutput l
                     | all isShortcutOutput l -> exactMatch $ fmap stripShortcutOutput l
                     | not (null evalstr) ->  evall ++ escape (head l) ++ evalr
                     | all isOutput l && any ("commit" `isPrefixOf`) l && any ("Author" `isPrefixOf`) l && any ("Date" `isPrefixOf`) l -> exactMatch $ fmap (fromJust . stripPrefix "commit " . trim) $ filter ("commit " `isPrefixOf`) l
                     | isOutput (head l) -> c
                     | otherwise -> exactMatch l
             where lastArg = last $ args
                   sas = shortcutArgs args
                   args = parseShellArgs c
                   (evall, evalstr, evalr) = evalStr c
                   exactMatch ls = fromMaybe "" (stripSuffix lastArg c) ++ (escape $ exactNext ls)   
                   exactNext ls = let rev = reverse ls in rev !! case findIndex (== unescape lastArg) rev of
                               Just i -> if i <= 0 then length ls - 1 else (i-1)
                               Nothing -> length ls - 1 

dpromptHighlightPredicate cl cmd = case break (\s -> isSearchFilter s || isWidgetFilter s) args of
                (bef, w:_:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                            in promptHighlightPredicate wp cl (widgetCmd wp cmd)
                _ | isShortcutOutput cl -> stripShortcutOutput cl == unescapedLastArg
                  | isGrepOutput cl -> stripGrepOutput cl == unescapedLastArg
                  | trimcl =~ "commit [0-9a-z]{40}" -> last (words trimcl) == unescapedLastArg
                  | isOutput cl -> False
                  | otherwise -> not (null args) && unescapedLastArg == cl 
             where args = parseShellArgs cmd
                   unescapedLastArg = unescape $ last args 
                   trimcl = trim cl

whenNull ma mb = do
    l <- ma
    if null l then mb else return l

dpromptComplFunc c home dir fasdf fasdd cmds s = do
        let args = parseShellArgs s
            lastArg = if null args then "" else last args
            unescapedArgs = map unescape args
            sht = shortend home
            epd = expandd home
            sp = searchPredicate c
            fasd aft set = return $ take fasdLimit $ filter (sp $ joinStr " " aft) set
            sct pre = fmap lines $ runProcessWithInput (myScriptsDir++"/xshortcut") [ "print", pre ] ""
            ntailsp = length $ takeWhile isSpace (reverse s)
            output pro ags = fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput pro ags ""
            (_,evalstr,_) = evalStr s
            trycmp = foldl whenNull (return [])
            scopecmp = case reverse unescapedArgs of
                                   "":fa:_ | ntailsp == 1 -> fmap (fmap (fillSpace outputWidth) . lines) $ runProcessWithInput "/home/lingnan/bin/scope" [epd fa, show outputWidth, show outputHeight, show outputHeight] ""
                                   _ -> return [] 
            shellcmp = let scs = if length args > 1 then [] else cmds
                           sas = if (head unescapedArgs) `elem` ["c", "cd"] then ["directory"] else ["file"]
                       in getShellComplWithDir dir False sas scs $ epd lastArg
        fmap (fmap sht) $ case break (\s -> isSearchFilter s || isWidgetFilter s) unescapedArgs of
                    (_, "f":af:afs) -> fasd (af:afs) fasdf
                    (_, "d":af:afs) -> fasd (af:afs) fasdd
                    (_, "a":af:afs) -> fasd (af:afs) $ fasdf++fasdd
                    (_, "z":af:afs) -> fasd (af:afs) $ fasdd
                    (_, ['s':pre]) -> sct pre
                    (_, ['m':pre]) -> sct pre
                    (_, "l":aft) -> fmap lines $ runProcessWithInput (myScriptsDir++"/xfind") (show findLimit:aft) ""
                    (_, "t":aft) -> fmap (fmap (symtaglibroot++) . lines) $ runProcessWithInput "symtag" [ "print", "false", "%t", show tagLimit, joinStr ".*" aft ] ""
                    (_, "g":aft) -> fmap (fmap (limitSpace outputWidth) . filter (':' `elem`) . lines) $ runProcessWithInput (myScriptsDir++"/xgrep") ([ show grepLimit, "-R" ] ++ aft ++ [ "." ]) ""
                    (_, "which":af:afs) -> 
                        -- test if af is one of the commands
                        if af `elem` cmds then fmap lines $ runProcessWithInput "which" (af:afs) ""
                                          else return $ take whichLimit $ filter (sp $ joinStr " " (af:afs)) cmds
                    (bef, w:_:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                            in promptComplFunction wp (widgetCmd wp s)
                -- grave key evaluation (evaluate the grave enclosed string in shell and show the output as autocompletion)
                    _ | not (null evalstr) -> fmap (take evalLimit . lines) $ runProcessWithInput "bash" ["-c", evalstr] ""
                      | otherwise -> case unescapedArgs of
                            "man":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xman") $ show outputWidth:show outputHeight:pa:pai:pas
                            "git":"":[] | ntailsp == 1 -> output "git" ["status"]
                            -- only gives the prime command completion on three spaces
                            "git":pa:[] -> return $ filter (sp pa) ["add", "am", "archive", "bisect", "branch", "bundle", "checkout", "cherry-pick", "citool", "clean", "clone", "commit", "describe", "diff", "fetch", "format-patch", "gc", "grep", "gui", "init", "log", "merge", "mv", "notes", "pull", "rebase", "reset", "rm", "shortlog", "show", "stash", "status", "submodule", "tag"]
                            -- in all other instances we should give the log information
                            "git":pa:_ -> trycmp [output "git" $ ["log", "--grep", last unescapedArgs], scopecmp, shellcmp]
                            _ -> trycmp [scopecmp, shellcmp]

dpromptAction c home dir s = 
        -- perform some special actions on some commands
        let args = parseShellArgs s
        in case (findWidgetForAction s, args) of
                (Just (rest, w), _) -> (promptAction w) rest
                (_, ('m':pre):pas) | length pre == 1 -> spawn $ myScriptsDir++"/xshortcut mark "++pre++" "++(joinStr " " pas)
                (_, ha:pas) | ha `elem` ["cd", "c", "z"] -> do
                                 d <- if null pas 
                                         then return home 
                                         else case head pas of
                                                 "-" -> getCurrentWorkspaceOldDirectory
                                                 p -> return $ unescape p
                                 (correctDir home d) `catchX` (return home) >>= saveCurrentWorkspaceDirectory 
                                 dynamicPrompt c
                            | otherwise -> spawn $ shellFullCmd (dphandler++" "++if null s then "." else s) dir

dynamicPrompt c = do
    cmds <- io getCommands
    dir <- getCurrentWorkspaceDirectory
    home <- io $ env "HOME" "/home/lingnan"
    -- to better fasd performace, we can first extract out all the values for the fasd components
    fasdf <- fmap lines $ runProcessWithInput myFasdBin ["-R","-f","-l","-B","viminfo"] ""
    fasdd <- fmap lines $ runProcessWithInput myFasdBin ["-R","-d","-l"] ""
    cdir <- (correctDir home dir) `catchX` (return home)
    io $ setCurrentDirectory cdir
    mkXPrompt (DPrompt $ shortend home cdir) c (dpromptComplFunc c home dir fasdf fasdd cmds) (dpromptAction c home dir)
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
    modeAction CalcMode c tx = spawn $ "calc -- '"++c++"' | xclip"

calcMode :: XPMode
calcMode = XPT CalcMode

-- }}}

----- Dictionary / sdcv prompt -- {{{

sdcvBin = myScriptsDir++"/xsdcv"
chsdcvBin = myScriptsDir++"/chxsdcv"

sdLength = "250"

data StarDictMode = SDMode { bin :: String
                           , prompt :: String
                           , dictName :: String
                           }

instance XPrompt StarDictMode where
    showXPrompt (SDMode _ p _) = p
    commandToComplete _ = id
    completionFunction (SDMode bin _ d) = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput bin [d, s, sdLength] ""
    modeAction _ query _ = safeSpawn "espeak" [query]
    nextCompletion _ c _ =  c
    highlightPredicate _ _ _ = False
mkSDMode p d = XPT $ SDMode sdcvBin p d
mkCHSDMode p d = XPT $ SDMode chsdcvBin p d
defaultSDMode = SDMode sdcvBin "Collins Cobuild 5 > " "Collins Cobuild 5"

defaultModesForInput (c:cs)
    | isNumber c || isSymbol c = [calcMode]
    | c `elem` ['a'..'z'] = [XPT $ defaultSDMode, mkSDMode  "English Thesaurus > " "English Thesaurus", mkSDMode  "Merrian Webster > " "Merrian Webster 10th dictionary"]
    -- we assume that such case means that we need to use Chinese dictionaries
    | otherwise = [mkCHSDMode "现代汉语词典 > " "Modern Chinese Dictionary", mkCHSDMode  "汉语大词典 > " "Chinese Big Dictionary"]
-- }}}

----- Vimb prompt -- {{{

data VBPrompt = VBPrompt

vbNextCompletion cmd ls = last $ words $ ls !! ni
        where ni = case findIndex (vbIsEqualToCompletion cmd) ls of
                      Just i -> if i >= length ls - 1 then 0 else i+1
                      Nothing -> 0 
vbHighlightPredicate = flip vbIsEqualToCompletion
vbAction s = spawn $ "vb "++(escapeQuery s)
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
vbPrompt = mkVBPrompt myXPConfig {
  autoComplete = Nothing
  , promptKeymap = M.fromList $ (M.toList emacsLikeXPKeymap)++[
  ((myModMask, xK_v), quit)
  ]
}

-- }}}

----- Wiki prompt -- {{{
data WikiMode = WkMode

instance XPrompt WikiMode where
    showXPrompt WkMode = "wiki > "
    commandToComplete WkMode = id
    completionFunction WkMode = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput "wiki" [s, "200"] ""
    modeAction WkMode query tx = safeSpawn "vp" ["w",escapeQuery query]

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

escapeQuery m = '\'' : ((escapeq m) ++ "\'")
                          

instance XPrompt WolframMode where
    showXPrompt WAMode = "wolframAlpha > "
    commandToComplete WAMode = id
    completionFunction WAMode = \s -> if (length s == 0) then return [] else do
        fmap lines $ runProcessWithInput "wa" [s] "" 
    modeAction WAMode query tx = safeSpawn "vp" ["wa",escapeQuery query]

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
            jumpWindowSaved <- getJumpWindowSavedState
            let keyStr = keysymToString sym
                -- , ; . are excepted because they DO have meaning during a stack retrace
                validKeysForRetraceAllTime = (myModMask, "period") : [(mk, k) | mk <- [myModMask, myModMask .|. shiftMask, myModMask .|. controlMask, myModMask .|. controlMask .|. shiftMask, myModMask .|. mod1Mask, myModMask .|. mod1Mask .|. controlMask], k <- [historyBackKey, historyForwardKey]]
                validKeysForRetraceWhenJumpWindowSaved = [(myModMask, k) | k <- ["comma", "semicolon"]]
            {-case mode of-}
            if (m, keyStr) `elem` (validKeysForRetraceAllTime ++ if jumpWindowSaved then validKeysForRetraceWhenJumpWindowSaved else []) 
                   then return ()
                   else clearAllMarks
                 {-_       -> return ()-}
        return (All True)
handleKeyEventForXMonadMode _ = return (All True)

-- we need to put the toggle in the front interface so that the subsequent triggering of m-; and m-, does not break the save toggle
-- dir is the direction to navigate through the history (Prev means going back in history and Next means going forward)
jumpWindowHistory dir p saved = do
    saveJumpWindowSavedState saved 
    {-saveXMonadMode Retrace-}
    if saved then saveFindFunction (\dr -> jumpWindowHistory (if dr == dir then Next else Prev) p True) else return ()
    let nmat dirp = nextMatch History (dirp <&&> validWindow <&&> fmap not isMinimized <&&> p) 
    case dir of
         Prev -> withFocused markWindow >> nmat (fmap not windowIsMarkedQuery)
         Next -> nmat windowIsMarkedQuery >> withFocused unmarkWindow >> runLogHook

-- we need to have a toggle to indicate whether the last jump window query is saved; this is used when we clear the modes (if the last query is saved then m-; and m-, are excepted otherwise they clear the modes

data JumpWindowSavedState = JumpWindowSavedState Bool deriving (Typeable, Show, Read)
instance ExtensionClass JumpWindowSavedState where
    initialValue = JumpWindowSavedState False
    extensionType = PersistentExtension
 
getJumpWindowSavedState = XS.get >>= \(JumpWindowSavedState s) -> return s
saveJumpWindowSavedState = XS.put . JumpWindowSavedState

-- }}}

---------------- Quick find window states -- {{{

-- this module tries to simulate the f <key> behavior in vim
-- the module saves just one thing: a function that takes a dir argument and can be invoked later through m-, and m-;
data QuickFindFunction = QuickFindFunction (Direction1D -> X ()) deriving Typeable
instance ExtensionClass QuickFindFunction where
    initialValue = QuickFindFunction (\d -> return ())

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

-- the cycling protocol is useful for a set of windows matching a query; useful for task groups, etc.
cycleMatchingOrDo qry dir f d = do
    -- get all the windows before the focused and after the focused depending on the direction
    (l, r, gf, ml, float, ll, rl) <- getSides
    -- get the focused window
    mf <- gets (W.peek . windowset)
    let wwins = rl ++ ll
        (awinr, awinl, bwins, cwins) = case mf of
                        Just f 
                            | [f] == gf -> (r, l, float, ml)
                            | otherwise -> case break (==f) float of
                                (fl, _:fr) -> (fr, fl, l++gf++r, ml)
                                -- the focus is neither on the focal of the stack nor on float (something is wrong)
                                _ -> ([], [], float, ml)
                        _ -> ([], [], float, ml)
        winl = if dir == Next then awinr++awinl++bwins++cwins++wwins else reverse $ wwins++cwins++bwins++awinr++awinl
    matches <- filterM (runQuery qry) winl
    case matches of
         [] -> d
         ws -> f ws

getSides = do
    -- get all the windows before the focused and after the focused depending on the direction
    wss <- allWorkspaces
    winset <- gets windowset
    -- split the windows into two
    -- it's safer to get the windows for the current workspace from group stacks (it's more stable)
    gs <- G.getCurrentGStack
    ml <- fmap reverse getCurrentMinimizedWindows
    let (l, r) = maybe ([],[]) lrs gs
        gf = maybeToList $ maybe Nothing G.focal gs
        (b, a') = break ((==W.currentTag winset). W.tag) wss
        a = if not $ null a' then tail a' else []
        ll = nub $ concatMap (W.integrate' . W.stack) b
        rl = nub $ concatMap (W.integrate' . W.stack) a
        -- also retrieve the float windows (for comprehensiveness)
        currswins = W.integrate' $ W.stack $ W.workspace $ W.current winset
        float = filter (not . (`elem` (l++r++ml++gf))) currswins 
    return (l, r, gf, ml, float, ll, rl)
        where lrs (G.Leaf (Just (W.Stack f u d))) = (reverse u, d)
              lrs (G.Leaf Nothing) = ([], [])
              lrs (G.Node (W.Stack f u d)) = let (l, r) = lrs f in (concatMap G.flattened (reverse u) ++ l, r ++ concatMap G.flattened d)

    -- implementation that uses the group stacks instead
cycleMatchingOrDoSaved qry dir f d = saveFindFunction (\dr -> cycleMatchingOrDo qry (if dr == dir then Next else Prev) f d) >> cycleMatchingOrDo qry dir f d
cycleMatching qry dir = cycleMatchingOrDoSaved qry dir (deminimizeFocus . head) (return ())

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
{-doViewShift = doF . liftM2 (.) W.greedyView W.shift-}
                              
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
          , construct = runShell "vb"
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
    , def { taskGroupName = "pidgin"
          , filterKey = "d"
          , filterPredicate = className =? "Pidgin"
          , construct = mkNamedScratchpad scratchpads "pidgin"
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
          , construct = spawn "zathura"
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
          , filterKey = "g"
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

{-allStaticTaskGroups = taskGroups ++ fmap (\(TaskGroup k p l a lh ws) -> WindowGroup k (p <&&> notInAnyWindowGroup) l a lh ws) siftedWindowGroups-}
    {-where notInAnyTaskGroup = foldl (<&&>) alwaysTrue $ fmap (fmap not .filterPredicate) taskGroups-}

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

-- rewrite cycle between groups to do according to the status bar
-- the fun will receive a ([(b, w)], task group), b == whether that window is focused, w == the window
cycleTaskGroups dir = do
    cycleMatchingOrDoSaved toggleTaskGroupPredicate dir (\wins -> do
            -- get the task groups for all the windows
            tgs <- mapM (taskGroupOfWindow taskGroups) wins
            case groupBy (\(a,_) (b,_) -> a==b) $ zip tgs wins of
                 (h:_) -> nextMatch History $ isOneOfWindows $ snd $ unzip h
                 _ -> if null wins then return () else focus $ head wins
        ) (return ())

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
    (filter (not . null . filterKey) siftedTaskGroups)++[currentTaskGroup {filterKey = k} | k <- mdKeys++(fmap ("M-" ++) mdKeys)]


-- a simplified version of appFilterKeys with correct predicates for filtering in the current workspace; if the app's definition is not useful for local context then switch to global
localFirstFilterPredicate g = let f = filterPredicate g in if localFirst g then isInCurrentWorkspace <&&> f else f

-- apply the managehook to a window
runManageHook :: ManageHook -> Window -> X ()
runManageHook mh = windows . appEndo <=< runQuery mh

runManageHookOnFocused = withFocused . runManageHook

mountDupKeys dupKeys ls = 
    ls ++ fmap (\(k, ok) -> (k, maybe (return ()) snd $ find ((== ok) . fst) ls)) dupKeys

appendActionWithException a ex ls = ex ++ fmap (\(k, ka) -> (k, ka >> a ka)) ls

-- the normal typableChars
typeables = ['0'..'9']++['a'..'z']++['A'..'Z']++"!@#$%^&*()-_=+\\|`~[{]}'\",<.>?"
typeablesNoSlash = filter (/='/') typeables

charToKeyStroke c = if isUpper c then "S-"++[toLower c] else [c]

shiftWindowsHere wins = do
    -- deminimize all of them
    mapM deminimize wins
    curr <- gets (W.currentTag . windowset)
    -- move to the temp workspace and then move back
    -- move one by one because we can't be sure if the windows are the last one in the tab group (really annoying though)
    mapM (windows . W.shiftWin scratchpadWorkspaceTag) wins
    {-windows $ \s -> foldr (W.shiftWin scratchpadWorkspaceTag) s wins-}
    windows $ \s -> foldr (W.shiftWin curr) s wins
          
hasTagQuery s = ask >>= \w -> liftX $ hasTag s w

myKeys toggleFadeSet = 
    ----- dup key-- {{{
    mountDupKeys
    [
      ("M1-C-0", "M1-C-1")
    ] 
    -- }}}
    ----- Repeat-- {{{
    $
    appendActionWithException saveLastCommand
    [ 
      ("M-.", getLastCommand >>= id)
    ]
    -- }}}
    ----- wallpaper revoke -- {{{
    $
    appendActionWithException (\_ -> XS.put $ WallPaperToggleState False)
    [ ("M-S-x", wallpaperChangeFadeFullHook changeWallpaperCmd)
    , ("M-x", wallpaperToggleHook toggleFadeSet)
    , ("M-C-x d", wallpaperChangeFadeFullHook $ changeWallpaperCmd++" -D")
    , ("M-C-x f", wallpaperChangeFadeFullHook $ changeWallpaperCmd++" -F")
    , ("M-C-x e", wallpaperChangeFadeFullHook $ changeWallpaperCmd++" -E")
    ]
    -- }}}
    $
    [ 
    ----- XMonadMode manipulation-- {{{
    -- we need to call runLogHook to force an update to the log hook
      ("M-<Esc>", clearAllMarks >> runLogHook)
    -- }}}
    ----- Window manipulation-- {{{
    , ("M-<L>", withFocused $ keysMoveWindow (-10,0))
    , ("M-<R>", withFocused $ keysMoveWindow (10,0))
    , ("M-<U>", withFocused $ keysMoveWindow (0,-10))
    , ("M-<D>", withFocused $ keysMoveWindow (0,10))
    , ("M-S-<L>", withFocused $ keysAbsResizeWindow (-10,0) (0,0))
    , ("M-S-<R>", withFocused $ keysAbsResizeWindow (10,0) (0,0))
    , ("M-S-<U>", withFocused $ keysAbsResizeWindow (0,-10) (0,0))
    , ("M-S-<D>", withFocused $ keysAbsResizeWindow (0,10) (0,0))
    -- }}}
    ----- Window task group navigation-- {{{
    -- common definitions of some of the classes
    , ("M-C-]", cycleTaskGroups Next)
    , ("M-C-[", cycleTaskGroups Prev)
    , ("M-[ u", U.withUrgents $ flip whenJust deminimizeFocus . listToMaybe)
    , ("M-] u", U.withUrgents $ flip whenJust deminimizeFocus . listToMaybe)
    ]
    -- cycling task group by number
    ++ 
    [ ("M1-C-"++(show $ n+1), do
            cur <- getCurrentTaskGStack taskGroups
            case cur  of
                 Just (G.Node s@(W.Stack fg us ds))
                    | n == length us -> toggleTaskGroup
                    | n < length ls -> nextMatch History (isOneOfWindows $ G.flattened $ ls !! n)
                    | otherwise -> return ()
                        where ls = W.integrate s
                 _ -> return ()
       )
    | n <- filter (/= 5) [0..8]]
    ++
    [ ("M-"++dc++" "++(filterKey g), 
        let pre = localFirstFilterPredicate g in
        -- determine if we are in the correct group, if yes then cycle, otherwise go to the last visited window in that group
        cycleMatchingOrDoSaved pre dir (\wins -> do
            mf <- gets (W.peek . windowset)
            res <- case mf of
                       Just f -> runQuery pre f
                       _ -> return False
            if res then deminimizeFocus $ head wins
                   else withNextMatchOrDo History (isOneOfWindows wins) deminimizeFocus (return ())
        ) $ constructInDir (construct g) dir)
    | (dc, dir) <- [("[", Prev), ("]", Next)]
    , g <- allTaskGroupsWithFilterKey ["[","]"] ]
    -- forced creation of new windows
    ++
    [ ("M-S-"++dc++" "++(filterKey g), constructInDir (construct g) dir)
    | (dc, dir) <- [("[", Prev), ("]", Next)]
    , g <- allTaskGroupsWithFilterKey ["[","]","S-[","S-]"] ]
    -- }}}
    ----- History level navigation -- {{{
    ++
    [ ("M-"++m++dirk++ms, jumpWindowHistory dir p saved)
    | (dirk, dir) <- [(historyBackKey, Prev), (historyForwardKey, Next)]
    , (m, ms, p, saved) <- [ ("", "", alwaysTrue, False) ] 
                           ++
                           -- the app filtered history navigation is experimental -- it can changes the order of history in complex ways
                           [ ("S-"++sm, " "++(filterKey g), smp <&&> filterPredicate g, True) 
                           | g <- allTaskGroupsWithFilterKey ["o","i", "S-o", "S-i"]
                           , (sm, smp) <- [("", alwaysTrue), ("C-", toggleGroupPredicate)] ]
                           ++
                           -- inter group stack level
                           [ ("C-", "", toggleGroupPredicate, True)
                           -- within the group stack
                           , ("M1-", "", toggleWithinGroupPredicate, True)
                           -- within different task group
                           , ("C-M1-", "", toggleTaskGroupPredicate, True)]
    ]
    -- }}}
    ----- Window deletion mode -- {{{
    -- m-d goes into the mode where all selected windows are deleted; m-s-d will only delete windows that are unfocused
    ++
    [ ("M-"++m++"d "++(filterKey g), ifWindows (mp <&&> (localFirstFilterPredicate g)) (mapM_ focusNextKillWindow) (return ()))
    | (m, mp) <- [("", alwaysTrue), ("S-", isUnfocused)]
    , g <- allTaskGroupsWithFilterKey ["d", "S-d"] ]
    -- }}}
    ----- Info prompt system (now centralized as an intelligent system) -- {{{
    ++
    [ ("M-c "++[c], launcherPrompt myInfoXPConfig {defaultText = [c]} $ defaultModesForInput [c])
    | c <- "-+="++['0'..'9']++['a'..'z'] ]
    ++
    [("M-c <Return>", do
        l <- fmap (head . lines) $ runProcessWithInput "xsel" [] ""
        launcherPrompt myInfoXPConfig {defaultText = l} $ defaultModesForInput l)]
    -- }}}
    ----- Common Tasks-- {{{
    ++
    [
      ("M-M1-q", spawn myRestartCmd)
    -- we should also kill the processes and then exit
    , ("M-M1-S-q", spawn myKillCmd >> io (exitWith ExitSuccess))
    -- cycling of the window styles
    , ("M-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Next)
    , ("M-S-t", runManageHookOnFocused $ (windowStyle currentTaskGroup) Prev)
    {-, ("M-y", spawn "xvkbd -no-jump-pointer -xsendevent -text \"\\D1`xsel`\" 2>/dev/null")-}
    , ("<F10>", spawn "amixer get Master | fgrep '[on]' && amixer set Master mute || amixer set Master unmute")
    , ("<F11>", spawn $ "amixer set Master 2-; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    , ("<F12>", spawn $ "amixer set Master 2+; amixer set Master unmute; "++myScriptsDir++"/dzen_vol.sh")
    -- }}}
    ----- Prompts-- {{{
    -- bind change mode to some key that would never be used
    , ("M-r", dynamicPrompt myXPConfig { changeModeKey = xK_VoidSymbol, autoComplete = Nothing, searchPredicate = repeatedGrep })
    , ("M-b", vbPrompt)
    -- xmonad commands
    , ("M-C-,", xmonadPrompt myXPConfig {autoComplete = Nothing})
    -- tag windows
    , ("M-u", withFocused unTag)
    ]
    ++
    [ ("M-"++mf++"/", mkSearchPrompt myXPConfig { 
                                        searchPredicate = repeatedGrep
                                      , autoComplete = Nothing
                                    } p validWindow a)
    | (mf, p, a) <- [ ("", "Go to window: ", deminimizeFocus)
                    , ("S-", "Bring window: ", shiftWindowsHere . wrapList)]
    ]
    ++
    [ ("M-"++modk++" "++tk, gt a) 
    | (tk, gt) <- [([k], flip id [k]) | k <- typeablesNoSlash] 
                  ++
                  [("/", tagPrompt myXPConfig {searchPredicate = prefixSearchPredicate})]
    , (modk, a) <- [
          ("m", withFocused . addTag)
          -- automatically minimizing the current window on marking
        , ("S-m", \t -> withFocused $ \f -> do
                addTag t f 
                correctFocus $ minimizeWindow f)
        , ("'", \t -> cycleMatching (hasTagQuery t) Next)
        , ("S-'", \t -> cycleMatchingOrDoSaved (hasTagQuery t <&&> isInCurrentWorkspace) Next shiftWindowsHere (return ()))
        , ("C-S-'", \t -> cycleMatchingOrDoSaved (hasTagQuery t) Next shiftWindowsHere (return ()))] ]
    ++
    [ -- launcher
      ("M-z", mkTaskPrompt myXPConfig {
            autoComplete = Nothing
            , promptKeymap = M.fromList $ (M.toList emacsLikeXPKeymap)++[
                ((myModMask, xK_z), quit)
            ]})
    , ("M-y", mkFMCPrompt myXPConfig {
            autoComplete = Just 0
            , promptKeymap = M.fromList $ (M.toList emacsLikeXPKeymap)++[
                ((myModMask, xK_y), quit)
            ]})
    -- }}}
    ----- Layout hotkeys-- {{{
    -- Layoutgroups
    , ("M-<Space>", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage NextLayout)
    , ("M-S-<Space>", nextOuterLayout)
    {-, ("M-g M-0", sendMessage $ G.Modify G.focusMaster)-}
    {-, ("M1-S-4", sendMessage $ G.Modify G.focusLast)-}
    {-, ("M1-C-S-4", sendMessage $ G.Modify G.swapWithLast)-}
    ]
    -- within GroupStack manipulations
    ++ 
    [ (af++"M1-"++mf++[last $ show $ n+1], a)
    {-| n <- filter (/=5) [0..8]-}
    | n <- [0..9]
    , (af, mf, a) <- [
          ("", "", do
              -- let's check if the current window is already focused for the given position
              gs <- G.getCurrentGStack
              case fmap G.baseCurrent gs of
                   Just (G.Leaf s@(Just (W.Stack f u d)))
                      | n == length u -> nextMatch History (isOneOfWindows $ W.integrate' s)
                      | otherwise -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.focusAt n
                   _ -> return ()
          ) 
        , ("", "S-", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapWith n)
        , ("M-d ", "", do
              gs <- G.getCurrentGStack  
              case fmap (G.flattened . G.baseCurrent) gs of
                   Just ls
                      | n < length ls -> focusNextKillWindow (ls !! n)
                      | otherwise -> return ()
                   _ -> return ()
          )
    ]]
    -- inter GroupStack manipulations
    ++ 
    [ (af++"C-"++mf++[last $ show $ n+1], a)
    | n <- filter (/=5) [0..9]
    , (af, mf, a) <- [
          ("", "", do
                gs <- G.getCurrentGStack
                case gs of
                     Just (G.Node (W.Stack f u d))
                        | n == length u -> toggleGroup
                        | otherwise -> sendMessage $ G.Modify $ G.focusGroupAt n
                     _ -> return ()
          ) 
        , ("", "S-", sendMessage $ G.Modify $ G.moveToGroupAt n)
        , ("M-d ", "", do
            gs <- G.getCurrentGStack
            case fmap (maybe [] G.flattened . G.groupAt n) gs of
                 Just ls -> mapM_ focusNextKillWindow ls
                 _ -> return ()
          )
        ]]
    ++ 
    {-[ ("M-l", sendMessage $ G.Modify G.focusDown)-}
    {-, ("M-h", sendMessage $ G.Modify G.focusUp)-}
    [ ("M-k", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.focusGroupUp)
    , ("M-j", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.focusGroupDown)
    , ("M-h", sendMessage $ G.Modify G.focusGroupUp)
    , ("M-l", sendMessage $ G.Modify G.focusGroupDown)

    , ("M-p", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.focusUp)
    , ("M-n", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.focusDown)
    -- simplifying the process by not providing keys for shifting a tab page to adjacent columns
    -- since by user convention it doesn't seem like a thing that I tend to use
    , ("M-S-p", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapUp)
    , ("M-S-n", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapDown)

    , ("M-C-S-k", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupUp)
    , ("M-C-S-j", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupDown)
    , ("M-C-S-h", sendMessage $ G.Modify G.swapGroupUp)
    , ("M-C-S-l", sendMessage $ G.Modify G.swapGroupDown)
    , ("M-C-S-s", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.splitGroup)

    , ("M-S-h", sendMessage $ G.Modify $ G.moveToGroupUp True)
    , ("M-S-l", sendMessage $ G.Modify $ G.moveToGroupDown True)
    {-, ("M-S-h", G.moveSubGroupToGroupUp)-}
    {-, ("M-S-l", G.moveSubGroupToGroupDown)-}
    , ("M-S-k", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveToGroupUp True)
    , ("M-S-j", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.moveToGroupDown True)
    , ("M-C-h", sendMessage $ G.Modify G.moveToNewGroupUp)
    , ("M-C-l", sendMessage $ G.Modify G.moveToNewGroupDown)
    , ("M-C-k", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.moveToNewGroupUp)
    , ("M-C-j", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.moveToNewGroupDown)
    , ("M-q", withFocused focusNextKillWindow)
    , ("M-S-q", G.getCurrentGStack >>= maybe (return ()) (mapM_ (focusNextKillWindow) . G.flattened . G.baseCurrent))
    , ("M-C-q", G.getCurrentGStack >>= maybe (return ()) (mapM_ (focusNextKillWindow) . maybe [] G.flattened . G.current))
    , ("M-s", sortGroupStacks taskGroups >> runLogHook)
    -- a permanent sticky layout that will automatically move any new window into the corresponding task group
    , ("M-S-s", do
        h <- getCurrentWorkspaceHandle 
        toggleAutoArrangeWorkspace h
        runLogHook)
    -- toggle insert older
    , ("M-C-s", toggleInsertOlder >> runLogHook)
    -- force insert older, while remembering the old toggle
    , ("M-M1-s", toggleInsertOlderForce >> runLogHook)
    -- recover for the old toggle
    , ("M-M1-S-s", toggleInsertOlderRecover >> runLogHook)
    , ("M-C-c", sendMessage $ G.Modify G.collapse)

    , ("M-S-,", sendMessage $ G.ToEnclosing $ SomeMessage zoomOut)
    , ("M-S-.", sendMessage $ G.ToEnclosing $ SomeMessage zoomIn)
    , ("M-\\", sendMessage $ G.ToEnclosing $ SomeMessage zoomReset)
    , ("M--", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomOut)
    , ("M-S-=", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomIn)
    , ("M-=", sendMessage $ G.ToFocused $ SomeMessage $ G.ToEnclosing $ SomeMessage zoomReset)
    {-, ("M-S-m", sendMessage $ G.Modify G.focusGroupMaster)-}
    , ("M-S-<Return>", sendMessage $ G.Modify G.swapGroupMaster)
    , ("M-<Return>", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify G.swapGroupMaster)
    {-, ("M-C-,", sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN 1)-}
    {-, ("M-C-.", sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN (-1))-}
    , ("M-C-m", correctFocus $ withFocused minimizeWindow)
    , ("M-C-S-m", sendMessage RestoreNextMinimizedWin)
    , ("M-<Tab>", switchFocusFloat)
    , ("M-S-<Tab>", switchFocusFloat)
    -- }}}
    ----- Workspace hotkeys-- {{{
    -- for Dynamic Workspaces
    {-, ("M-<Backspace>", clearWorkspacesAfter >> return ())-}
    , ("M-C-S-q", removeCurrentWorkspace)
    , ("M-S-c", renameWorkspacePrompt myXPConfig {searchPredicate = repeatedGrep, autoComplete = Nothing})
    ]
    ++
    [ ("M-"++mf++"a "++af, newWorkspacePrompt myXPConfig {searchPredicate = repeatedGrep, autoComplete = Nothing} (prompt++" ("++(show pos)++")") pos action)
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
    [ 
    -- for cycleing through the set
      ("M-6", lastWorkspaceTag >>= windows . W.view )
    , ("M-S-6", lastWorkspaceTag >>= windows . W.shift)
    , ("M-C-6", lastWorkspaceTag >>= swapWith)
    , ("M-4", allWorkspaceTags >>= toggleTag . last >>= windows . W.view)
    , ("M-S-4", allWorkspaceTags >>= toggleTag . last >>= windows . W.shift)
    , ("M-C-4", allWorkspaceTags >>= toggleTag . last >>= swapWith)
    {-, ("M1-6", toggleWithinGroup)-}
    , ("C-M1-6", toggleTaskGroup)
    , ("M-C-n", doTo Next validWSType myWorkspaceSort (windows . W.greedyView))
    , ("M-C-p", doTo Prev validWSType myWorkspaceSort (windows . W.greedyView))
    , ("M-C-S-n", doTo Next validWSType myWorkspaceSort (windows . W.shift))
    , ("M-C-S-p", doTo Prev validWSType myWorkspaceSort (windows . W.shift))
    {-, ("M-M1-C-p", modSwapTo Prev)-}
    {-, ("M-M1-C-n", modSwapTo Next)-}
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
-- }}}
----- QuickFindWindow-- {{{
    -- zip through all the possible combinations of keys
    ++
    [ ("M-"++m++"f "++k, cycleInCurrWSOfPropMatchingPrefix Next p c casesens)
    | (m, p, casesens) <- zip3 ["", "S-"] [title, className] [True, False]
    , (k, c) <- fmap (\c -> ([c], c)) ['a'..'z'] 
                ++
                fmap (\c -> ("S-"++[c], toUpper c)) ['a'..'z']]
    ++
    [ ("M-;", playLastFindFunction Next)
    , ("M-,", playLastFindFunction Prev)]
-- }}}
        where cycleInCurrWSOfPropMatchingPrefix dir p c casesens =  
                  let trans = if casesens then id else toLower in
                  cycleMatching ((fmap ((==) (trans c) . trans . head) p) <&&> isInCurrentWorkspace) dir
              
-- }}}

---------------- Main -- {{{
main = do
    toggleFadeSet <- newIORef S.empty
    dzenLogBar <- myStatusBars
    -- urgencyhook is not used currently due to conflict with the wallpaper system
    xmonad $ ewmh $ U.withUrgencyHook U.NoUrgencyHook $ defaultConfig { 
    {-xmonad $ ewmh $ defaultConfig { -}
        manageHook = myManageHook 
        , terminal = myTerminal
        , workspaces = [scratchpadWorkspaceTag, tmpWorkspaceTag]
        , startupHook = myStartupHook
        , modMask = myModMask
        , layoutHook = myLayout
        , logHook = myLogHook toggleFadeSet dzenLogBar 
        , handleEventHook = myHandleEventHook toggleFadeSet
        , focusFollowsMouse = False
        , borderWidth = 2
        , normalBorderColor = myBgColor
        , focusedBorderColor = color6
        } `additionalKeysP` myKeys toggleFadeSet

-- }}}
