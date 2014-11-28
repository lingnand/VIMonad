{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Vim
-- Copyright   :  (C) 2014 Lingnan Dai
-- License     :  BSD3
--
-- A module for simulating a vim environment for XMonad 
--
-----------------------------------------------------------------------------

module XMonad.Vim
    (
      viminize
    , vimXPKeymap
    , VimLayout
    , VimStatusTheme(..)
    ) where

import Prelude hiding (mapM)
import System.IO
import System.Directory
import Data.Char
import Data.Maybe
import Data.Monoid (mempty, All(..), appEndo)
import Data.List hiding (delete)
import Data.List.Split
import Data.Traversable hiding (sequence)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad hiding (mapM)
import Data.IORef
import XMonad
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Layout.ZoomRow
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.TiledTabs
import XMonad.Layout.Simplest
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Groups as G
import XMonad.Actions.TagWindows
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D (switchLayer)
import XMonad.Actions.FloatKeys
import XMonad.Actions.PhysicalScreens
import XMonad.Util.Run
import XMonad.Util.Image
import XMonad.Util.Font
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Timer
import XMonad.Util.Types
import XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.OnWindowsInserted
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FloatNext (runLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicBars
import qualified XMonad.Hooks.UrgencyHook as U
import XMonad.Vim.InsertPosition
import XMonad.Vim.InsertOlder
import XMonad.Vim.Minimize
import XMonad.Vim.Prompt.Calc
import XMonad.Vim.Prompt.Vimb
import XMonad.Vim.Prompt.Dict
import XMonad.Vim.Prompt.Workspace
import XMonad.Vim.Prompt.Wallpaper
import XMonad.Vim.Prompt.DynamicPrompt
import XMonad.Vim.Prompt.WindowSearch
import XMonad.Vim.WorkspaceHandles
import XMonad.Vim.WorkspaceDirectories
import XMonad.Vim.Workspaces
import XMonad.Vim.WindowStyle
import XMonad.Vim.Routine
import XMonad.Vim.Constants
import XMonad.Vim.Term
import XMonad.Vim.Tag
import XMonad.Vim.TaskGroup
import XMonad.Vim.QuickFind
import XMonad.Vim.Repeat
import XMonad.Vim.Macro
import XMonad.Vim.CIM

vimXPKeymap = emacsLikeXPKeymap' (\c -> not (isAlphaNum c) && c /= '_')
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

-- toggleTaskGroup = nextMatch History toggleTaskGroupPredicate
-- toggleTaskGroupPredicate t = fmap not (filterPredicate $ contextualGroup t) <&&> isInCurrentWorkspace <&&> fmap not isMinimized
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

-- return all window groups including the currentTaskGroup one; note that window groups without filter keys are excepted
allTaskGroupsWithFilterKey t mdKeys = 
    (filter (not . null . filterKey) $ siftTaskGroups t)++if null mdKeys then [] else [(contextualGroup t) {filterKey = k} | k <- mdKeys]

-- a simplified version of appFilterKeys with correct predicates for filtering in the current workspace; if the app's definition is not useful for local context then switch to global
localFirstFilterPredicate g = let f = filterPredicate g in if localFirst g then isInCurrentWorkspace <&&> f else f


----------------- Layout

data GroupEQ a = GroupEQ
  deriving (Show, Read)

instance Eq a => EQF GroupEQ (G.Group l a) where
    eq _ (G.G l1 _) (G.G l2 _) = G.sameID l1 l2

vimLayout myTabsTheme tgs = G.group3 (renamed [CutWordsRight 1] $ addTabs shrinkText vtc Simplest) (Mirror (zoomRowWith GroupEQ) ||| Full) (zoomRowWith GroupEQ ||| Full)
    where vtc = myTabsTheme {
                     subThemeForWindow = \w -> do
                        -- get the group stack
                        mgs <- G.getCurrentGStack
                        tg <- fmap (maybe def colorScheme) $ taskGroupOfWindow tgs w 
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

-- tabbar related settings
fetchTabNo' w (h:t)
    | isJust r = r
    | otherwise = fetchTabNo' w t
        where r = fetchTabNo w h
fetchTabNo' w [] = Nothing

fetchTabNo w (G.Node s) = fetchTabNo' w $ W.integrate s
fetchTabNo w (G.Leaf ms) = elemIndex w $ W.integrate' ms

-- the focused stack is the base current stack, unless the current focus is on a floating window, in that case it is the stack formed by that floating group
-- the selection stack should be 
-- 1. the actively selected elements, if any  
-- 2. the passively selected elements in the basecurrent group, if any
-- 3. the current window
-- note that when the focus is on a float window it would always be case 3

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


------------------- Log -----

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

data VimStatusTheme = VimStatusTheme { backgroundColor :: String
                                     , foregroundColor :: String
                                     , textHighLight :: String
                                     , notifyColor :: String
                                     , foregroundHighLight :: String
                                     -- used for hidden workspaces without windows
                                     , foregroundDimLight :: String
                                     }

instance Default VimStatusTheme where
    def = VimStatusTheme { backgroundColor = "#181512"
                         , foregroundColor = "#888888"
                         , textHighLight = "#fafac0"
                         , notifyColor = "#f39d21"
                         , foregroundHighLight = "#ffffff"
                         , foregroundDimLight = "#505050"
                         }

vimStatusLogHook colors@(VimStatusTheme myBgColor myFgColor myTextHLight myNotifyColor myFgHLight myFgDimLight) tgs =  do
    myBitmapsDir <- io getMyBitmapsDir
    pp <- workspaceNamesPP $ def {
          ppCurrent     = dzenColor myTextHLight myBgColor
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
                                _                     ->      ""
                          in joinStr "" . fmap (iconify . trim) . reverse . splitOn "by"
        , ppTitle       = wrap ("^fg(#222222)^i("++myBitmapsDir++"/corner_left.xbm)^bg(#222222)^fg(#4c7899)^fn(fkp)x^fn()") ("^fg(#222222)^i("++myBitmapsDir++"/corner_right.xbm)") .  dzenColor myFgColor "#222222" . shorten 120 . pad        
        , ppOrder   =  \(ws:l:t:hist:gp:hid:xs) -> [hist,ws,l,hid,gp] ++ xs ++ [t]
        , ppSort        = fmap (. filter ((/=scratchpadWorkspaceTag) . W.tag)) getSymbolStringSort
        , ppExtras      = [  printLayoutInfo colors
                           , printGStack colors tgs
                           , printRegs colors tgs
                           ]
        } 
    -- dynamicLogWithPP pp
    -- for the time being we will just use the same PP for any screen
    multiPP pp pp

printLayoutInfo (VimStatusTheme myBgColor myFgColor myTextHLight myNotifyColor _ _) = do
    s <- getMarkedWindowsSize
    InsertOlderToggle t <- XS.get
    XMS mode <- XS.get
    MMS m <- XS.get
    let (plus, c) = if s == 0 then (" ", myFgColor) else ("+", myNotifyColor)
        [lb,rb] = fmap (dzenColor myFgColor myBgColor) $ ["[","]"]
        insertOlderIndicator = if t then dzenColor myNotifyColor myBgColor "|" else "<"
        modeIndicator = case mode of
                             Visual Win -> dzenColor myNotifyColor myBgColor "VW" 
                             Visual Row -> dzenColor myNotifyColor myBgColor "VR" 
                             Visual Col -> dzenColor myNotifyColor myBgColor "VC" 
                             _ -> "Nm"
        recordingIndicator = case m of
                                  Just (name, _) -> dzenColor myNotifyColor myBgColor $ "● " ++ name
                                  _ -> "■  "
    return $ Just $ recordingIndicator ++ " | " ++ modeIndicator++ " " ++ lb ++ insertOlderIndicator ++ "-" ++dzenColor c myBgColor plus ++ rb

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

-- pretty printing for history stack status
-- a tentative printing function for a given GStack
-- we'll first convert the gstack to a stack of strings according to the task group within each leaf, and then we can easily print it using another function
-- return the stack tagged with task groups
taggedGStack:: [TaskGroup] -> G.MultiStack Window -> X (G.MultiStack (Window, Maybe TaskGroup))
taggedGStack wgs = mapM (\w -> siftedTaskGroupOfWindow wgs w >>= \r -> return (w, r))

-- return a (MultiStack String) which contains all the human readable information regarding the task groups of the windows
infoFromTaggedGStack :: VimStatusTheme -> G.MultiStack (Window, Maybe TaskGroup) -> String
infoFromTaggedGStack (VimStatusTheme myBgColor myFgColor myTextHLight myNotifyColor _ _) ms@(G.Node (W.Stack f u d)) = 
    -- for the first level the wrap pairs are ("1.", "") ("2.", "") and so on
    let wl = zip (fmap ((++".") . wrapList) (fullSequence columngroupSymbolSequence) ++ repeat "") $ repeat ""
        nl = G.level ms
    -- need to get the number of levels inside the struct
    in infoFromTaggedGStack' False ("  ":repeat "") ("","") (wl:(repeat $ repeat ("[","]"))) (myNotifyColor, "/") (take (nl-1) (repeat (myFgColor, myFgColor)) ++ [(myTextHLight, myFgColor)]) myBgColor ms
infoFromTaggedGStack _ _ = ""

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

printGStack cs tgs = do
       mgs <- G.getCurrentGStack
       {-spawn $ "echo '"++(show mgs)++"' > ~/.xmonad/xmonad.test"-}
       case mgs of
            Just gs -> taggedGStack tgs gs >>= return . Just . infoFromTaggedGStack cs
            _ -> return $ Just "" 

printRegs (VimStatusTheme myBgColor myFgColor myTextHLight myNotifyColor _ _) tgs = do
    mf <- gets (W.peek . windowset)
    als <- allOrderedWindows
    altags <- mapM getTags als
    mwins <- getMinimizedWindows
    let ls = filter (not . null . snd) $ zip als altags
        (wins, _) = unzip ls
        regs = sortRegs $ nub $ concat $ fmap snd ls
        unrefed = filter (not . (`elem` wins)) mwins
    rwins <- mapM (\r -> filterM (hasTag r) wins) regs
    -- now for each tag we scan through the list to get all the windows
    let pairs = zip regs rwins ++ (if not (null unrefed) then [("*", unrefed)] else [])
        (rls, winls) = unzip pairs
        pinfo' c = infoFromTaggedGStack' True (repeat "") ("","") (repeat $ repeat ("","")) (c, "/") (repeat (myFgColor, myFgColor)) myBgColor
        toTg' l = taggedGStack tgs (G.fromZipper (W.differentiate $ reverse l) 1) 
        toTg (vls, hls) = do
            vtg <- toTg' vls
            htg <- toTg' hls
            let vc = case mf of
                        Just f | f `elem` vls -> myTextHLight
                        _ -> myFgColor
            return (pinfo' vc vtg, pinfo' myNotifyColor htg)
        prinfo (r, (vinfo, hinfo)) = "'"++r++":"++jvh vinfo hinfo
        jvh [] b = b
        jvh a [] = a
        jvh a b = a ++ "-" ++ b
    tgs <- mapM toTg $ fmap (partition (not . (`elem` mwins))) winls
    return $ Just $ joinStr "|" $ fmap prinfo $ zip rls tgs 

----------- hooks

vimManageHook t = composeAll [ 
      onWindowsInsertedManageHook
    , insertPositionHook
    , launchHook $ contextualGroup t
    , manageDocks
    ]


---------------- Start workspace transition timer -- {{{
-- due to some weird reasons the dzen bar refuses to be detected by the avoidStruts at startup. Resolution: switch to the right workspace after some delay (assuming that dzen has started up by that time

-- data StartWSChangeState = StartWSChangeState TimerId deriving Typeable
-- instance ExtensionClass StartWSChangeState where
--     initialValue = StartWSChangeState 0

-- startWSSwitchHook e = do
--     (StartWSChangeState t) <- XS.get                 
--     handleTimer t e $ do              
--         windows $ W.view tmpWorkspaceTag
--         return Nothing                  
--     return $ All True

vimHandleEventHook m = do
    -- startWSSwitchHook
    handleKeyEventForXMonadMode m

handleKeyEventForXMonadMode myModMask (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
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
handleKeyEventForXMonadMode _ _ = return (All True)

vimLogHook toggleFadeSet = do
    historyHook
    wallpaperLogHook toggleFadeSet
    onWindowsInsertedLogHook

------------ keyboard shortcuts
-- implementing the motion keys list
-- we don't allow 0 because that is kept for use as d0
numberKeyToMotion a = if null a then a else "g "++a
numberMotionKeys = fmap (\(a,b) -> (numberKeyToMotion a, b)) numberKeys
tabKeys = zip (fmap wrapList subgroupSymbolSequence) [0..]
extendedTabKeys = zip (fmap charToKeyStroke $ extendedSequence subgroupSymbolSequence) [length tabKeys..]
columnKeys = zip (fmap wrapList columngroupSymbolSequence) [0..]
extendedColumnKeys = zip (fmap charToKeyStroke $ extendedSequence columngroupSymbolSequence) [length columnKeys..]
feedReg k fun = fmap Just $ fun k
wrapInclude = fmap (\a -> (a, True))
getMinimizedWindowsWithoutRegs = getMinimizedWindows >>= filterM (fmap null . getTags)

hasTagQuery s = ask >>= \w -> liftX $ hasTag s w
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
                    , \_ d -> fmap (wrapInclude . listToMaybe . if d == Prev then reverse else id) getMinimizedWindowsWithoutRegs
                    , \_ -> getMinimizedWindowsWithoutRegs)]
regUnnamedKey = (""
                , feedReg ""
                , False
                , \_ _ -> return Nothing
                , \_ -> return [])

data RegPrompt = RegPrompt String
instance XPrompt RegPrompt where
    showXPrompt (RegPrompt prompt) = prompt

regPrompt config prompt action = do
    allRegs <- allOrderedWindows >>= mapM getTags
    let regs = sortRegs $ nub $ concat allRegs
        compl s = return $ filter (searchPredicate config s) regs
    mkXPromptWithReturn (RegPrompt prompt) config compl action

regTagPrompt xpc prompt a = initMatches >>= \r -> regPrompt (xpc r) {
    searchPredicate = prefixSearchPredicate
} prompt a
regPromptKeys xpc p = regPromptKeys' xpc p p
regPromptKeys' xpc promptForCut promptForAdd = 
    [ ( charToKeyStroke c
      , regTagPrompt xpc pr
      , add
      , targetForReg
      , \t -> orderedWindowsMatchingPredicate (hasTagQuery t))
    | (c, add, pr) <- [('/', False, promptForCut), ('?', True, promptForAdd)] ]

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
motionKeys :: (HistoryMatches -> XPConfig) -> [TaskGroup] -> [(String, String, X Motion)]
motionKeys xpc tgs = 
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
            let move dir = wrapAroundWindowsMatchingPredicate (if dir == d then Next else Prev) (fmap not isFocused <&&> localFirstFilterPredicate g <&&> fmap not isMinimized) 
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
    | g <- allTaskGroupsWithFilterKey tgs ["c"]
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
    | (tk, ta, _, xt, xls) <- regKeys ++ regPromptKeys xpc "Select windows from register: " ++ regReadonlyKeys
    ]

cmdMotionKeys xpc t = fmap (\(_,a,b) -> (a,b)) . nubBy (\(_,a,_) (_,b,_) -> a == b) $ motionKeys xpc t
argMotionKeys xpc t = fmap (\(a,_,b) -> (a,b)) . nubBy (\(a,_,_) (b,_,_) -> a == b) $ motionKeys xpc t

motionKeyCommands' xpc t = flip fmap (cmdMotionKeys xpc t) $ \(k, x) -> (k, do
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

motionKeyCommands a b = concatMap processKey $ motionKeyCommands' a b

-- return the normal selection for most of the commands
windowSelectionForMotionKey key xpc tgs = maybe (return []) snd $ find ((==key) . fst) $ motionKeyWindowSelection xpc tgs
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
motionKeyWindowSelection xpc t = flip fmap (argMotionKeys xpc t) $ \(k, x) -> (k, x >>= windowSelectionFromMotion)

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

cutCommands xpc tgs = concatMap (processKey . addPrefix) $
    [ (regk++dc, da)
    | (regk, ta, add, _, _) <- [regUnnamedKey]++ quoteRefed (regKeys ++ regPromptKeys' xpc "Cut windows to register: " "Add windows to register: ")
    , (pk, cutcmd) <- [("d", delete), ("m", cut)]
    , cmd <- [\ls -> ta $ \t -> cutcmd add t ls]
    , (dc, da) <- -- deletion
                  [ (fk, xls >>= cmd >> refresh)
                  | (fk, xls) <- [ (pk++" "++mk, xls')
                                 | (mk, xls') <- motionKeyWindowSelection xpc tgs
                                                 ++
                                                 structKeys pk]
                                 ++
                                 [ ("S-"++pk, windowSelectionForMotionKey "S-4" xpc tgs) ]
                  ]
                  ++
                  [ (pk++" "++nk++"s", do
                        wst <- workspaceStack
                        let ls = case wst of
                                    Just (W.Stack f _ d) -> [f]++d
                                    _ -> []
                        removeWorkspaces cmd $ fmap W.tag $ take n ls)
                  | (nk, n) <- numberKeys
                  ]
                  ++
                  [ (pk++" M-"++t, removeWorkspaces cmd [t])
                  | t <- quickWorkspaceTags]
                  -- the find motion for workspaces
                  ++
                  [ (pk++" "++mk++"M-"++ch, do
                      ts <- allWorkspaceTags
                      curr <- gets (W.currentTag . windowset)
                      -- first we should try to locate the indices
                      case (elemIndex curr ts, ati ts) of
                           (Just ci, Just ti) | ci < ti -> removeWorkspaces cmd $ drop ci $ take (ti+1) ts
                                              | ci == ti -> if useSingleCurrent then removeWorkspaces cmd $ [ts !! ci] else return ()
                                              | otherwise -> removeWorkspaces cmd $ drop ti $ take ci ts
                           _ -> return ())
                  | (mk, ch, ati, useSingleCurrent) <- 
                                     [ ("f ", charToKeyStroke t, elemIndex [t], False) | t <- symbolSequence]
                                     ++
                                     [ ("", "0", \_ -> Just 0, False), ("", "S-4", \ts -> Just $ length ts - 1, True) ]
                  ]
                  -- the [ and ] motion
                  ++
                  [ (pk++" "++nk++dk, do
                          wst <- workspaceStack
                          let ls = case wst of
                                       Just (W.Stack f u d) | dir == Prev -> if null u then [] else [f]++u
                                                            | otherwise -> if null d then [] else [f]++d
                                       _ -> []
                          removeWorkspaces cmd $ fmap W.tag $ take (n+1) ls)
                  | (nk, n) <- numberKeys
                  , (dk, dir) <- [("[", Prev), ("]", Next)]
                  ]
                  ++
                  [ ((if pk == "d" then "" else "S-")++"x", onSelectedWindows cmd >> refresh) ]
    ]

yankCommands xpc tgs = concatMap (processKey . addPrefix) $
    [ (kstr, da)
    | (regk, ta, add, _, _) <- regKeys ++ regPromptKeys' xpc "Yank windows to register:" "Add windows to register: "
    , (kstr, da) <- [ ("' "++regk++" y "++mk, do
                            ls <- xls
                            ta $ \t -> yank add t ls
                            return ())
                    | (mk, xls) <- motionKeyWindowSelection xpc tgs
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
    | (mk, xls) <- motionKeyWindowSelection xpc tgs
                   ++
                   structKeys "u"
                   ++
                   wssKeys]
    ++
    [ ("S-u", do
           gs <- getSelectedWindowStack
           mapM_ unTag (W.integrate' gs))
    ]

constructionKeys m xpc sc tgs cimdb = 
    [ (filterKey g, \n ls lf -> do
           onWindowsInserted n (\_ _ -> return ()) ls (\_ _ -> lf)
           (construct g) n Nothing)
    | g <- allTaskGroupsWithFilterKey tgs ["c"]]
    ++
    [ ("/", \n l lf -> do
            let base x = mkDynamicPrompt' m xpc sc cimdb (return ()) (return ()) $ OnWindowsInserted 1 (\_ _ -> return ()) (\_ _ -> return ()) (\mf ls -> do
                if not (null ls) then l mf $ head ls
                                 else return ()
                x)
            (!! n ) $ (iterate base lf)
      ) ]

-- the change commands allow an extra X () that will be run AFTER the windows has been created
changeCommands modKey xpc sc tgs cimdb = concatMap (processKey . addPrefix) $
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
    | (ck, construct) <- constructionKeys modKey xpc sc tgs cimdb
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
                        | (k, xmt) <- argMotionKeys xpc tgs
                        , (mk', k') <- [ ("", k++" ") ] ++ if k == "S-4" then [ ("S-", "") ] else []
                        ]
                        ++
                        [ ("", mt'++" ", fmap ((,) Nothing) xls') 
                        | (mt', xls') <- structKeys "c"]
    , (regk, ta, add, _, _) <- [regUnnamedKey] ++ quoteRefed (regKeys ++ regPromptKeys' xpc "Cut windows to register: " "Add windows to register: ")
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
                          , ("g ", toggleInsertOlder', putInsertOlder)]
    , (ipk, inp) <- insertPositionKeys
    , (ck, construct) <- constructionKeys modKey xpc sc tgs cimdb
    , (regk, ta, add, _, _) <- [regUnnamedKey] ++ quoteRefed (regKeys ++ regPromptKeys' xpc "Cut windows to register: " "Add windows to register: ")
    ]

pasteCommands xpc = concatMap (processKey . addPrefix)
    [ (regk++cmd, ta (paste i xls) >> return ())
    | (cmd, i) <- [("p", False)
                  , ("g p", True)
                  ]
    , (regk, ta, _, _, xls) <- [regUnnamedKey]++ quoteRefed (regKeys ++ regPromptKeys xpc "Paste windows from register: " ++ regReadonlyKeys)
    ]
visualCommands = 
    [ ("M-v", enterVisualMode Win)
    , ("M-S-v", enterVisualMode Row)
    , ("M-C-v", enterVisualMode Col)
    ]

historyCommands tgs =
    [ ("M-"++m++dirk++ms, jumpWindowHistory dir p)
    | (dirk, dir) <- [(historyBackKey, Prev), (historyForwardKey, Next)]
    , (m, ms, p) <- [ ("", "", alwaysTrue) ] 
                    ++
                    -- the task group filtered history navigation is experimental -- it can changes the order of history in complex ways
                    [ ("S-"++sm, " "++(filterKey g), smp <&&> filterPredicate g) 
                    | g <- allTaskGroupsWithFilterKey tgs ["M-S-o", "M-S-i"]
                    , (sm, smp) <- [("", alwaysTrue)] ]
                    ++
                    -- inter group stack level
                    [ ("C-", "", toggleGroupPredicate)
                    -- within the group stack
                    , ("M1-", "", toggleWithinGroupPredicate)]
    ]

getSourceIndexInCurrentBase wins = do
    mgs <- fmap (fmap (G.toZipper . G.baseCurrent)) G.getCurrentGStack
    case mgs of
         Just s -> return $ findIndex (`elem` wins) $ W.integrate' s
         _ -> return $ Nothing

insertAt wins sourceIndex destIndex = 
    sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.insertAt wins $ if sourceIndex < destIndex then destIndex + 1 else destIndex

layoutCommands tgs = 
    [ ("C-S-"++k, applySelectedWindowStack True $ \s -> sendMessage $ G.Modify $ G.moveWindowsToGroupAt n s)
    | (k, n) <- columnKeys]
    ++
    [ ("M1-S-"++k, fmap W.integrate' getSelectedWindowStackOnlyInFocusStack >>= \l -> getSourceIndexInCurrentBase l >>= maybe (return ()) (\si -> insertAt l si n))
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
    [ (nk++ck++"S-"++fk, action)
    | (nk, n) <- numberMotionKeys
    , (fk, ck, action) <- [ ("b", "", fmap W.integrate' getSelectedWindowStackOnlyInFocusStack >>= \l -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapWindowsUpN n l)
                          , ("w", "", fmap W.integrate' getSelectedWindowStackOnlyInFocusStack >>= \l -> sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapWindowsDownN n l)
                          , ("k", "C-", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapGroupUpN n)
                          , ("j", "C-", sendMessage $ G.ToFocused $ SomeMessage $ G.Modify $ G.swapGroupDownN n)
                          , ("h", "C-", sendMessage $ G.Modify $ G.swapGroupUpN n)
                          , ("l", "C-", sendMessage $ G.Modify $ G.swapGroupDownN n)
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
    , ("M-<Tab>", switchLayer)
    -- cycling of the window styles
    , ("M-t", runManageHookOnFocused $ (windowStyle $ contextualGroup tgs) Next)
    , ("M-S-t", runManageHookOnFocused $ (windowStyle $ contextualGroup tgs) Prev)
    ]

mkDynamicPrompt' myModMask xpc statusCleanupCmd cimdb immediate final owi = initMatches >>= \r -> dynamicPrompt (xpc r) { 
              changeModeKey = xK_VoidSymbol
            , searchPredicate = repeatedGrep
            , promptKeymap = M.fromList $ (M.toList $ vimXPKeymap r)++[
                  ((myModMask, xK_b), addOrTruncateTillPrefix "vb ")
                , ((myModMask, xK_c), addOrTruncateTillPrefix "calc ")
                , ((myModMask, xK_k), addOrTruncateTillPrefix "tk ")
                -- shift + d
                , ((myModMask .|. shiftMask, xK_D), cycleDictionaryForDPrompt Prev)
                , ((myModMask, xK_d), cycleDictionaryForDPrompt Next)
                -- fmd related
                , ((myModMask, xK_r), addOrTruncateTillPrefix "rpc ")
                , ((myModMask, xK_n), setInputAndDone "rpc next")
                , ((myModMask, xK_a), setInputAndDone "[ \"`rpc info '%r'`\" = 1 ] && rpc unrate || rpc rate")
                , ((myModMask, xK_u), setInputAndDone "rpc ban")
                , ((myModMask, xK_t), setInputAndDone "rpc toggle")
                , ((myModMask, xK_s), setInput "rpc setch " >> endOfLine)
                , ((myModMask, xK_x), setInputAndDone "rpc stop")
                , ((myModMask, xK_w), setInputAndDone "rpc webpage")
                -- system related
                -- suspend after one sec to avoid keyboard-mashing to wake up the machine again
                , ((myModMask, xK_l), setInputAndDone "rpd-running && rpc stop; sudo systemctl suspend")
                -- hot restart
                , ((myModMask, xK_h), setInputAndDone "sudo reboot")
                , ((myModMask, xK_q), setInputAndDone "sudo systemctl poweroff")
                -- , ((myModMask, xK_z), setInputAndDone "sleep 1; xset dpms force off")
                -- run stuff on a terminal
                , ((myModMask, xK_Return), changeInputAndDone $ \str -> "xeval " ++ escapeQuery str)
            ]} cimdb immediate final owi
mkDynamicPrompt m xpc sc cimdb i f = mkDynamicPrompt' m xpc sc cimdb i f def

dynamicPromptCommand m xpc sc cimdb = ("M-r", mkDynamicPrompt m xpc sc cimdb) 

promptCommands xpc =
    [ ("M-"++mf++"/", \immi final -> initMatches >>= \r -> mkSearchPrompt (xpc r) {searchPredicate = repeatedGrep} p validWindow (a immi final))
    | (mf, p, a) <- [ ("", "Go to window: ", \i f w -> deminimizeFocus w >> i >> f)
                    , ("S-", "Bring window: ", \i f w -> shiftWindowsHere [w] >> i >> f)
                    ]
    ]
    ++
    [ ("M-s " ++ af, \immi final -> do
        r <- initMatches
        -- get the current workspace name
        name <- getCurrentWorkspaceName
        renameWorkspacePrompt (xpc r) {
              searchPredicate = repeatedGrep
            , defaultText = name
        } immi final)
    | af <- ["S-c"] ]
    ++
    [ ("M-"++mf++"s "++nk++mk++af, fun)
    | (mf, action, prompt) <- [ ("", windows . W.view, "workspace")
                              , ("S-", \t -> onSelectedWindowsAfterMovingToTmpSpace $ \wins ->  windows (shiftWins t wins), "move to workspace") ]
    , (af, pos) <- insertPositionKeys
    , (nk, n) <- numberKeys
    , (mk, fun) <- [ ("", \immi final -> initMatches >>= \r -> newWorkspacePrompt (xpc r) {searchPredicate = repeatedGrep, autoComplete = Nothing} (prompt++" ("++(show pos)++")") pos n $ \t -> do
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


miscCommands myModMask xpc toggleFadeSet = 
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
            home <- io getHomeDirectory
            lns <- fmap lines $ io (readFile $ home ++ "/.xmonad/.winactivate") `catchX` (return "")
            case lns of
                 h:_ -> deminimizeFocus (read h) `catchX` (return ())
                 _ -> return ()
      )
    ----- wallpaper invoke -- {{{
    , ("M-z", initMatches >>= \r -> mkWPPrompt toggleFadeSet (xpc r) {
          searchPredicate = repeatedGrep
        , promptKeymap = M.fromList $ (M.toList $ vimXPKeymap r)++[
              ((myModMask, xK_n), setInputAndDone "next")
            , ((myModMask, xK_a), setInputAndDone "rate")
            , ((myModMask, xK_s), addOrTruncateTillPrefix "setch ")
            , ((myModMask, xK_f), addOrTruncateTillPrefix "flickr ")
            , ((myModMask, xK_u), setInputAndDone "ban")
            , ((myModMask, xK_t), setInputAndDone "trash")
        ]
    })
    ]

workspaceCommands = concatMap (processKey . addPrefix) $
    -- workspace prompt interface
    [ (pk++mk++wk, gt >>= flip whenJust ta) 
    | (mk, ta) <- [ ("", windows . W.view)
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
    [ ("f M-"++charToKeyStroke t, quickWorkspace [t] >>= windows . W.view) 
    | t <- symbolSequence ]
    ++
    -- workspace motion key g M-S-0 and g M-S-4
    [ ("g M-0", allWorkspaceTags >>= windows . W.view . head)
    , ("g M-S-4", allWorkspaceTags >>= windows . W.view . last) ]
    ++
    [ (mk++"e", f)
    | (mk, f) <- [ ("", onNextNeighbour W.view)
                 , ("S-", onSelectedWindowsAfterMovingToTmpSpace $ \wins -> onNextNeighbour $ \t -> shiftWins t wins)
                 , ("C-", onNextNeighbour W.view)]
    ]
    {-, ("M-S-f", withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)-}

------------- marco recording

-- macroRemapCommands are a complement of the defualt Macros Map; they use the macro mechanism 
-- but are more flexible in terms of their mappings
macroRemapCommands keybindings tgs =
    [ (k, (iterate (retrieveMacro keybindings macrols) (return ())) !! n) 
    | (nk, n) <- numberKeys
    , (k, macrols) <- [ ("M-c "++nk++"C-"++d++" "++tk, ["M-<Esc>", trigger, "M-C-"++d])
                      | d <- ["h", "l", "j", "k"]
                      , (tk, trigger) <- [ (filterKey g, "M-c i "++(filterKey g)) 
                                         | g <- allTaskGroupsWithFilterKey tgs ["c"]]
                                         ++
                                         [ ("/", "M-r") ]
                      ]
    ]

------- registers

paste invertInsert xls register = do
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
                 _ -> xls register
    if not (null wins)
       then do
            markWindowsSelection wins
            case (if invertInsert then not t else t) of
                 -- focus the last window (as in vim which places cursor on the last element)
                 False -> shiftWindowsHereAndFocusLast (Just (last wins)) wins
                 True -> shiftWindowsHereAndFocusLast Nothing wins
       else return ()

cut add register ls = flip correctFocus ls $ \wins -> do
                        deleteWindowsSelection wins
                        -- we only minimize the windows if they are NOT already minimized
                        mwins <- getMinimizedWindows
                        minimizeWindows $ filter (not . (`elem` mwins)) wins
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
    refresh

mvWindows nr fun wins = do
    rep <- orderedWindowsMatchingPredicate (hasTagQuery nr) 
    mapM_ (delTag nr) rep
    mapM_ (addTag nr) wins
    fun nr rep wins

putWindowsIntoRegister add register wins = do
    if not add then orderedWindowsMatchingPredicate (hasTagQuery register) >>= mapM_ (delTag register) else return ()
    mapM_ (addTag register) wins


-- make the keystrokes easier to type
appendablize (k, ka) = (k, \immi final -> ka >> immi >> final)
appendImmediate f (k, ka) = (k, \immi final -> ka (f >> immi) final)
normalize = fmap (\(k, ka) -> (k, ka (return ()) (return ())))
-- this is the version that allows for appending an action when the designed action has finished
myAppendableKeys :: KeyMask -> (HistoryMatches -> XPConfig) -> String -> [TaskGroup] -> CIMDb -> [(String, X ())] -> IORef (S.Set Window) -> [(String, X () -> X () -> X ())]
myAppendableKeys m xpc sc t cimdb additionalKeys toggleFadeSet =
    fmap (\(k, ka) -> flip appendImmediate (k, ka) $ appendMacroKey k)
       (fmap appendablize (visualCommands ++ motionKeyCommands xpc t)
        ++
        fmap (appendImmediate $ ifInVisualMode exitVisualMode $ return ())
           (fmap appendablize (layoutCommands t ++ historyCommands t ++ miscCommands m xpc toggleFadeSet ++ additionalKeys ++ workspaceCommands)
            -- dynamic prompt
            ++
            [dynamicPromptCommand m xpc sc cimdb]
            ++
            promptCommands xpc
            ++
            fmap appendablize (concatMap (processKey . addPrefix)
            [ (nk++".", getLastCommand >>= sequence_ . take n . repeat . id) 
            | (nk, n) <- numberMotionKeys])
            ++
            fmap (\(k, ka) -> flip appendImmediate (k, ka) $ saveLastCommand $ ka (return ()) (return ()))
               (fmap appendablize (cutCommands xpc t ++ pasteCommands xpc ++ yankCommands xpc t)
                ++
                changeCommands m xpc sc t cimdb
               )
           )
       )

type VimLayout = ModifiedLayout WithBorder (ModifiedLayout AvoidStruts (ModifiedLayout (ConfigurableBorder Ambiguity) (G.Groups (G.Groups (ModifiedLayout Rename (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)) (Choose (Mirror (ZoomRow GroupEQ)) Full)) (Choose (ZoomRow GroupEQ) Full))))

-- the clean up hook meant to be called after each spawning/respawning of the task bar
cleanupHook :: X ()
cleanupHook = do
    wallpaperStartupHook
    -- curr <- currWorkspace
    -- we will only need to switch to the right workspace during startup
    -- if not (validWS curr) then startTimer 0.5 >>= XS.put . StartWSChangeState else return ()
    -- if not (validWS curr) then windows (W.view tmpWorkspaceTag) else return ()

viminize :: Theme -> (HistoryMatches -> XPConfig) -> VimStatusTheme -> DynamicStatusBar -> String -> [TaskGroup] -> CIMDb -> [(String, X ())] -> (XConfig l1) -> IO (XConfig VimLayout)
viminize tabTheme xpc colors dsb sc tgs cimdb additionalKeys config = do
    toggleFadeSet <- newIORef S.empty
    let keys = myAppendableKeys m xpc sc tgs cimdb additionalKeys toggleFadeSet
        m = modMask config
    macroCommands <- retrieveMacroCommands keys $ \r -> (xpc r) {
                         searchPredicate = prefixSearchPredicate
                     }
    return $ config {
                  logHook = (logHook config) >> vimLogHook toggleFadeSet >> vimStatusLogHook colors tgs
                , manageHook = composeAll [vimManageHook tgs, manageHook config]
                , handleEventHook = (handleEventHook config) <+> (vimHandleEventHook m) <+> dynStatusBarEventHook dsb (spawn sc) cleanupHook
                , startupHook = startupHook config >> dynStatusBarStartup dsb (spawn sc) cleanupHook
                , layoutHook = noBorders . avoidStruts . lessBorders Screen $ vimLayout tabTheme tgs
                , terminal = myTerminal
                , workspaces = workspaces config
           } `additionalKeysP` ((normalize keys) ++ macroCommands ++ macroRemapCommands keys tgs)
