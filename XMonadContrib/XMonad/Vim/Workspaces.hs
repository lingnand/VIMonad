{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.Workspaces
    ( 
      tmpWorkspaceTag
    , lastWorkspaceTag
    , quickWorkspaceTags
    , symbolSequence
    , compareSymbolString
    , getSymbolStringSort
    , allWorkspaces
    , allWorkspaceTags
    , allWorkspaceNames
    , removeWorkspace
    , removeWorkspaces
    , removeAllWorkspaces
    , insertWorkspace
    , getWorkspaceNames
    , getWorkspaceNameForTag
    , setWorkspaceName
    , setWorkspaceNameByTag
    , setCurrentWorkspaceName
    , getCurrentWorkspaceName
    , findWorkspaceWithName
    , InsertPosition(..)
    , currWorkspace
    , workspaceStack
    -- windows
    , allOrderedWindows
    , wrapAroundWindowsMatchingPredicate
    , orderedWindowsMatchingPredicate
    , getSides
    -- scratchpad
    , scratchpadWorkspaceTag
    , mkNamedScratchpad
    -- managehook helpers
    , alwaysTrue
    , alwaysFalse
    , validWS
    , validWindow
    , isMinimized
    , isInCurrentWorkspace
    , isFocused
    , isOneOfWindows
    , hasSamePropertyAsFocused
    , focusIsInGStack
    , isFloating
    , windowIsSelected
    , doSink
    -- operations
    , deminimize
    , deminimizeFocus
    , killWindows
    , correctFocus
    , shiftWindowsHere
    , shiftWindowsHereAndFocusLast
    , shiftWins
    , swapWith
    , quickWorkspace
    -- layout / gstack
    , getFocusStack
    , getBaseCurrentStack
    , getBaseCurrentWindows
    , subgroupIndexToSymbol
    , subgroupSymbolSequence
    , columngroupSymbolSequence
    -- window history
    , getMarkedWindowsSize
    , jumpWindowHistory
    , historyBackKey
    , historyForwardKey
    , clearAllMarks
    , historyHook
    , markWindowsSelection
    -- mode
    , XMonadMode(..)
    , XMonadModeStorage(..)
    , VisualMode(..)
    , enterVisualMode
    , isInVisualMode
    , ifInVisualMode
    , visualModeUpdateToggleSet
    , visualModeUpdateEndMarker
    , onSelectedWindows
    , onSelectedWindowsAfterMovingToTmpSpace
    , getSelectedWindowStack
    , applySelectedWindowStack
    , exitToNormalMode
    , exitVisualMode
    , deleteWindowsSelection
    -- regs
    , compareRegs
    , sortRegs
    -- toggle
    , toggleGroup
    , toggleGroupPredicate
    , toggleWithinGroup
    , toggleWithinGroupPredicate
    , toggleTag
    ) where

import XMonad
import XMonad.Vim.Routine
import XMonad.Vim.Constants
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad
import qualified XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.CycleWS
import XMonad.Vim.InsertPosition
import XMonad.Vim.WorkspaceDirectories
import XMonad.Vim.WorkspaceHandles
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.WindowGo
import Data.List
import Data.Maybe
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Set as S
import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Minimize
import XMonad.Util.NamedScratchpad
import XMonad.Vim.Term
import XMonad.Util.WindowProperties
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.FloatNext (runLogHook)
import XMonad.Actions.SwapWorkspaces

---- Workspace sorting
-- The temp buffer is the default workspace and will never be recycled (make sure that it should proceed other buffers in the sequence
-- The symbolSequence replaces the originalSequence in the locale, thus enabling custom sorting
symbolSequence   = filter (/= '6') $ fullSequence quickWorkspaceSequence
-- the workspace tags are implemented as a sliding window across the symbol stream; at any instant the tags are a list counting from a specific symbol
symbolStream = symbolSequence ++ filter (not . (`elem` symbolSequence)) (enumFrom (toEnum 0))
tmpWorkspaceTag = [head quickWorkspaceSequence]
quickWorkspaceTags = map wrapList quickWorkspaceSequence
quickWorkspaceSequence = "`12345789"
--- subgroup symbol sequence: we eliminated '`' due to its ugliness from the subgroup indexing stream; and note that the sub group doesn't necessarily need an index after all
-- this is for tab keys
subgroupSymbolSequence = "1234567890-="
columngroupSymbolSequence = "123456789"
indexToSymbol s n = if n >= 0 && n < length s then Just [s !! n] else Nothing
subgroupIndexToSymbol = indexToSymbol $ fullSequence subgroupSymbolSequence

-- make sure that the star register is sorted last
compareRegs a b =
    case compare (elemIndex a ["*", ""]) (elemIndex b ["*", ""]) of
        EQ -> compareWithPriorList elemIndex (fmap wrapList "'\"123456789") a b
        r -> r
sortRegs = sortBy compareRegs

-- validWS is the workspace that are considered open to user (not including "NSP" in the obvious sense)
validWS = (/= scratchpadWorkspaceTag) . W.tag 
validWSType = WSIs $ return validWS

allWorkspaces = filterWorkspaces validWS
workspaceStack = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    return $ case break ((==curr) . W.tag) wss of
                    (bf, c:af) -> Just $ W.Stack c (reverse bf) af
                    _ -> Nothing

filterWorkspaces p = do
    ws <- gets windowset 
    sort <- getSymbolStringSort
    return $ sort . filter p $ W.workspaces ws

invalidWorkspaces = filterWorkspaces (not . validWS)

currWorkspace = gets (W.workspace . W.current . windowset)

allWorkspaceTags = allWorkspaces >>= return . map W.tag

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

quickWorkspace tag = do
    wsts <- allWorkspaceTags
    if length tag == 1
       then case stripPrefix (fmap head wsts) ((takeWhile (/= tc) symbolStream) ++ [tc]) of
                 Just ls -> sequence_ (map (DW.addHiddenWorkspace . \c -> [c]) ls) >> return tag
                 Nothing -> return tag
       -- this is the case where the tag is not even valid
       else return tag
    where tc = head tag

toggleGroup = nextMatch History toggleGroupPredicate
toggleGroupPredicate = fmap not isInCurrentGroup <&&> isInCurrentWorkspace <&&> fmap not isMinimized
toggleWithinGroup = nextMatch History toggleWithinGroupPredicate
toggleWithinGroupPredicate = isInCurrentGroup
toggleTag tag = do
    curr <- gets (W.currentTag . windowset)
    l <- lastWorkspaceTag
    return $ if curr == tag then l else tag

isRecyclableWS = do
          -- get the names for the workspace
          wns <- getWorkspaceNames
          return $ \w -> W.tag w /= tmpWorkspaceTag && not (':' `elem` (wns $ W.tag w)) && validWS w && isNothing (W.stack w) 

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

insertWorkspace name insp iter path fun = do
    let cw = do
        t <- reuseHiddenIdleWorkspaceWithName name insp 
        saveWorkspaceDirectory path t 
        return t
    res <- sequence $ take iter $ repeat $ cw
    case res of
         [] -> return ()
         h:_ -> fun $ last res

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

symbolFrom :: Char -> String
symbolFrom ch = dropWhile (/=ch) symbolStream

compareSymbol a b
    | a == b = EQ
    | otherwise = case dropWhile (\s -> s /= a && s /= b) symbolStream of
                        (h:_) | h == a -> LT
                              | otherwise -> GT
                        _ -> EQ

nextSymbol = head . tail . symbolFrom

compareSymbolString = compareLists compareSymbol
getSymbolStringSort = mkWsSort $ return compareSymbolString

---- perWorkspaceScratchpads are scratchpads that are workspace specific
lastWorkspaceTag = withWindowSet $ return . W.tag . head . filter validWS . W.hidden

---- workspaceNames
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
allWorkspaceNames = do
    ts <- allWorkspaceTags
    mapM getWorkspaceNameForTag ts

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

-- validWindow is the counterpart to validWS -- any validWindow is a window that is in a validWS; this is implemented as a managehook to put into many conditions
validWindow = ask >>= \w -> liftX $ do
    iws <- invalidWorkspaces
    return $ not $ any (containsWin w) iws
        where containsWin w ws = case W.stack ws of
                   Just s -> w `elem` (W.focus s) : ((W.up s) ++ (W.down s))
                   Nothing -> False

isMinimized = ask >>= \w -> liftX $ do
    wss <- allWorkspaces
    wins <- mapM getMinimizedWindows $ fmap W.layout wss
    return $ w `elem` (concat $ wins)

-- query bool defintion to matching windows in the current workspace
isInCurrentWorkspace = ask >>= \w -> liftX $ do
    ws <- gets windowset
    return $ w `elem` W.index ws

isFocused = ask >>= \w -> liftX $ do
    fs <- getFocusStack
    return $ fmap W.focus fs == Just w
     
isOneOfWindows ls = ask >>= \w -> do
    return $ w `elem` ls

isInCurrentGroup = ask >>= \w -> liftX $ do
    gs <- G.getCurrentGStack
    return $ maybe False (elem w . maybe [] G.flattened. G.current) gs


-- query bool defintion for matching a property same as the current window
hasSamePropertyAsFocused qry = ask >>= \w -> liftX $ do
    ws <- gets windowset 
    case W.peek ws of
         Just f ->  do
             pf <- runQuery qry f 
             pw <- runQuery qry w
             return $ pf == pw
         Nothing -> return False

alwaysTrue = liftX $ return True
alwaysFalse = liftX $ return False

focusIsInGStack = gets (W.peek . windowset) >>= maybe (return False) (fmap not . runQuery isFloating)

doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

routeMessageToWS fun mess = do
    wss <- allWorkspaces
    curr <- gets (W.currentTag . windowset)
    case find fun wss of
         Just ws
            | W.tag ws /= curr -> sendMessageWithNoRefresh mess ws 
            | otherwise -> sendMessage mess
         _ -> return ()

isFloating :: Query Bool
isFloating =  ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset

deminimizeFocus w = do
    deminimize w 
    focus w
deminimize w = routeMessageToWS ((w `elem`) . W.integrate' . W.stack) (RestoreMinimizedWin w)

killWindows ls = flip correctFocus ls $ \wins -> do
    deleteWindowsSelection wins
    -- now just kill all windows that 
    mapM_ killWindow wins

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

getFocusStack = do
    mbs <- getBaseCurrentStack
    mf <- gets (W.peek . windowset)
    (_, _, _, _, float, _, _) <- getSides
    case (mbs, mf, break ((==mf) . Just) float) of
         (Just (W.Stack sfw u d), Just fw, _) | sfw == fw -> return mbs
         (_, _, (fl,f:fr)) -> return $ Just $ W.Stack f (reverse fl) fr
         _ -> return Nothing

getBaseCurrentStack :: X (Maybe (W.Stack Window))
getBaseCurrentStack = do
    ins <- focusIsInGStack
    mgs <- G.getCurrentGStack 
    case (ins, mgs) of
        (True, Just gs) -> return $ G.toZipper $ G.baseCurrent gs
        _ -> return Nothing

getBaseCurrentWindows = getBaseCurrentStack >>= return . W.integrate'

-- scratchpad
scratchpadContext = "scratchpad"
singletonContext = "global"
perWSScratchpadContextKey = "perWorkspaceScratchpad"
perWSScratchpadContext tag = getWorkspaceHandle tag >>= \h -> return $ S.fromList [scratchpadContext, perWSScratchpadContextKey, h]

-- queries concerning the scratchpad context
isScratchpad = uniqueTermHasContext scratchpadContext
isPerWSScratchpad = uniqueTermHasContext perWSScratchpadContextKey
isPerWSScratchpadBoundToWS tag = isPerWSScratchpad <&&> (liftX (perWSScratchpadContext tag) >>= uniqueTermHasContexts)
isPerWSScratchpadBoundToCurrentWS = ask >>= \w -> liftX $ gets (W.currentTag . windowset) >>= \t -> runQuery (isPerWSScratchpadBoundToWS t) w

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

-- marks
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
jumpWindowHistory dir p = do
    let nmat dirp = nextMatch History (dirp <&&> validWindow <&&> fmap not isMinimized <&&> p) 
    case dir of
         Prev -> withFocused markWindow >> nmat (fmap not windowIsMarkedQuery)
         Next -> nmat windowIsMarkedQuery >> withFocused unmarkWindow >> runLogHook

-- modes
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

data VisualMode = Win | Row | Col deriving (Show, Read, Eq)
deleteWindowsSelection [] = return ()
deleteWindowsSelection ls = do
    VisualSelections (ac, pass) <- XS.get 
    let ft = S.filter (not . (`elem` ls))
    XS.put $ VisualSelections (ft ac, ft pass)
markWindowsSelection ls = do
    VisualSelections (ac, pass) <- XS.get 
    XS.put $ VisualSelections (S.filter (not . (`elem` ls)) ac, foldr S.insert pass ls)
clearAllSelections = XS.put $ VisualSelections (S.empty, S.empty)
windowIsSelected w = do
    VisualSelections (ac, pass) <- XS.get 
    return $ S.member w $ allDiff ac pass
windowIsSelectedQuery = ask >>= liftX . windowIsSelected

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

onSelectedWindows a = do
    (st, _) <- getSelectedWindowStack' False
    a $ W.integrate' st

onSelectedWindowsAfterMovingToTmpSpace a = do
    (st, opts) <- getSelectedWindowStack' False
    flip correctFocus (W.integrate' st) $ \wins -> do
        case opts of
             Just (mvs, lw) -> windows $ \s -> foldr (W.shiftWin scratchpadWorkspaceTag) s mvs
             _ -> return ()
        a wins


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

swapWith t = 
    -- first ensure that t is already accessible
    quickWorkspace t >>= \t -> do
        currTag <- gets (W.currentTag . windowset)
        swapWorkspaceHandlesByTags currTag t
        windows $ swapWorkspaces t currTag

-- the onselectedwindows protocol expects: the windows disappear after the action applies
-- the after moving to tmpspace is useful if you want to apply a one-step operation (like shifting)
shiftWins t wins s = foldr (W.shiftWin t) s wins

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
