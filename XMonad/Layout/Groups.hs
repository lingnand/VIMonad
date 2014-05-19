{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, DeriveFunctor
  , DeriveTraversable, DeriveFoldable, ImpredicativeTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Two-level layout with windows split in individual layout groups,
-- themselves managed by a user-provided layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups ( -- * Usage
                              -- $usage
                              -- * Creation
                              group
                            , group3
                              -- * Messages
                            , GroupsMessage(..)
                            , ModifySpec
                              -- ** Useful 'ModifySpec's
                            , swapUp
                            , swapDown
                            , swapWith
                            , swapWithLast
                            , insertAt
                            , swapMaster
                            , focusUp
                            , focusDown
                            , focusMaster
                            , focusAt
                            , focusLast
                            , swapGroupUp
                            , swapGroupDown
                            , swapGroupMaster
                            , focusGroupUp
                            , focusGroupDown
                            , focusGroupMaster
                            , focusGroupAt
                            , swapGroupWith
                            , moveToGroupUp
                            , moveToGroupDown
                            , moveToNewGroupUp
                            , moveToNewGroupDown
                            , moveToGroupAt
                            , splitGroup
                            , moveWindowsToNewGroupUp
                            , moveWindowsToNewGroupDown
                            , moveWindowsUp
                            , moveWindowsDown
                            , moveWindowsToGroupAt
                            , collapse
                              -- * Types
                            , Groups(..)
                            , Group(..)
                            , onZipper
                            , onLayout
                            , WithID(..)
                            , Uniq(..)
                            , sameID
                            , GroupsManageHook
                            , MultiStack(..)
                            , filter
                            , GStack
                            , toZipper
                            , fromZipper
                            , level
                            , current
                            , baseCurrent
                            , bases
                            , groupAt
                            , focal
                            , contains
                            , flattened
                            , flattenedSet
                            , insertWUp
                            , insertGUp
                            , getCurrentGStack
                            , getGStackForWSTag
                            , getGStack
                            , applyGStack
                            {-, applyGStackForWSTag-}
                            , applyGStackTransform
                            {-, applyGStackTransformForWSTag-}
                            , moveSubGroupToGroupUp
                            , moveSubGroupToGroupDown
                            , splitSubGroupUp
                            , splitSubGroupDown
                            , sortWindows
                            ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.Stack

import Prelude hiding (filter)
import qualified Data.List as L ((\\), maximumBy, find, filter)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes, fromJust)
import Data.Either
import Data.Tuple
import Data.List (sortBy)
import Text.Read
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import Control.Monad (forM)
import qualified Data.Traversable as V
import qualified Data.Foldable as F
import Data.Maybe
import qualified XMonad.Util.Types as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- This module provides a layout combinator that allows you
-- to manage your windows in independent groups. You can provide
-- both the layout with which to arrange the windows inside each
-- group, and the layout with which the groups themselves will
-- be arranged on the screen.
--
-- The "XMonad.Layout.Groups.Examples" and "XMonad.Layout.Groups.Wmii"
-- modules contain examples of layouts that can be defined with this
-- combinator. They're also the recommended starting point
-- if you are a beginner and looking for something you can use easily.
--
-- One thing to note is that 'Groups'-based layout have their own
-- notion of the order of windows, which is completely separate
-- from XMonad's. For this reason, operations like 'XMonad.StackSet.SwapUp'
-- will have no visible effect, and those like 'XMonad.StackSet.focusUp'
-- will focus the windows in an unpredictable order. For a better way of
-- rearranging windows and moving focus in such a layout, see the
-- example 'ModifySpec's (to be passed to the 'Modify' message) provided
-- by this module.
--
-- If you use both 'Groups'-based and other layouts, The "XMonad.Layout.Groups.Helpers"
-- module provides actions that can work correctly with both, defined using
-- functions from "XMonad.Actions.MessageFeedback".

-- | Create a 'Groups' layout.
--
-- Note that the second parameter (the layout for arranging the
-- groups) is not used on 'Windows', but on 'Group's. For this
-- reason, you can only use layouts that don't specifically
-- need to manage 'Window's. This is obvious, when you think
-- about it.
group :: l Window -> l2 (Group l Window) -> Groups l l2 Window
group l l2 = Groups l l2 startingGroups (U 1 0)
    where startingGroups = fromJust $ singletonZ $ G (ID (U 0 0) l) emptyZ

-- first layout for managing windows, the second to the second level, so on and so forth
group3 l l2 l3 = Groups g l3 start (U 2 0)
    where g = group l l2
          start = fromJust $ singletonZ $ G (ID (U 2 1) g) emptyZ

-- * Stuff with unique keys

data Uniq = U Integer Integer
  deriving (Eq, Show, Read)

-- | From a seed, generate an infinite list of keys and a new
-- seed. All keys generated with this method will be different
-- provided you don't use 'gen' again with a key from the list.
-- (if you need to do that, see 'split' instead)
gen :: Uniq -> (Uniq, [Uniq])
gen (U i1 i2) = (U (i1+1) i2, zipWith U (repeat i1) [i2..])

-- | Split an infinite list into two. I ended up not
-- needing this, but let's keep it just in case.
-- split :: [a] -> ([a], [a])
-- split as = snd $ foldr step (True, ([], [])) as
--     where step a (True, (as1, as2)) = (False, (a:as1, as2))
--           step a (False, (as1, as2)) = (True, (as1, a:as2))

-- | Add a unique identity to a layout so we can
-- follow it around.
data WithID l a = ID { getID :: Uniq
                     , unID :: (l a)}
  deriving (Show, Read)

-- | Compare the ids of two 'WithID' values
sameID :: WithID l a -> WithID l a -> Bool
sameID (ID id1 _) (ID id2 _) = id1 == id2

instance Eq (WithID l a) where
    ID id1 _ == ID id2 _ = id1 == id2

instance LayoutClass l a => LayoutClass (WithID l) a where
    runLayout ws@W.Workspace { W.layout = ID id l } r
        = do (placements, ml') <- flip runLayout r
                                     ws { W.layout = l}
             return (placements, ID id <$> ml')
    handleMessage (ID id l) sm = do ml' <- handleMessage l sm
                                    return $ ID id <$> ml'
    description (ID _ l) = description l



-- * The 'Groups' layout


-- ** Datatypes

-- | A group of windows and its layout algorithm.
data Group l a = G { gLayout :: WithID l a
                   , gZipper :: Zipper a }
  deriving (Show, Read, Eq)

onLayout :: (WithID l a -> WithID l a) -> Group l a -> Group l a
onLayout f g = g { gLayout = f $ gLayout g }

onZipper :: (Zipper a -> Zipper a) -> Group l a -> Group l a
onZipper f g = g { gZipper = f $ gZipper g }

-- | The type of our layouts.
data Groups l l2 a = Groups { -- | The starting layout for new groups
                              baseLayout :: l a
                              -- | The layout for placing each group on the screen
                            , partitioner :: l2 (Group l a)
                              -- | The window groups
                            , groups :: W.Stack (Group l a)
                              -- | A seed for generating unique ids
                            , seed :: Uniq
                            }

deriving instance (Show a, Show (l a), Show (l2 (Group l a))) => Show (Groups l l2 a)
deriving instance (Read a, Read (l a), Read (l2 (Group l a))) => Read (Groups l l2 a)

-- | Messages accepted by 'Groups'-based layouts.
-- All other messages are forwarded to the layout of the currently
-- focused subgroup (as if they had been wrapped in 'ToFocused').
data GroupsMessage = ToEnclosing SomeMessage -- ^ Send a message to the enclosing layout
                                             -- (the one that places the groups themselves)
                   | ToGroup Int SomeMessage -- ^ Send a message to the layout for nth group
                                             -- (starting at 0)
                   | ToFocused SomeMessage -- ^ Send a message to the layout for the focused
                                           -- group
                   | ToAll SomeMessage -- ^ Send a message to all the sub-layouts
                   | Refocus -- ^ Refocus the window which should be focused according
                             -- to the layout.
                   | Modify ModifySpec -- ^ Modify the ordering\/grouping\/focusing
                                       -- of windows according to a 'ModifySpec'
                   | GroupsManage GroupsManageHook
                                        -- a message sent to the layout to determine the function used when new windows are added into this layout; a message containing a function is only used once when new windows are found, after that the function is discarded
                     deriving Typeable

-- a groups managehook takes a list of windows and original gstack, returns a new gstack (presumably containing all the new windows)
type GroupsManageHook = [Window] -> (Maybe GStack) -> (Maybe GStack)
data GroupsManageStorage = GroupsManageStorage (Maybe GroupsManageHook) deriving (Typeable)
instance ExtensionClass GroupsManageStorage where
    initialValue = GroupsManageStorage Nothing
    

data GroupsSaveMessage = GroupsSave deriving Typeable-- a message sent to the groups layout to save the current group structure inside the storage, and later retrieve it via that storage
instance Show GroupsSaveMessage where
    show _ = "GroupsSave"
instance Message GroupsSaveMessage

instance Show GroupsMessage where
    show (ToEnclosing _) = "ToEnclosing {...}"
    show (ToGroup i _) = "ToGroup "++show i++" {...}"
    show (ToFocused _) = "ToFocused {...}"
    show (ToAll _) = "ToAll {...}"
    show Refocus = "Refocus"
    show (Modify _) = "Modify {...}"
    show (GroupsManage _) = "GroupsManage {...}"

instance Message GroupsMessage

modifyGroups :: (Zipper (Group l a) -> Zipper (Group l a))
             -> Groups l l2 a -> Groups l l2 a
modifyGroups f g = let (seed', id:_) = gen (seed g)
                       defaultGroups = fromJust $ singletonZ $ G (ID id $ baseLayout g) emptyZ
                   in g { groups = fromMaybe defaultGroups . f . Just $ groups g
                        , seed = seed' }


-- ** Readaptation

-- | Adapt our groups to a new stack.
-- This algorithm handles window additions and deletions correctly,
-- ignores changes in window ordering, and tries to react to any
-- other stack changes as gracefully as possible.
readapt :: Zipper Window -> Groups l l2 Window -> (WithID l Window -> (Zipper (Group l Window), [Window]) -> Zipper (Group l Window)) -> Groups l l2 Window
readapt z g mh = let mf = getFocusZ z
                     (seed', id:_) = gen $ seed g
                     g' = g { seed = seed' }
                 in flip modifyGroups g' $ mapZ_ (onZipper $ removeDeleted z)
                                           >>> filterKeepLast (isJust . gZipper)
                                           >>> findNewWindows (W.integrate' z)
                                           >>> mh (ID id $ baseLayout g)
                                           >>> focusGroup mf
                                           >>> onFocusedZ (onZipper $ focusWindow mf)
    where filterKeepLast _ Nothing = Nothing
          filterKeepLast f z@(Just s) = maybe (singletonZ $ W.focus s) Just
                                            $ filterZ_ f z

-- | Remove the windows from a group which are no longer present in
-- the stack.
removeDeleted :: Eq a => Zipper a -> Zipper a -> Zipper a
removeDeleted z = filterZ_ (flip elemZ z)

-- | Identify the windows not already in a group.
findNewWindows :: Eq a => [a] -> Zipper (Group l a)
               -> (Zipper (Group l a), [a])
findNewWindows as gs = (gs, foldrZ_ removePresent as gs)
    where removePresent g as' = L.filter (not . flip elemZ (gZipper g)) as'

-- | Add windows to the focused group. If you need to create one,
-- use the given layout and an id from the given list.
addWindows :: WithID l a -> (Zipper (Group l a), [a]) -> Zipper (Group l a)
addWindows l (Nothing, as) = singletonZ $ G l (W.differentiate as)
addWindows _ (z, as) = onFocusedZ (onZipper add) z
    where add z = foldl (flip insertUpZ) z as

addWindowsWithHook :: GroupsManageHook -> WithID l Window -> (Zipper (Group l Window), [Window]) -> Zipper (Group l Window)
addWindowsWithHook trans l0 (z, as) = toGroupsZipper l0 z $ trans as (fromGroupsZipper z)

-- | Focus the group containing the given window
focusGroup :: Eq a => Maybe a -> Zipper (Group l a) -> Zipper (Group l a)
focusGroup Nothing = id
focusGroup (Just a) = fromTags . map (tagBy $ elemZ a . gZipper) . W.integrate'

-- | Focus the given window
focusWindow :: Eq a => Maybe a -> Zipper a -> Zipper a
focusWindow Nothing = id
focusWindow (Just a) = fromTags . map (tagBy (==a)) . W.integrate'

-- Extension: save the information in a save location for the user to query upon
-- we need a comprehensive way to save the information regarding the stack
data MultiStack a = Leaf (Maybe (W.Stack a)) | Node (W.Stack (MultiStack a))
deriving instance (Show a) => Show (MultiStack a)
deriving instance (Read a) => Read (MultiStack a)
deriving instance (Eq a) => Eq (MultiStack a)
deriving instance (Ord a) => Ord (W.Stack a)
deriving instance (Ord a) => Ord (MultiStack a)
deriving instance Functor W.Stack
deriving instance Functor MultiStack
deriving instance F.Foldable W.Stack
deriving instance F.Foldable MultiStack
deriving instance V.Traversable W.Stack
deriving instance V.Traversable MultiStack
{-deriving instance Foldable W.Stack-}

type GStack = MultiStack Window

filter fun (Node (W.Stack f u d)) = 
    let f' = filter fun f
        ft = fmap (filter fun)
        u' = ft u
        d' = ft d
    in case W.filter isJust (W.Stack f' u' d') of
            Just (W.Stack nf nu nd) -> Just $ Node $ W.Stack (fromJust nf) (fmap fromJust nu) (fmap fromJust nd)
            _ -> Nothing
filter fun (Leaf (Just s@(W.Stack f u d))) =
    case W.filter fun s of
         Just s' -> Just $ Leaf $ Just s'
         _ -> Nothing
filter _ _ = Nothing

{-instance Functor MultiStack where-}
    {-fmap f (Leaf (Just (W.Stack f' u' d'))) = Leaf $ Just $ W.Stack (f f') (fmap f u') (fmap f d')-}
    {-fmap f (Leaf Nothing) = Leaf Nothing-}
    {-fmap f (Node (W.Stack f' u' d')) = Node $ W.Stack (fmap f f') (fmap (fmap f) u') (fmap (fmap f) d')-}

{-instance Foldable MultiStack where-}
    {-foldMap f (Leaf -}

data GStackStorage = GStackStorage (Maybe GStack) deriving (Typeable, Read, Show)
instance ExtensionClass GStackStorage where
    initialValue = GStackStorage Nothing
    extensionType = PersistentExtension

{-windowInGStack w (Leaf s) = w `elem` (W.integrate' s)-}
{-windowInGStack w (Node s) = any (windowInGStack w) $ W.integrate s-}

flattenedSet :: Ord a => MultiStack a -> S.Set a
flattenedSet = S.fromList . flattened
flattened :: MultiStack a -> [a]
flattened (Node s) = concatMap flattened $ W.integrate s
flattened (Leaf s) = W.integrate' s

level (Leaf _) = 1
level (Node (W.Stack f _ _)) = (level f) + 1

current (Leaf _) = Nothing
current (Node (W.Stack f _ _)) = Just f

baseCurrent s@(Leaf _) = s
baseCurrent (Node (W.Stack f _ _)) = baseCurrent f

-- return a node stack shortening all 
bases s@(Node (W.Stack (Leaf _) u d)) = Just s
bases (Node (W.Stack f u d)) = case bases f of
                                    Just (Node (W.Stack f' u' d')) -> Just $ Node $ W.Stack f' (u'++(concatMap reverse $ pl u)) (d'++(concat $ pl d))
                                    _ -> Nothing
            where pl = fmap ((\(Node s) -> W.integrate s) . fromJust) . L.filter isJust . fmap bases
bases (Leaf _) = Nothing

groupAt i (Leaf s) = Nothing
groupAt i (Node s) = let ls = W.integrate s in if i < length ls then Just (ls !! i) else Nothing

-- return the smallest focus element
focal (Leaf s) = fmap W.focus s
focal (Node (W.Stack f _ _)) = focal f

contains w (Leaf s) = w `elem` (W.integrate' s)
contains w (Node s) = any (contains w) $ W.integrate s

-- gives back the Stack Window representation (path shortening in essence)
toZipper (Leaf s) = s
toZipper (Node (W.Stack f u d)) = case toZipper f of
                                       Just (W.Stack f' u' d') -> Just $ W.Stack f' (u' ++ concatMap (reverse . flattened) u)  (d' ++ concatMap flattened d)
                                       Nothing -> Nothing

-- we should probably associate each group with its respective id and save the layout into its own id every time instead of relying on unification of the tree structure
{-splitAtLevel' i gs@(Leaf _) = (gs, [gs])-}
{-splitAtLevel' i gs@(Node (W.Stack f u d))-}
    {-| i == 0 =  let (before, fo:after) = break (==focal gs) (S.toList $ winset f) in-}
                    {-(Leaf $ W.Stack fo ((reverse before)++(S.toList $ S.unions $ fmap winset u)) (after++(S.toList $ S.unions $ fmap winset d)), f:(u++d))-}
    {-| i > 0 = let (f', fs) = splitAtLevel' (i-1) f-}
                  {-(u', us) = unzip $ fmap (splitAtLevel' (i-1)) u-}
                  {-(d', ds) = unzip $ fmap (splitAtLevel' (i-1)) d in-}
                  {-(Node $ W.Stack f' u' d', fs++(concat us)++(concat ds))-}
    {-| otherwise = (gs, [gs])-}

{-splitAtLevel i gs = let (a, b) = splitAtLevel' i gs in a:b-}

insertWUp w (Leaf s) = Leaf $ insertUpZ w s
insertWUp w (Node (W.Stack f u d)) = Node $ W.Stack (insertWUp w f) u d
insertGUp g s@(Leaf _) = s
insertGUp g (Node (W.Stack f u d)) = Node $ W.Stack g u (f:d)

fromGroup (G _ s) = Leaf s
fromGroups (Groups _ _ s@(W.Stack f up down) _) = fromJust $ fromGroupsZipper $ Just s

fromGroupsZipper (Just (W.Stack f up down)) =
    let cf = fromGroup f
        cu = fmap fromGroup up
        cd = fmap fromGroup down
    in Just $ Node $ W.Stack cf cu cd
fromGroupsZipper _ = Nothing

toGroupsZipper l0 oz (Just (Node s@(W.Stack f u d))) =
    -- we need to run an estimation here for retaining the same layout for the same set of windows
    let (bef,nf:aft) = splitAt (length u) $ fst $ foldr match ([], fmap (\(G l z') -> (W.integrate' z', l)) $ W.integrate' oz) $ fmap toZipper $ W.integrate s
        match zp (r,cls) = let wins = W.integrate' zp
                               score = length . L.filter (`elem` wins)
                               (wss, gls) = unzip cls
                               (res, rls) = case (cls, L.maximumBy (\(s1,_,_) (s2,_,_)-> compare s1 s2) $ zip3 (fmap score wss) wss gls) of
                                                 ([],_) -> (l0, [])
                                                 (_,(s,ws,l)) 
                                                    | s == 0 -> (l0, cls)
                                                    | otherwise -> (l, L.filter ((/=ws) . fst) cls)
                           in ((G res zp):r, rls)
    in Just $ W.Stack nf (reverse bef) aft
toGroupsZipper _ _ _ = Nothing

getCurrentGStack = gets (W.currentTag . windowset) >>= getGStackForWSTag

getGStackForWSTag t = do
    -- send the message to the current layout for saving the information regarding
    wss <- gets (W.workspaces . windowset)
    case L.find ((==t) . W.tag) wss of
         Just ws -> getGStack $ W.layout ws
         _ -> return Nothing

getGStack l = do
    res <- handleMessage l (SomeMessage GroupsSave) `catchX` return Nothing
    case res of
         Just _ -> do
             GStackStorage gs <- XS.get
             return gs
         _ -> return Nothing

genLayouts (ID d base) = fmap (\i -> (ID i base)) $ snd $ gen d

-- the send function should be able to deliver a message to the given layout l
-- nothing to apply upon, as this will be on the group 
applyGStack' (Leaf _) send = return ()
applyGStack' mn@(Node ns@(W.Stack _ u _)) send = do
    send $ Modify $ \l0 oz -> toGroupsZipper l0 oz (Just mn)
    -- then send separate message to each and every sub layouts
    let (bef,fx:aft) = splitAt (length u) $ fmap (\(i, g)-> applyGStack' g (send . ToGroup i . SomeMessage)) $ zip [0..] $ W.integrate ns
    sequence_ $ bef++aft++[fx]

applyGStackForWSTag t gs = do
    wss <- gets (W.workspaces . windowset)
    case L.find ((==t) . W.tag) wss of
         -- for some weird reason it's not working when using custom send methods (even with the exact same code)
         {-Just ws -> applyGStack' gs (\m -> do-}
                         {-ml' <- handleMessage (W.layout ws) (SomeMessage m) `catchX` return Nothing -}
                         {-whenJust ml' $ \l' ->-}
                             {-windows $ \s -> s { W.current = mods l' (W.current s)-}
                                               {-, W.visible = fmap (mods l') (W.visible s)-}
                                               {-, W.hidden = fmap (modws l') (W.hidden s)-}
                                               {-})-}
         {-Just ws -> applyGStack' gs (\m -> sendMessageWithNoRefresh m ws >> refresh)-}
         Just ws -> applyGStack' gs sendMessage
         _ -> return ()
         where mods l scr = scr { W.workspace = modws l (W.workspace scr) }
               modws l ws = if W.tag ws == t then ws { W.layout = l} else ws

applyGStack gs = gets (W.currentTag . windowset) >>= \t -> applyGStackForWSTag t gs

fromZipper sing 1 = Leaf sing
fromZipper sing n = if n <= 1 then Leaf sing else Node $ W.Stack (fromZipper sing (n-1)) [] []
    
collapseAtTrans i s
    | i <= 0 = fromZipper (toZipper s) (level s)
    | otherwise = case s of
        Node (W.Stack f u d) -> Node $ W.Stack (c f) (fmap c u) (fmap c d)
        _ -> s
        where c = collapseAtTrans (i-1)

applyGStackTransformForWSTag t f = do
    mgs <- getGStackForWSTag t
    case mgs of
            -- send layered messages to the underlying groups and leaves
         Just gs -> do
             -- we need to first collapse all the windows in each and every group so as to avoid window drawing issues
             {-applyGStack (collapseAtTrans (level gs-2) gs) sendMessage-}
             {-sendMessage $ Modify $ \l0 s -> s-}
             {-spawn $ "echo '"++(show gs)++"' > ~/.xmonad/xmonad.test"-}
             applyGStackForWSTag t (f gs)
         _ -> return ()

applyGStackTransform f = gets (W.currentTag . windowset) >>= \t -> applyGStackTransformForWSTag t f

moveSubGroupTrans dir split (Node (W.Stack (Node f'@(W.Stack f u d)) u' d')) =
    let (fg, r) = _removeFocused f'
        (inc, ls) = case r of
                          Just g -> (if dir == T.Prev then if split then 0 else (-1) else 1, reverse u' ++ [Node g] ++ d')
                          Nothing -> (if dir == T.Prev && not split then (-1) else 0, reverse u' ++ d')
        insi = length u' + inc
    in if not split then let insi' = insi `mod` (length ls)
                             (before, (Node (W.Stack inf inu ind)):after) = splitAt insi' ls
                         in Node $ W.Stack (Node $ W.Stack fg inu (inf:ind)) (reverse before) after
                    else let (before, after) = splitAt insi ls
                         in Node $ W.Stack (Node $ W.Stack fg [] []) (reverse before) after
moveSubGroupTrans _ _ g = g

moveSubGroupToGroupUp = applyGStackTransform $ moveSubGroupTrans T.Prev False
moveSubGroupToGroupDown = applyGStackTransform $ moveSubGroupTrans T.Next False
splitSubGroupUp = applyGStackTransform $ moveSubGroupTrans T.Prev True
splitSubGroupDown = applyGStackTransform $ moveSubGroupTrans T.Next True
{-moveSubGroupToGroupUp = moveSubGroup T.Prev False-}
{-moveSubGroupToGroupDown = moveSubGroup T.Next False-}
{-splitSubGroupUp = moveSubGroup T.Prev True-}
{-splitSubGroupDown = moveSubGroup T.Next True-}

moveSubGroup dir split = do
    -- get all the windows in the current sub group
    mgs <- getCurrentGStack
    case mgs of
         Just (Node (W.Stack (Node f') u' d')) -> 
             let (fg, r) = _removeFocused f'
                 (inc, ls) = case r of
                                   Just g -> (if dir == T.Prev then if split then 0 else (-1) else 1, reverse u' ++ [Node g] ++ d')
                                   Nothing -> (if dir == T.Prev && not split then (-1) else 0, reverse u' ++ d')
                 insi = length u' + inc
                 insi' = insi `mod` (length ls)
                 tl = Leaf . toZipper
                 fms = if not split then let (before, (Node (W.Stack inf inu ind)):after) = splitAt insi' ls
                                         in Node $ W.Stack (tl (Node $ W.Stack fg inu (inf:ind))) (fmap tl $ reverse before) (fmap tl after)
                                 else let (before, after) = splitAt insi ls
                                      in Node $ W.Stack (tl (Node $ W.Stack fg [] [])) (fmap tl $ reverse before) (fmap tl after)
             in do
                 if split then return () else do
                      sendMessage $ ToGroup insi' $ SomeMessage $ GroupsManage $ \ls s ->
                          fmap (insertGUp $ Leaf $ W.differentiate ls) s
                 applyGStack fms
         _ -> return ()

-- * Interface

-- ** Layout instance

instance (LayoutClass l Window, LayoutClass l2 (Group l Window))
    => LayoutClass (Groups l l2) Window where

        description (Groups _ p gs _) = s1++" by "++s2
        -- trying to communicate the entire group information
        {-description g = show g-}
            where s1 = description $ gLayout $ W.focus gs
                  s2 = description p
                  {-cur = (length $ W.up gs) + 1-}
                  {-[>cur = getID $ gLayout $ W.focus gs<]-}
                  {-total = (length $ W.up gs) + 1 + (length $ W.down gs)-}


        runLayout ws@(W.Workspace wsn _l z) r = 
            do 
               -- get the managehook function
               GroupsManageStorage mmh <- XS.get
               mh <- case mmh of
                         Just h -> do
                             XS.put $ GroupsManageStorage Nothing
                             return $ addWindowsWithHook h
                         _ -> return addWindows
               let l = readapt z _l mh
               (areas, mpart') <- runLayout ws { W.layout = partitioner l
                                               , W.stack = Just $ groups l } r

               results <- forM areas $ \(g, r') -> runLayout ws { W.layout = gLayout g
                                                                , W.stack = gZipper g } r'

               let hidden = map gLayout (W.integrate $ groups l) L.\\ map (gLayout . fst) areas
               hidden' <- mapM (flip handleMessage $ SomeMessage Hide) hidden

               let placements = concatMap fst results
                   newL = justMakeNew l mpart' (map snd results ++ hidden')

               {-let place = case fmap winlist mgs of-}
                               {-Just ls -> sortBy (\(w1,_) (w2,_) -> compare (elemIndex w1 ls) (elemIndex w2 ls))-}
                               {-_ -> id-}

               return $ (placements, newL)


        handleMessage l sm | Just (GroupsManage fun) <- fromMessage sm
            = do
                -- save the function into the storage
                XS.put $ GroupsManageStorage $ Just fun
                return (Just l)

        handleMessage l@(Groups _ p (W.Stack f u d) _) sm | Just GroupsSave <- fromMessage sm
            = do 
                -- a dirty hack for saving the information related to the groups
                cf <- cg f
                cu <- mapM cg u
                cd <- mapM cg d
                XS.put $ GStackStorage $ Just $ Node $ W.Stack (fromJust cf) (fmap fromJust cu) (fmap fromJust cd)
                return (Just l)
                    where cg g = do
                              res <- handleMessage (gLayout g) (SomeMessage GroupsSave) `catchX` return Nothing
                              mgs <- case res of
                                   Just _ -> do
                                       GStackStorage gs <- XS.get
                                       return gs
                                   Nothing -> return Nothing
                              case mgs of
                                   Nothing -> return $ Just $ fromGroup g
                                   r -> return r

        handleMessage l@(Groups _ p _ _) sm | Just (ToEnclosing sm') <- fromMessage sm
            = do mp' <- handleMessage p sm'
                 return $ maybeMakeNew l mp' []

        handleMessage l@(Groups _ p gs _) sm | Just (ToAll sm') <- fromMessage sm
            = do mp' <- handleMessage p sm'
                 mg's <- mapZM_ (handle sm') $ Just gs
                 return $ maybeMakeNew l mp' $ W.integrate' mg's
            where handle sm (G l _) = handleMessage l sm

        handleMessage l sm | Just a <- fromMessage sm
            = let _rightType = a == Hide -- Is there a better-looking way
                                         -- of doing this?
              in handleMessage l $ SomeMessage $ ToAll sm

        handleMessage l@(Groups _ _ z _) sm = case fromMessage sm of
              Just (ToFocused sm') -> do mg's <- W.integrate' <$> handleOnFocused sm' z
                                         return $ maybeMakeNew l Nothing mg's
              Just (ToGroup i sm') -> do mg's <- handleOnIndex i sm' z
                                         return $ maybeMakeNew l Nothing mg's
              Just (Modify spec) -> case applySpec spec l of
                                      Just l' -> refocus l' >> return (Just l')
                                      Nothing -> return $ Just l
              Just Refocus -> refocus l >> return (Just l)
              Just _ -> return Nothing
              Nothing -> handleMessage l $ SomeMessage (ToFocused sm)
            where handleOnFocused sm z = mapZM step $ Just z
                      where step True (G l _) = handleMessage l sm
                            step False _ = return Nothing
                  handleOnIndex i sm z = mapM step $ zip [0..] $ W.integrate z
                      where step (j, (G l _)) | i == j = handleMessage l sm
                            step _ = return Nothing



justMakeNew :: Groups l l2 a -> Maybe (l2 (Group l a)) -> [Maybe (WithID l a)]
            -> Maybe (Groups l l2 a)
justMakeNew g mpart' ml's = Just g { partitioner = fromMaybe (partitioner g) mpart'
                                   , groups = combine (groups g) ml's }
    where combine z ml's = let table = map (\(ID id a) -> (id, a)) $ catMaybes ml's
                           in flip mapS_ z $ \(G (ID id l) ws) -> case lookup id table of
                                        Nothing -> G (ID id l) ws
                                        Just l' -> G (ID id l') ws
          mapS_ f = fromJust . mapZ_ f . Just


maybeMakeNew :: Groups l l2 a -> Maybe (l2 (Group l a)) -> [Maybe (WithID l a)]
             -> Maybe (Groups l l2 a)
maybeMakeNew _ Nothing ml's | all isNothing ml's = Nothing
maybeMakeNew g mpart' ml's = justMakeNew g mpart' ml's

refocus :: Groups l l2 Window -> X ()
refocus g = case getFocusZ $ gZipper $ W.focus $ groups g
            of Just w -> focus w
               Nothing -> return ()

-- ** ModifySpec type

-- | Type of functions describing modifications to a 'Groups' layout. They
-- are transformations on 'Zipper's of groups.
--
-- Things you shouldn't do:
--
-- * Forge new windows (they will be ignored)
--
-- * Duplicate windows (whatever happens is your problem)
--
-- * Remove windows (they will be added again)
--
-- * Duplicate layouts (only one will be kept, the rest will
--   get the base layout)
--
-- Note that 'ModifySpec' is a rank-2 type (indicating that 'ModifySpec's must
-- be polymorphic in the layout type), so if you define functions taking
-- 'ModifySpec's as arguments, or returning them,  you'll need to write a type
-- signature and add @{-# LANGUAGE Rank2Types #-}@ at the beginning
type ModifySpec = forall l. WithID l Window
                -> Zipper (Group l Window)
                -> Zipper (Group l Window)

-- | Apply a ModifySpec.
applySpec :: ModifySpec -> Groups l l2 Window -> Maybe (Groups l l2 Window)
applySpec f g = let (seed', id:ids) = gen $ seed g
                    g' = flip modifyGroups g $ f (ID id $ baseLayout g)
                                               >>> toTags
                                               >>> foldr reID ((ids, []), [])
                                               >>> snd
                                               >>> fromTags
                in case groups g == groups g' of
                     True -> Nothing
                     False -> Just g' { seed = seed' }

    where reID eg ((id:ids, seen), egs)
              = let myID = getID $ gLayout $ fromE eg
                in case elem myID seen of
                     False -> ((id:ids, myID:seen), eg:egs)
                     True -> ((ids, seen), mapE_ (setID id) eg:egs)
              where setID id (G (ID _ _) z) = G (ID id $ baseLayout g) z
          reID _ (([], _), _) = undefined -- The list of ids is infinite





-- ** Misc. ModifySpecs

-- | helper
onFocused :: (Zipper Window -> Zipper Window) -> ModifySpec
onFocused f _ gs = onFocusedZ (onZipper f) gs

-- | Swap the focused window with the previous one.
swapUp :: ModifySpec
swapUp = onFocused swapUpZ

-- | Swap the focused window with the next one.
swapDown :: ModifySpec
swapDown = onFocused swapDownZ

-- | Swap the focused window with the i'th window (from the user's perspective
swapWithZ :: Int -> Zipper a -> Zipper a
swapWithZ _ Nothing = Nothing
swapWithZ i (Just (W.Stack f up down)) 
    | i < lu = Just $ W.Stack f (tail a) (reverse b++[head a]++down)
    | u >= 0 && u < length down = Just $ W.Stack f (reverse c++[head d]++up) (tail d)
        where lu = length up
              (b, a) = splitAt (lu - i - 1) up
              u = i - lu - 1
              (c, d) = splitAt u down

swapWithZ _ (Just s) = Just s

swapWith :: Int -> ModifySpec
swapWith = onFocused . swapWithZ

-- insert the focused window at the i'th position
insertAtZ :: Int -> Zipper a -> Zipper a
insertAtZ _ Nothing = Nothing
insertAtZ i (Just s@(W.Stack f up down))
    | i < 0 = Nothing
    | i < length up = let (bf, af) = splitAt i (reverse up) in Just $ W.Stack f (reverse bf) (af++down)
    | i == length up = Just s
    | otherwise = let (bf, af) = splitAt (i-(length up)) down in Just $ W.Stack f ((reverse bf) ++ up) af

insertAt :: Int -> ModifySpec
insertAt = onFocused . insertAtZ

swapWithLastZ :: Zipper a -> Zipper a
swapWithLastZ Nothing = Nothing
swapWithLastZ (Just (W.Stack f up down)) 
    | not $ null down = Just $ W.Stack f ((reverse $ init down) ++ (last down : up)) []
swapWithLastZ (Just s) = Just s

swapWithLast :: ModifySpec
swapWithLast = onFocused swapWithLastZ

-- | Swap the focused window with the (group's) master
-- window.
swapMaster :: ModifySpec
swapMaster = onFocused swapMasterZ

-- | Swap the focused group with the previous one.
swapGroupUp :: ModifySpec
swapGroupUp _ = swapUpZ

-- | Swap the focused group with the next one.
swapGroupDown :: ModifySpec
swapGroupDown _ = swapDownZ

-- | Swap the focused group with the master group.
swapGroupMaster :: ModifySpec
swapGroupMaster _ = swapMasterZ

-- | Move focus to the previous window in the group.
focusUp :: ModifySpec
focusUp = onFocused focusUpZ

-- | Move focus to the next window in the group.
focusDown :: ModifySpec
focusDown = onFocused focusDownZ

-- | Move focus to the group's master window.
focusMaster :: ModifySpec
focusMaster = onFocused focusMasterZ

-- | Move focus to the last window
focusLastZ :: Zipper a -> Zipper a
focusLastZ Nothing = Nothing
focusLastZ (Just (W.Stack f up down)) 
    | not $ null down
        = Just $ W.Stack (last down) (reverse (init down) ++ [f] ++ up) []
focusLastZ (Just s) = Just s

focusLast :: ModifySpec
focusLast = onFocused focusLastZ

-- | Move focus to the i'th element (from user's perspective)
focusAtZ :: Int -> Zipper a -> Zipper a
focusAtZ _ Nothing = Nothing
focusAtZ i (Just s@(W.Stack f up down)) 
    | i < 0 || i >= length ls = Nothing
    | not $ null a = Just $ W.Stack (head a) (reverse b) (tail a)
        where (b, a) = splitAt i $ (reverse up) ++ [f] ++ down
              ls = W.integrate s
focusAtZ _ (Just s) = Just s

focusAt :: Int -> ModifySpec
focusAt = onFocused . focusAtZ

-- | Move focus to the previous group.
focusGroupUp :: ModifySpec
focusGroupUp _ = focusUpZ

-- | Move focus to the next group.
focusGroupDown :: ModifySpec
focusGroupDown _ = focusDownZ

-- | Move focus to the master group.
focusGroupMaster :: ModifySpec
focusGroupMaster _ = focusMasterZ

-- | Move focus to the n'th group
focusGroupAt :: Int -> ModifySpec
focusGroupAt i _ = focusAtZ i

-- | Swap the current group with the i'th group
swapGroupWith :: Int -> ModifySpec
swapGroupWith i _ = swapWithZ i

-- | helper
_removeFocused :: W.Stack a -> (a, Zipper a)
_removeFocused (W.Stack f up (d:down)) = (f, Just $ W.Stack d up down)
_removeFocused (W.Stack f (u:up) []) = (f, Just $ W.Stack u up [])
_removeFocused (W.Stack f [] []) = (f, Nothing)

-- helper
_moveToNewGroup :: WithID l Window -> W.Stack (Group l Window)
                -> (Group l Window -> Zipper (Group l Window)
                                   -> Zipper (Group l Window))
                -> Zipper (Group l Window)
_moveToNewGroup l0 s insertX | G l (Just f) <- W.focus s
    = let (w, f') = _removeFocused f
          s' = s { W.focus = G l f' }
      in insertX (G l0 $ singletonZ w) $ Just s'
_moveToNewGroup _ s _ = Just s

-- | Move the focused window to a new group before the current one.
moveToNewGroupUp :: ModifySpec
moveToNewGroupUp _ Nothing = Nothing
moveToNewGroupUp l0 (Just s) = _moveToNewGroup l0 s insertUpZ

-- | Move the focused window to a new group after the current one.
moveToNewGroupDown :: ModifySpec
moveToNewGroupDown _ Nothing = Nothing
moveToNewGroupDown l0 (Just s) = _moveToNewGroup l0 s insertDownZ

removeWindows wins (G l Nothing) = G l Nothing
removeWindows wins (G l (Just s)) 
    = G l $ W.filter (not . (`elem` wins)) s

moveWindowsToNewGroup :: T.Direction1D -> (Maybe (W.Stack Window)) -> ModifySpec
moveWindowsToNewGroup _ Nothing _ s = s
moveWindowsToNewGroup dir ws l0 s@(Just (W.Stack f up down)) =
    -- first take all the windows out of the stack
    let f' = removeWindows (W.integrate' ws) f
    in case (f', dir) of
            (G _ (Just _), T.Prev) -> insertUpZ (G l0 ws) $ Just $ W.Stack f' up down
            (G _ (Just _), T.Next) -> insertDownZ (G l0 ws) $ Just $ W.Stack f' up down
            _ -> s

moveWindowsToNewGroupUp = moveWindowsToNewGroup T.Prev
moveWindowsToNewGroupDown = moveWindowsToNewGroup T.Next

moveWindows :: T.Direction1D -> Bool -> (Maybe (W.Stack Window)) -> ModifySpec
moveWindows _ _ Nothing _ s = s
moveWindows _ True _ _ s@(Just (W.Stack f [] [])) = s
moveWindows dir wrap ws@(Just (W.Stack wf wu wd)) l0 (Just s@(W.Stack f up down)) = 
    -- first get the particular group to move the windows to 
    let f' = removeWindows (W.integrate' ws) f
        fl = case f' of
                  G _ Nothing -> []
                  _ -> [f']
        insertG (G l (Just (W.Stack f u d))) = G l (Just (W.Stack wf (wu++u) (wd++[f]++d)))
        insertG x = x
    in case (up, down, dir) of
        ([], _, T.Prev) | wrap -> Just $ W.Stack (insertG $ last down) ((reverse $ init down) ++ fl) []
                        | otherwise -> Just $ W.Stack (G l0 ws) [] (fl++down)
        (uh:ul, _, T.Prev) -> Just $ W.Stack (insertG uh) ul (fl++down)
        (_, [], T.Next) | wrap -> Just $ W.Stack (insertG $ last up) [] ((reverse $ init up) ++ fl)
                        | otherwise -> Just $ W.Stack (G l0 ws) (fl++up) []
        (_, dh:dl, T.Next) -> Just $ W.Stack (insertG dh) (fl++up) dl
        _ -> Just s

moveWindowsUp = moveWindows T.Prev
moveWindowsDown = moveWindows T.Next
-- | Move the focused window to the previous group.
-- If 'True', when in the first group, wrap around to the last one.
-- If 'False', create a new group before it.
moveToGroupUp :: Bool -> ModifySpec
moveToGroupUp _ _ Nothing = Nothing
moveToGroupUp False l0 (Just s) = if null (W.up s) then moveToNewGroupUp l0 (Just s)
                                                   else moveToGroupUp True l0 (Just s)
moveToGroupUp True _ (Just s@(W.Stack _ [] [])) = Just s
moveToGroupUp True _ (Just s@(W.Stack (G l (Just f)) _ _))
    = let (w, f') = _removeFocused f
      in onFocusedZ (onZipper $ insertUpZ w) $ focusUpZ $ Just s { W.focus = G l f' }
moveToGroupUp True _ gs = gs

-- flatten a stack of groups into a stack of windows
flatten (Just (W.Stack (G _ (Just (W.Stack f u d))) up down)) = Just $ W.Stack f (u ++ concatMap pg up) (d ++ concatMap pg down)
    where pg (G _ s) = W.integrate' s
flatten _ = Nothing

-- | Move to a specific group
moveToGroupAt :: Int -> ModifySpec
moveToGroupAt i l0 (Just s@(W.Stack (G l (Just f)) up down)) = 
    -- inspect the group first to see whether the designated group is available
        let (w, f') = _removeFocused f
            gs = W.integrate s
        in if length gs > i then onFocusedZ (onZipper $ insertUpZ w) $ focusAtZ i $ Just s { W.focus = G l f' }
           else if length gs == i then insertDownZ (G l0 $ singletonZ w) $ focusAtZ (i-1) $ Just s { W.focus = G l f' }
           else Just s

moveWindowsToGroupAt _ Nothing _ s = s
moveWindowsToGroupAt _ _ _ s@(Just (W.Stack f [] [])) = s
moveWindowsToGroupAt i ws@(Just (W.Stack wf wu wd)) l0 (Just s@(W.Stack f up down)) = 
    -- first get the particular group to move the windows to 
    let f' = removeWindows (W.integrate' ws) f
        gs = reverse up ++ [f'] ++ down
        insertG (G l (Just (W.Stack f u d))) = G l (Just (W.Stack wf (wu++u) (wd++[f]++d)))
        insertG x = x
    in if i >= 0 && i < length gs && i /= length up
          then let (bf, toi:af) = splitAt i gs
               in Just $ W.Stack (insertG toi) (reverse bf) af
          else Just s

-- | Move the focused window to the next group.
-- If 'True', when in the last group, wrap around to the first one.
-- If 'False', create a new group after it.
moveToGroupDown :: Bool -> ModifySpec
moveToGroupDown _ _ Nothing = Nothing
moveToGroupDown False l0 (Just s) = if null (W.down s) then moveToNewGroupDown l0 (Just s)
                                                       else moveToGroupDown True l0 (Just s)
moveToGroupDown True _ (Just s@(W.Stack _ [] [])) = Just s
moveToGroupDown True _ (Just s@(W.Stack (G l (Just f)) _ _))
    = let (w, f') = _removeFocused f
      in onFocusedZ (onZipper $ insertUpZ w) $ focusDownZ $ Just s { W.focus = G l f' }
moveToGroupDown True _ gs = gs

-- | Split the focused group into two at the position of the focused window (below it,
-- unless it's the last window - in that case, above it).
splitGroup :: ModifySpec
splitGroup _ Nothing = Nothing
splitGroup l0 z@(Just s) | G l (Just ws) <- W.focus s
    = case ws of
        W.Stack _ [] [] -> z
        W.Stack f (u:up) [] -> let g1 = G l  $ Just $ W.Stack f [] []
                                   g2 = G l0 $ Just $ W.Stack u up []
                               in insertDownZ g1 $ onFocusedZ (const g2) z
        W.Stack f up (d:down) -> let g1 = G l  $ Just $ W.Stack f up []
                                     g2 = G l0 $ Just $ W.Stack d [] down
                                 in insertUpZ g1 $ onFocusedZ (const g2) z
splitGroup _ _ = Nothing

-- sort the windows in the focused group
sortWindows :: (Window -> String) -> ModifySpec
sortWindows fun _ (Just s@(W.Stack (G l (Just f)) _ _))
    = let wins = sortBy (\a b -> compare (fun a) (fun b)) $ W.integrate f
          fw = W.focus f
          st = case break (==fw) wins of
                    (bs, nf:as) -> Just $ W.Stack nf (reverse bs) as
                    (bs, _) -> W.differentiate bs
      in Just s { W.focus = G l st }
sortWindows _ _ gs = gs

collapse :: ModifySpec
collapse _ (Just (W.Stack (G l (Just (W.Stack f u d))) up down)) = Just $ W.Stack (G l $ Just $ W.Stack f (u ++ (reverse $ flatten up)) (d ++ flatten down)) [] []
    where flatten = concatMap (W.integrate' . gZipper) 
collapse _ _ = Nothing
