module XMonad.Vim.TaskGroup
    ( 
      TaskGroup(..)
    , siftTaskGroups
    , taskGroupOfWindow
    , siftedTaskGroupOfWindow
    , contextualGroup
    , filterKeyList
    ) where

import XMonad
import XMonad.Util.Types
import XMonad.Layout.Decoration
import XMonad.Vim.Routine
import Data.Char
import Data.List
import Data.List.Split
import XMonad.Vim.Workspaces
import XMonad.Vim.WindowStyle
import qualified XMonad.StackSet as W

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

instance Default TaskGroup where 
    def = TaskGroup { taskGroupName = "Unknown"
                      , filterKey = ""
                      , filterPredicate = alwaysTrue
                      , localFirst = True
                      , construct = \_ _ -> return ()
                      , launchHook = idHook
                      -- the default window styles involving
                      , windowStyle = windowStyleFromList [doSink, lowerHalfRectHook]
                      , colorScheme = def
                      }

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
