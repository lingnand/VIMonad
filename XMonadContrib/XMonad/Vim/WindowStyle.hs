{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.WindowStyle
    (
      windowStyleFromList
    , lowerHalfRectHook
    , upperHalfRectHook
    , rightPanelHook
    , leftPanelHook
    , runManageHookOnFocused
    ) where

import XMonad
import XMonad.Util.Types
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import Data.Monoid (appEndo)
import Control.Monad

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

runManageHook :: ManageHook -> Window -> X ()
runManageHook mh = windows . appEndo <=< runQuery mh

runManageHookOnFocused = withFocused . runManageHook

