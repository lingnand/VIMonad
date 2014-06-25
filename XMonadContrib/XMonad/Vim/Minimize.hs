{-# LANGUAGE DeriveDataTypeable #-}
-- providing a consistent minimize interface (showing register and minimize content across all workspaces)
module XMonad.Vim.Minimize
    ( 
      minimizeWindows
    , minimizeWindow
    , getMinimizedWindows
    , removeMinimizedState
    , deminimizeWindows
    , deminimizeWindow
    , isMinimized
    ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import XMonad.Operations
import XMonad.ManageHook
import Data.List

data MinimizedWindows = MinimizedWindows [Window] deriving (Typeable, Show, Read)
instance ExtensionClass MinimizedWindows where
    initialValue = MinimizedWindows []
    extensionType = PersistentExtension

-- save the list of windows and set them as minimized status
minimizeWindows ls = do
    mapM unmanage ls
    MinimizedWindows ol <- XS.get
    XS.put $ MinimizedWindows $ ol ++ ls
    -- sink all the windows if necessary
    windows $ \s -> foldr W.sink s ls

minimizeWindow w = minimizeWindows [w]

removeMinimizedState ls = do
    MinimizedWindows ol <- XS.get
    let (inl, outl) = partition (`elem` ls) ol
    XS.put $ MinimizedWindows outl
    return inl

deminimizeWindows ls = do
    removeMinimizedState ls >>= mapM manage

deminimizeWindow w = deminimizeWindows [w]

getMinimizedWindows = do
    MinimizedWindows l <- XS.get
    return l

isMinimized = ask >>= \w -> liftX $ do
    mwins <- getMinimizedWindows
    return $ w `elem` mwins
