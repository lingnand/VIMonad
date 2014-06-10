{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.InsertOlder
    ( 
      insertPositionHook
    , toggleInsertOlderForce
    , toggleInsertOlderRecover
    , toggleInsertOlder
    , toggleInsertOlder'
    , putInsertOlder
    , InsertOlderToggle(..)
    ) where

import XMonad
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext(runLogHook)
import qualified XMonad.Util.ExtensibleState as XS

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

toggleInsertOlder' = do
    XS.put $ InsertOlderToggleBackUp (False, False)
    InsertOlderToggle t <- XS.get
    XS.put $ InsertOlderToggle $ not t
    return t

putInsertOlder t = do
    XS.put $ InsertOlderToggle t
    runLogHook

insertPositionHook = ask >>= \w -> do
    tog <- liftX $ XS.get >>= \(InsertOlderToggle t) -> return t
    insertPosition Above $ if tog then Older else Newer
