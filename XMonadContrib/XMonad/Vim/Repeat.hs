{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.Repeat
    (
      saveLastCommand
    , getLastCommand
    ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

data LastCommand = LastCommand (X ()) deriving Typeable
instance ExtensionClass LastCommand where
    initialValue = LastCommand (return ())

saveLastCommand a = XS.put $ LastCommand a
getLastCommand = XS.get >>= \(LastCommand a) -> return a
