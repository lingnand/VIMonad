{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.QuickFind
    (
      saveFindFunction
    , playLastFindFunction
    ) where

import XMonad
import XMonad.Util.Types
import qualified XMonad.Util.ExtensibleState as XS

-- this module tries to simulate the f <key> behavior in vim
-- the module saves just one thing: a function that takes a dir argument and can be invoked later through m-, and m-;
data QuickFindFunction = QuickFindFunction (Direction1D -> X (Maybe (Window, Bool))) deriving Typeable
instance ExtensionClass QuickFindFunction where
    initialValue = QuickFindFunction (\d -> return Nothing)

saveFindFunction q = XS.put $ QuickFindFunction q
getFindFunction = XS.get >>= \(QuickFindFunction q) -> return q

playLastFindFunction d = getFindFunction >>= \f -> f d
