{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Util.WorkspaceDirectories
    ( getWorkspaceDirectory
    , removeWorkspaceDirectory
    , saveWorkspaceDirectory
    , getCurrentWorkspaceDirectory
    , saveCurrentWorkspaceDirectory
    ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M
import XMonad
-- this module uses WorkspaceHandles to ensure consistency
import XMonad.Util.WorkspaceHandles
import qualified XMonad.StackSet as W

-- the default directory is the empty directory; in bash that translates to the home directory just fine
defaultDirectory = ""

newtype WorkspaceDirectories = WorkspaceDirectories (M.Map String String)
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceDirectories where
    initialValue = WorkspaceDirectories M.empty
    extensionType = PersistentExtension

getWorkspaceDirectory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    case M.lookup h m of
        Nothing -> do
            -- need to insert an initial directory into the database
            XS.put $ WorkspaceDirectories $ M.insert h defaultDirectory m 
            return defaultDirectory
        Just s  -> return s

getCurrentWorkspaceDirectory = gets (W.currentTag . windowset) >>= getWorkspaceDirectory

saveCurrentWorkspaceDirectory dir = gets (W.currentTag . windowset) >>= saveWorkspaceDirectory dir

removeWorkspaceDirectory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    XS.put $ WorkspaceDirectories $ M.delete h m 

saveWorkspaceDirectory directory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    XS.put $ WorkspaceDirectories $ M.insert h directory m 

