{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Util.WorkspaceDirectories
    ( getWorkspaceDirectory
    , getWorkspaceOldDirectory
    , removeWorkspaceDirectory
    , saveWorkspaceDirectory
    , getCurrentWorkspaceDirectory
    , getCurrentWorkspaceOldDirectory
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

newtype WorkspaceDirectories = WorkspaceDirectories (M.Map String (String, String))
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceDirectories where
    initialValue = WorkspaceDirectories M.empty
    extensionType = PersistentExtension

getWorkspaceDirectory tag = fmap fst $ getWorkspaceDir tag 

getWorkspaceDir tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    case M.lookup h m of
        Nothing -> do
            -- need to insert an initial directory into the database
            XS.put $ WorkspaceDirectories $ M.insert h (defaultDirectory, defaultDirectory) m 
            return (defaultDirectory, defaultDirectory)
        Just s -> return s

getWorkspaceOldDirectory tag = fmap snd $ getWorkspaceDir tag

getCurrentWorkspaceDirectory = gets (W.currentTag . windowset) >>= getWorkspaceDirectory
getCurrentWorkspaceOldDirectory = gets (W.currentTag . windowset) >>= getWorkspaceOldDirectory

saveCurrentWorkspaceDirectory dir = gets (W.currentTag . windowset) >>= saveWorkspaceDirectory dir

removeWorkspaceDirectory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    XS.put $ WorkspaceDirectories $ M.delete h m 

saveWorkspaceDirectory directory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    XS.put $ WorkspaceDirectories $ M.insert h (directory, maybe defaultDirectory fst $ M.lookup h m) m

