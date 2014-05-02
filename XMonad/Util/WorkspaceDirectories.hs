{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Util.WorkspaceDirectories
    ( setWorkspaceDirectory
    , getWorkspaceOldDirectory
    , removeWorkspaceDirectory
    , saveWorkspaceDirectory
    , setCurrentWorkspaceDirectory
    , getCurrentWorkspaceDirectory
    , getCurrentWorkspaceOldDirectory
    , saveCurrentWorkspaceDirectory
    , expandd
    , shortend
    ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M
import XMonad
-- this module uses WorkspaceHandles to ensure consistency
import XMonad.Util.WorkspaceHandles
import qualified XMonad.StackSet as W
import System.Directory
import Data.List

-- okay we are going to implement the complete workspace related settings HERE
-- the default directory is the empty directory; in bash that translates to the home directory just fine
defaultDirectory = io $ getHomeDirectory
correctDir home d = io (canonicalizePath (if null d then home else expandd home d)) `catchX` (return home)
shortend home s = maybe s ("~"++) $ stripPrefix home s
expandd home s = maybe s (home++) $ stripPrefix "~" s


newtype WorkspaceDirectories = WorkspaceDirectories (M.Map String (String, String))
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceDirectories where
    initialValue = WorkspaceDirectories M.empty
    extensionType = PersistentExtension

setWorkspaceDirectory tag = setWorkspaceDir tag fst

setWorkspaceDir tag f = getWorkspaceDir tag >>= io . setCurrentDirectory . f
getWorkspaceDir tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    dd <- defaultDirectory
    case M.lookup h m of
        Nothing -> do
            -- need to insert an initial directory into the database
            XS.put $ WorkspaceDirectories $ M.insert h (dd, dd) m 
            return (dd, dd)
        Just (od', cd') -> do
            -- we need to correct the directories again
            od <- correctDir dd od'
            cd <- correctDir dd cd'
            return (od, cd)

getWorkspaceOldDirectory = fmap snd . getWorkspaceDir
getWorkspaceDirectory = fmap fst . getWorkspaceDir

setCurrentWorkspaceDirectory = gets (W.currentTag . windowset) >>= setWorkspaceDirectory
getCurrentWorkspaceOldDirectory = gets (W.currentTag . windowset) >>= getWorkspaceOldDirectory
getCurrentWorkspaceDirectory = gets (W.currentTag . windowset) >>= getWorkspaceDirectory

saveCurrentWorkspaceDirectory dir = gets (W.currentTag . windowset) >>= saveWorkspaceDirectory dir

removeWorkspaceDirectory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    XS.put $ WorkspaceDirectories $ M.delete h m 

saveWorkspaceDirectory directory tag = do
    h <- getWorkspaceHandle tag
    WorkspaceDirectories m <- XS.get
    dd <- defaultDirectory
    cd <- correctDir dd directory
    XS.put $ WorkspaceDirectories $ M.insert h (cd, maybe dd fst $ M.lookup h m) m

