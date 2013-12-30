{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Util.WorkspaceHandles
    ( getWorkspaceHandle
    , swapWorkspaceHandlesByTags
    , removeWorkspaceHandleByTag
    , getCurrentWorkspaceHandle
    , WorkspaceHandles(..)
    ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad

newtype WorkspaceHandles = WorkspaceHandles (M.Map WorkspaceId String)
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceHandles where
    initialValue = WorkspaceHandles M.empty
    extensionType = PersistentExtension

newtype Sequencer = Sequencer Int deriving (Typeable, Read, Show)

instance ExtensionClass Sequencer where
    initialValue = Sequencer 0
    extensionType = PersistentExtension

getCurrentWorkspaceHandle = gets (W.currentTag . windowset) >>= getWorkspaceHandle

getWorkspaceHandle tag = do
    WorkspaceHandles m <- XS.get
    case M.lookup tag m of
        Nothing -> do
            -- need to insert the tag into the database
            -- need to make sure that the new tag is not used in the databaase
            Sequencer n <- XS.get
            XS.put $ WorkspaceHandles $ M.insert tag (show n) m 
            XS.put $ Sequencer (n+1)
            return $ show n
        Just s  -> return s

getWorkspaceHandle' tag = do
    WorkspaceHandles m <- XS.get
    return $ M.lookup tag m 

removeWorkspaceHandleByTag tag = do
    WorkspaceHandles m <- XS.get
    XS.put $ WorkspaceHandles $ M.delete tag m 

-- | Swap handles for the two workspaces.
swapWorkspaceHandlesByTags :: WorkspaceId -> WorkspaceId -> X ()
swapWorkspaceHandlesByTags w1 w2 = do
    wh1 <- getWorkspaceHandle w1
    wh2 <- getWorkspaceHandle w2
    WorkspaceHandles m <- XS.get
    XS.put $ WorkspaceHandles $ M.insert w1 wh2 $ M.insert w2 wh1 $ m

