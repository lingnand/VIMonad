module XMonad.Vim.Prompt.Workspace
    (
      workspacePrompt
    , renameWorkspacePrompt
    , newWorkspacePrompt
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Vim.Workspaces
import Data.List
import XMonad.Vim.Tag
import XMonad.Util.Run
import XMonad.Vim.Prompt.DynamicPrompt
import XMonad.Vim.WorkspaceDirectories
import XMonad.Hooks.DynamicLog
import Data.List.Split

-- the workspace prompt works by first returning all the completions for the current workspace; if there are no suitable completions, it automatically gives back results from "symtag print '%t' <tag>"
-- upon successfully creating a workspace, it will set the workspaceDirectory using the path given by "symtag print '%p' <tag> | head -n 1"
data WorkspacePrompt = WorkspacePrompt String

instance XPrompt WorkspacePrompt where
    showXPrompt (WorkspacePrompt s) = s ++ ": "
    commandToComplete _ = id
    nextCompletion _ c l = if null l then "" else l !! case c `elemIndex` l of
                                                       Just i -> if i >= length l - 1 then 0 else i + 1
                                                       Nothing -> 0

workspacePrompt :: XPConfig -> String -> (WorkspaceId -> String -> X ()) -> (String -> String -> X ()) -> X ()
workspacePrompt conf p ef f = do 
    wts <- allWorkspaceTags
    wns <- fmap (fmap (\(a,b)->if null b then a else a++":"++b) . zip wts) allWorkspaceNames
    tagBin <- io getTagBin
    let complFun s = if null extWns 
               then fmap lines $ runProcessWithInput tagBin ([show tagLimit, "false", "-type", "d"] ++ tagQuery (words s)) ""
               else return extWns
               where fil = filter (searchPredicate conf s)
                     extWns = fil wns 
    mkXPrompt (WorkspacePrompt p) conf complFun $ \s -> do
        -- if the string is one of the existing tags
        let (t, n) = break (== ':') s
            nm =  if null n then n else tail n
        if t `elem` wts 
           then do
               -- the tag is found and it is one of the existing workspace
               -- do something with the name
               ef t nm
           else do
            -- set the workspaceDirectory
            -- output <- runProcessWithInput "symtag" ("print" : if '/' `elem` s then ["true", "%p", "1", ".*"++s] else ["false", "%p", "1", ".*"++s++".*"]) ""
            ls <- fmap lines $ runProcessWithInput tagBin [show tagLimit, "true", s] ""
            let path = if null s || null ls then "" else trim $ head $ ls
                name = if null ltag then s 
                                    else if ' ' `elem` ltag then fmap head $ filter (not . null) $ splitOn " " ltag
                                    else if '_' `elem` tltag then fmap head $ filter (not . null) $ splitOn "_" ltag
                                    else ltag
                                    where ltag = last $ splitOn "/" path
                                          tltag = tail ltag
            -- run the specified script under the directory
            if not $ null path then spawn $ path ++ "/.ws_start" else return ()
            -- we will cut the string of any slash (shortened tag form)
            -- do something with the name
            f name path

newWorkspacePrompt conf prompt insp iter f = workspacePrompt conf prompt (\t n -> f t) (\n p -> insertWorkspace n insp iter p f)

---- rename the current workspace
-- for the time being because we want to allow the user some freedom of actually renaming the workspace after assigning a workspace directory to it, so what we do is that we'll only reassign the directory when necessary
renameWorkspacePrompt conf immi final = workspacePrompt conf "Rename workspace" (\t n -> setCurrentWorkspaceName n) (\n p -> do
    if not (null p) then saveCurrentWorkspaceDirectory p
                    else return ()
    setCurrentWorkspaceName n
    immi
    final) 

