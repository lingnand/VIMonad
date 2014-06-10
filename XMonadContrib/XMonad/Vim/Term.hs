module XMonad.Vim.Term
    ( 
      isTerm
    , isTermWithCmd
    , isRecyclableTerm
    , termFullCmd
    , runTerm
    , runShell
    , uniqueTermHasContext
    , uniqueTermHasContexts
    ) where

import XMonad
import qualified Data.Set as S
import Data.List
import System.Posix.User
import XMonad.Hooks.XPropManage
import XMonad.Util.Run
import Text.Read
import XMonad.Vim.WorkspaceDirectories
import XMonad.Vim.Constants

-- a relaxed version of uniqueTermHasCmd that also includes terminals that are not instances of UniqueTerm
isTerm = className =? "XTerm" 
hasCmd c = fmap (any (c `isInfixOf`)) $ getQuery wM_COMMAND
isTermWithCmd c = isTerm <&&> hasCmd c
-- query bool definition for recyclable terminals
isRecyclableTerm = className =? "XTerm" <&&> appName =? "xterm" <&&> ( title =? "zsh -i" <||> do
    mach <- stringProperty "WM_CLIENT_MACHINE" 
    user <- io $ getLoginName
    fmap (isPrefixOf $ user++"@"++mach++":") title )
-- note that args, cmd are NOT quoted; this is for maximum flexibility - you can then use parameter expansion, etc. if you'd like
termFullCmd title name args cmd = myTerminal++prop "title" title++prop "name" name++" "++args++" -e "++cmd
    where prop s v = if null v then "" else " -"++s++" " ++escapeQuery v
-- runTerm will ALWAYS start the program with the current directory
runTerm title appname cmd = setCurrentWorkspaceDirectory >> spawn (termFullCmd title appname "" cmd)
runShell cmd = setCurrentWorkspaceDirectory >> spawn cmd


-- we give up on a dynamically typed UniqueTerm (depending on the type of Context) because it might be easy to query for command only if you don't know what type of the context is
data UniqueTerm = UniqueTerm { context :: S.Set String
                             , command :: String 
                             , args    :: String
                             } deriving (Show, Read, Eq)

uniqueTerm cons = UniqueTerm (S.fromList cons)

uniqueTermAppName :: UniqueTerm -> String
uniqueTermAppName = show

uniqueTermFullCmd ut = termFullCmd (command ut) (uniqueTermAppName ut) (args ut) (command ut)

-- return the uniqueTerm from the specified app name
toUniqueTerm :: String -> Maybe UniqueTerm
toUniqueTerm = readMaybe 

-- useful queries
uniqueTermContext = fmap (fmap context . toUniqueTerm) appName 
uniqueTermCmd = fmap (fmap command . toUniqueTerm) appName 

uniqueTermHasContext con = fmap (maybe False (S.member con)) uniqueTermContext 
uniqueTermHasContexts cons = fmap (maybe False (== cons)) uniqueTermContext
uniqueTermHasCmd cmd = fmap (maybe False (== cmd)) uniqueTermCmd

isUniqueTerm ut =  appName =? (uniqueTermAppName ut)

runUniqueTerm ut = setCurrentWorkspaceDirectory >> spawn (uniqueTermFullCmd ut)
