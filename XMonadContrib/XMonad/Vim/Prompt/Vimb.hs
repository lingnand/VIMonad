module XMonad.Vim.Prompt.Vimb
    (
      VBPrompt(..)
    , mkVBPrompt
    , vbAction
    , vbComplFunc
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run
import Data.List
import XMonad.Vim.Term
import XMonad.Hooks.DynamicLog

data VBPrompt = VBPrompt
vbhistorySize = 10
vbMatchSize = 10

vbNextCompletion (cmd,_) ls = (c', length c')
        where c' = last . words $ ls !! ni
              ni = case findIndex (vbIsEqualToCompletion cmd) ls of
                      Just i -> if i >= length ls - 1 then 0 else i+1
                      Nothing -> 0 
vbHighlightPredicate cl (c,_) = vbIsEqualToCompletion c cl
vbAction s = runShell $ "vb " ++ escapeQuery s
----- if it's empty then return the top 10 results from history
vbComplFunc s
    | null $ trim s = do fmap lines $ runProcessWithInput "vbhistory" [(show vbhistorySize)] "" 
    | otherwise = do fmap lines $ runProcessWithInput "vbopenurls" (["-n",(show vbMatchSize)]++(words s)) "" 


instance XPrompt VBPrompt where
    showXPrompt _ = "vimb > "
    commandToComplete _ = id
    nextCompletion _ = vbNextCompletion
    highlightPredicate _ = vbHighlightPredicate

vbIsEqualToCompletion cmd cl = cmd == (last $ words cl)

mkVBPrompt c = mkXPrompt VBPrompt c vbComplFunc vbAction
