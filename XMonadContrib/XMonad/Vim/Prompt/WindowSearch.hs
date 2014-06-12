module XMonad.Vim.Prompt.WindowSearch
    ( 
      mkSearchPrompt
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Actions.GroupNavigation
import XMonad.Vim.Workspaces
import qualified XMonad.StackSet as W
import Data.List
import XMonad.Actions.TagWindows
import XMonad.Vim.Routine
import XMonad.Vim.Minimize
import XMonad.Hooks.DynamicLog (wrap)

data WindowSearchPrompt = WindowSearchPrompt String
instance XPrompt WindowSearchPrompt where
    showXPrompt (WindowSearchPrompt p) = p
    commandToComplete _ c = c
    nextCompletion _ = getNextCompletion


mkSearchPrompt config prompt predicate a = do
    -- we'd probably just get all the workspaces and then the windows inside
    names <- getWorkspaceNames
    ml <- getMinimizedWindows
    wss' <- fmap (concatMap $ \s -> zip (repeat $ names $ W.tag s) $ W.integrate' $ W.stack s) allWorkspaces
    let wss = wss' ++ zip (repeat "hidden") ml
    -- get the last window
    lm <- nextMatched History (return True)
    let winfo :: (String, Window) -> X (([String], String, String), Window)
        winfo (wn, w) = do
            t <- runQuery title w
            tags'' <- getTags w
            let tags = sortRegs $ if Just w == lm then tags'++["'"] else tags'
                tags' = case tags'' of
                         [] | w `elem` ml -> ["*"]
                            | otherwise -> []
                         ts -> ts
            return ((tags, wn, t), w)
        deduplicate = concatMap (fmap (\(s, ((a,t,b), w)) -> ((a,t,if s == "" then b else b++" ["++s++"]"), w)) . zip ([""] ++ fmap show [1..])) . groupBy (\(a,_) (b,_) -> a == b)
    (winfos, winls) <- fmap (unzip . deduplicate) $ mapM winfo wss
    let fillsp ls = let l = maximum $ fmap length ls
                        fsp s = (take (l - length s) $ repeat ' ')++s
                    in fmap fsp ls
        (is, wins) = unzip $ flip sortBy (zip winfos winls) $ \((a,b,c),_) ((d,e,f),_) -> 
            case (compareLists' compareRegs a d, compareSymbolString b e, compare c f) of
                 (EQ, EQ, z) -> z
                 (EQ, y, _) -> y
                 (x, _, _) -> x
        (regs', tags', tits) = unzip3 is
        regsToStr regs = (if null regs then id else wrap "[" "]") $ concatMap ("'"++) regs
        finfos = fmap (\(a,b,c)-> a++" ["++b++"]: "++c) $ zip3 (fillsp (fmap regsToStr regs')) (fillsp tags') tits
    -- filter the windows according to p
    mkXPrompt (WindowSearchPrompt prompt) config (\s -> return $ filter (searchPredicate config s) finfos) $ \r ->
        whenJust (fmap (wins !!) $ elemIndex r finfos) $ a

