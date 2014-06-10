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
import XMonad.Hooks.DynamicLog (wrap)

data WindowSearchPrompt = WindowSearchPrompt String
instance XPrompt WindowSearchPrompt where
    showXPrompt (WindowSearchPrompt p) = p
    commandToComplete _ c = c
    nextCompletion _ = getNextCompletion


mkSearchPrompt config prompt predicate a = do
    -- we'd probably just get all the workspaces and then the windows inside
    wss <- allWorkspaces
    curr <- gets (W.workspace . W.current . windowset)
    (l, r, gf, ml, float, _, _) <- getSides
    names <- getWorkspaceNames
    -- get the last window
    lm <- nextMatched History (return True)
    let wswins = W.integrate' . W.stack
        -- remove duplicate windows
        cwins = nub $ l++gf++r++float++ml
        winfo :: Window -> X (([String], String), Window)
        winfo w = do
            t <- runQuery title w
            tags'' <- getTags w
            let tags = sortRegs $ if Just w == lm then tags'++["'"] else tags'
                tags' = case tags'' of
                         [] | w `elem` ml -> ["*"]
                            | otherwise -> []
                         ts -> ts
            return ((tags, t), w)
        -- we must make sure there aren't duplicated entries
        wp ws = fmap (concatMap (fmap (\(s, ((a,t,b), w)) -> ((a,t,if s == "" then b else b++" ["++s++"]"), w)) . zip ([""] ++ fmap show [1..])) . groupBy (\(a,_) (b,_) -> a == b) . fmap (attag (W.tag ws))) $ mapM winfo $ wswins ws
        wr = fmap concat . mapM wp
        attag t ((a,b),w) = ((a,names t,b),w)
    (winfos, winls) <- case break ((==(W.tag curr)) . W.tag) wss of
         (bf, c:af) -> do 
                bis <- wr bf
                ais <- wr af
                cis <- wp curr
                return $ unzip $ bis ++ cis ++ ais
         (bf, _) -> fmap unzip $ wr bf
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

