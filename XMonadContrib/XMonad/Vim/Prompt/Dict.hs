module XMonad.Vim.Prompt.Dict
    (
      defaultSDMode
    , mobySDMode
    , modernCHSDMode
    , bigCHSDMode
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Vim.Prompt.Vimb
import XMonad.Vim.Constants

getSdcvBin = getMyScriptsDir >>= return . (++"/xsdcv")
sdLength = "250"

data StarDictMode = SDMode { prompt :: String
                           , dictName :: String
                           }

instance XPrompt StarDictMode where
    showXPrompt (SDMode p _) = p
    commandToComplete _ = id
    completionFunction (SDMode _ d) = \s -> if (length s == 0) then return [] else do
        sdcvBin <- getSdcvBin
        fmap lines $ runProcessWithInput sdcvBin [d, s, sdLength] ""
    modeAction _ query _ = vbAction $ (if length (words query) <= 1 then "wt " else "d !tr ") ++ query
    nextCompletion _ (c,_) _ =  (c, length c)
    highlightPredicate _ _ _ = False
defaultSDMode = SDMode "Collins Cobuild 5 > " "Collins Cobuild 5"
mobySDMode = SDMode  "Moby Thesaurus II > " "Moby Thesaurus II"
modernCHSDMode = SDMode "现代汉语词典 > " "Modern Chinese Dictionary"
bigCHSDMode = SDMode "汉语大词典 > " "Chinese Big Dictionary"
