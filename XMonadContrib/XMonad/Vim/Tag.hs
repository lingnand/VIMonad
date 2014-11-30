module XMonad.Vim.Tag
    (
      getTagBin
    , tagLibroot
    , tagQuery
    ) where

import XMonad.Vim.Constants
import XMonad.Vim.Routine
import Control.Monad.Trans

getTagBin :: MonadIO m => m String
getTagBin = getScriptsDir >>= return . (++"/xtag")
tagLibroot = "~/DB/"
tagQuery = prepend "-iwholename" . fmap (("*"++).(++"*"))
