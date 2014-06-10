module XMonad.Vim.Tag
    (
      getTagBin
    , tagLibroot
    , tagQuery
    ) where

import XMonad.Vim.Constants
import XMonad.Vim.Routine

getTagBin = getMyScriptsDir >>= return . (++"/xtag")
tagLibroot = "~/DB/"
tagQuery = prepend "-iwholename" . fmap (("*"++).(++"*"))
