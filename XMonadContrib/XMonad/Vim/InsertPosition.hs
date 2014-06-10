module XMonad.Vim.InsertPosition
    (
      InsertPosition(..)
    , insertPositionKeys
    ) where

data InsertPosition = Before | After | Head | Last deriving Eq
instance Show InsertPosition where
    show Before = "insert before"
    show After = "insert after"
    show Head = "insert head"
    show Last = "insert last"
insertPositionKeys = [ ("i"  , Before)
                     , ("S-i", Head)
                     , ("a"  , After)
                     , ("S-a", Last) ]
