module XMonad.Layout.TiledTabs (
    tiledTabs
    , TiledTabsConfig(..)
    , defaultTiledTabsConfig
    , increaseNMasterGroups
    , decreaseNMasterGroups
    , shrinkMasterGroups
    , expandMasterGroups
    , nextOuterLayout
    ) where

import XMonad hiding ((|||))

import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers

import XMonad.Layout.ZoomRow
import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest
{-import XMonad.Layout.TabBarDecoration-}


----- Copied over tiledtabs group for own modification

-- | Configuration data for the "tiled tab groups" layout
data TiledTabsConfig s = TTC { 
            tabsShrinker :: s
          , tabsTheme :: Theme }

defaultTiledTabsConfig = TTC shrinkText defaultTheme

{-tallTabs c = G.group (_tabs c) $ _vert c ||| Full-}
{-_tabs c = named "Tabs" $ tabbedAlways (tabsShrinker c) (tabsTheme c)-}

{-tiledTabs (TTC s t) l = G.group (tabBar s t Top Simplest) l-}
tiledTabs c = G.group (_tab c _tabs)
_tab (TTC s t) = renamed [CutWordsLeft 1] . addTabs s t
_tabs = named "Tabs" Simplest

-- | Increase the number of master groups by one
increaseNMasterGroups :: X ()
increaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN 1

-- | Decrease the number of master groups by one
decreaseNMasterGroups :: X ()
decreaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN (-1)

-- | Shrink the master area
shrinkMasterGroups :: X ()
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Expand

-- | Rotate the available outer layout algorithms
nextOuterLayout :: X ()
nextOuterLayout = sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout

