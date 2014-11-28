{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicBars
-- Copyright   :  (c) Ben Boeckel 2012
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Manage per-screen status bars.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicBars (
  -- * Usage
  -- $usage
    DynamicStatusBar
  , DynamicStatusBarCleanup
  , dynStatusBarStartup
  , dynStatusBarEventHook
  , multiPP
  ) where

import Prelude

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)

import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrandr

import System.IO

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- Provides a few helper functions to manage per-screen status bars while
-- dynamically responding to screen changes. A startup action, event hook, and
-- a way to separate PP styles based on the screen's focus are provided:
--
-- * The 'dynStatusBarStartup' hook which initializes the status bars.
--
-- * The 'dynStatusBarEventHook' hook which respawns status bars when the
-- number of screens changes.
--
-- * The 'multiPP' function which allows for different output based on whether
-- the screen for the status bar has focus.
--
-- The hooks take a 'DynamicStatusBar' function which is given the id of the
-- screen to start up and returns the 'Handle' to the pipe to write to. The
-- 'DynamicStatusBarCleanup' argument should tear down previous instances. It
-- is called when the number of screens changes and on startup.
--

instance Eq XineramaScreenInfo where
    (==) (XineramaScreenInfo a ax ay aw ah) (XineramaScreenInfo b bx by bw bh) = a == b && ax == bx && ay == by && aw == bw && ah == bh

data DynStatusBarInfo = DynStatusBarInfo
  { dsbInfoScreens :: [XineramaScreenInfo]
  , dsbInfoHandles :: [Handle]
  } deriving (Typeable)

instance ExtensionClass DynStatusBarInfo where
  initialValue = DynStatusBarInfo [] []

type DynamicStatusBar = XineramaScreenInfo -> IO Handle
type DynamicStatusBarCleanup = IO ()

dynStatusBarStartup :: DynamicStatusBar -> DynamicStatusBarCleanup -> X () -> X ()
dynStatusBarStartup sb cleanup x = do
  liftIO $ do
      dpy <- openDisplay ""
      xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
      closeDisplay dpy
  updateStatusBars sb cleanup x

-- the added X () is triggered when actually the event fires
dynStatusBarEventHook :: DynamicStatusBar -> DynamicStatusBarCleanup -> X () -> Event -> X All
dynStatusBarEventHook sb cleanup x (RRScreenChangeNotifyEvent {}) = updateStatusBars sb cleanup x >> return (All True)
dynStatusBarEventHook _  _       _ _                              = return (All True)

updateStatusBars :: DynamicStatusBar -> DynamicStatusBarCleanup -> X () -> X ()
updateStatusBars sb cleanup x = do
  dsbInfo <- XS.get
  screens <- getScreens
  when (screens /= dsbInfoScreens dsbInfo) $ do
      newHandles <- liftIO $ do
          hClose `mapM_` dsbInfoHandles dsbInfo
          cleanup
          mapM sb screens
      XS.put $ DynStatusBarInfo screens newHandles
      x

-----------------------------------------------------------------------------
-- The following code is from adamvo's xmonad.hs file.
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo%27s_xmonad.hs

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> X ()
multiPP focusPP unfocusPP = do
  dsbInfo <- XS.get
  multiPP' dynamicLogString focusPP unfocusPP (dsbInfoHandles dsbInfo)

multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
  st <- get
  let pickPP :: WorkspaceId -> WriterT (Last XState) X String
      pickPP ws = do
        let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset st
        put st{ windowset = W.view ws $ windowset st }
        out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
        when isFoc $ get >>= tell . Last . Just
        return out
  traverse put . getLast
    =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
    =<< mapM screenWorkspace (zipWith const [0 .. ] handles)
  return ()

-- we need a proper way of getting screen ids CORRECTLY
getScreens :: MonadIO m => m [XineramaScreenInfo]
getScreens = liftIO $ do
    dpy <- openDisplay ""
    msl <- xineramaQueryScreens dpy
    closeDisplay dpy
    return $ fromMaybe [] msl
