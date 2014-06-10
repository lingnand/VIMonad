{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Hooks.OnWindowsInserted 
    ( onWindowsInsertedManageHook
    , onWindowsInsertedLogHook
    , doAfterWindowsInserted
    , applyOnWindowsInserted
    , onWindowsInserted
    , OnWindowsInserted (..)
    ) where
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import Data.Maybe
import Data.Monoid
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad

data OnWindowsInserted = OnWindowsInserted {
          numberOfWindows :: Int
        , manageBefore :: Maybe Window -> Window -> X ()
        , logSuccessive :: Maybe Window -> Window -> X ()
        , logFinished :: Maybe Window -> [Window] -> X ()
    }

instance Default OnWindowsInserted where
    def = OnWindowsInserted {
          numberOfWindows = 0
        , manageBefore = \_ _ -> return ()
        , logSuccessive = \_ _ -> return ()
        , logFinished = \_ _ -> return ()
    }

instance Show OnWindowsInserted where
    show (OnWindowsInserted i a b c) = show i ++ " windows left to insert"

onWindowsInserted n gmh ls lf = do
    XS.put $ WindowsInserted (Nothing, [], False)
    XS.put $ LastLoggedWindow Nothing
    XS.put $ OnWindowsInsertedStorage $ Just $ OnWindowsInserted n gmh ls lf

applyOnWindowsInserted (OnWindowsInserted n gmh ls lf) = onWindowsInserted n gmh ls lf

doAfterWindowsInserted n f = onWindowsInserted n (\_ _ -> return ()) (\_ _ -> return ()) (\_ _ -> f)

data OnWindowsInsertedStorage = OnWindowsInsertedStorage (Maybe OnWindowsInserted) deriving (Show, Typeable)
instance ExtensionClass OnWindowsInsertedStorage where
    initialValue = OnWindowsInsertedStorage Nothing
-- this records the previously focused window as well as the new windows that is going to be created
data WindowsInserted = WindowsInserted (Maybe Window, [Window], Bool) deriving (Show, Read, Typeable)
instance ExtensionClass WindowsInserted where
    initialValue = WindowsInserted (Nothing, [], False)

data LastLoggedWindow = LastLoggedWindow (Maybe Window) deriving (Show, Read, Typeable)
instance ExtensionClass LastLoggedWindow where
    initialValue = LastLoggedWindow Nothing

onWindowsInsertedLogHook = do
    WindowsInserted mw <- XS.get
    OnWindowsInsertedStorage mwis <- XS.get
    LastLoggedWindow mlw <- XS.get
    case (mw, mwis) of
         ((mf, ls, ready), Just (OnWindowsInserted i _ as a)) -> do
             if null ls then return () 
                        else if Just (head ls) /= mlw then do
                            XS.put $ LastLoggedWindow $ Just $ head ls
                            as mf (head ls)
                        else return ()
             if ready then do
                         XS.put $ WindowsInserted (Nothing, [], False)
                         XS.put $ OnWindowsInsertedStorage Nothing
                         XS.put $ LastLoggedWindow Nothing
                         a mf (take i ls)
                      else return ()
         _ -> return ()

isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
onWindowsInsertedManageHook :: ManageHook
onWindowsInsertedManageHook = 
      fmap not isDialog <&&> fmap not isSplash --> ask >>= \w -> liftX $ do
        -- check for the change list
        OnWindowsInsertedStorage mow <- XS.get
        case mow of
             Just (OnWindowsInserted i gmh _ _) | i > 0 -> do
                WindowsInserted (mpf, ls, _) <- XS.get
                mf <- gets (W.peek . windowset)
                let mpf' = if isNothing mpf then mf else mpf
                    ls' = w:ls
                    r = length ls' >= i
                gmh mpf' w
                XS.put $ WindowsInserted (mpf', ls', r)
             _ -> return ()
        mempty
