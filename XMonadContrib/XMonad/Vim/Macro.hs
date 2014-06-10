{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.Macro
    (
      toggleMacroMode
    , retrieveMacro
    , retrieveMacroCommands
    , MacroModeStorage(..)
    , appendMacroKey
    ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import Control.Exception.Extensible as E
import System.IO
import System.Posix.Files
import Text.Read
import Data.List
import XMonad.Hooks.FloatNext (runLogHook)
import Text.Regex.Posix
import System.Directory
import XMonad.Vim.Routine
import XMonad.Vim.Constants
import XMonad.Prompt.Input
import XMonad.Prompt
import XMonad.Util.Run

-- requirement for macro names
-- - no number (number will be interpreted as number of times instead)
-- - no two macros shall have the same prefix (limitation of the keyboard system)
-- - it should not be "/" or anything with that prefix (it's not really like that you can make that in the file system out as well)
macrosDirWithinHomeDirectory = "/.macros"
getMacrosDir = do
    home <- getHomeDirectory
    return $ home ++ macrosDirWithinHomeDirectory
writeData file d = io $ do
  writeFile file (show d) `E.catch` \(SomeException e) ->
                          hPutStrLn stderr ("error writing data: "++show e)
  setFileMode file mode
    where mode = ownerReadMode .|. ownerWriteMode

readData file = io $ readData `E.catch` \(SomeException _) -> return Nothing
 where
    readData = do
        xs <- bracket (openFile file ReadMode) hClose hGetLine
        return $ readMaybe xs

data MacroModeStorage = MMS (Maybe (String, [String])) deriving Typeable
instance ExtensionClass MacroModeStorage where
    initialValue = MMS Nothing

appendMacroKey key = do
    MMS a <- XS.get
    case a of
         Just (n, ls) -> XS.put $ MMS $ Just (n, ls++[key])
         _ -> return ()

retrieveMacro :: [(String, X () -> X () -> X ())] -> [String] -> X () -> X ()
retrieveMacro bindingList list = foldl g id list
            where g z k a = do
                      let x = maybe (>>) snd $ find ((==k) . fst) bindingList
                      z (x (return ()) a)

toggleMacroMode name = do
    MMS m <- XS.get
    myMacrosDir <- io $ getMacrosDir
    case m of
         Just (n, a:as) -> writeData (myMacrosDir ++ "/" ++ n) (a:as)
         _ -> return ()
    case name of
         Just n -> XS.put $ MMS $ Just (n, [])
         _ -> XS.put $ MMS Nothing
    runLogHook

retrieveMacroCommands keybindings conf' = flip E.catch (\(SomeException e) -> return []) $ do
    myMacrosDir <- getMacrosDir
    fs <- fmap (filter (not . (=~ "^\\."))) $ getDirectoryContents myMacrosDir
    let allMacroNames = nubBy (\a b -> let (s, l) = if length a < length b then (a, b) else (a, b)
                                       in s `isPrefixOf` l
                              ) $ fs 
                                ++
                                fmap fst defaultMacrosMap
                                ++ 
                                fmap wrapList (alphas ++ "._-")
        allMacroKeys = zip (fmap (joinStr " " . fmap charToKeyStroke) allMacroNames) (fmap (return . Just) allMacroNames)
                       ++
                       [ ("/", do
                            r <- initMatches
                            let compl s = return $ filter ((searchPredicate conf) s) allMacroNames
                                conf = conf' r
                            inputPromptWithCompl conf "Enter the name of the macro" compl
                         )
                       ]
    return $ concatMap (processKey . addPrefix) $
        [ ("a "++nk++sk, do
            -- first try to play that as a xmonad macro
            s' <- ms
            case s' of
                 Just s -> do
                        mx <- readData (myMacrosDir ++ "/" ++ s) 
                              >>= return . maybe (fmap snd $ find ((==s) . fst) defaultMacrosMap) Just 
                              >>= return . fmap (retrieveMacro keybindings)
                        case mx of
                             Just x -> (iterate x (return ())) !! n
                             _ -> do
                                 let script = "str=\"`cat "++escapeQuery (myMacrosDir ++ "/" ++ s)++"`\"; [ -z \"$str\" ] && exit; res=\"$str\"; for ((i=1; i<"++show n++"; i++)); do res=\"$res $str\"; done; sleep 0.1; xdotool type \"$res\""
                                 spawn script
                 _ -> return ()
          )
        | (sk, ms) <- allMacroKeys
        , (nk, n) <- numberKeys
        ]
        ++
        [ ( "q " ++ k, n >>= toggleMacroMode) 
        | (k, n) <- [ ("<Esc>", return Nothing) ]
                    ++
                    allMacroKeys
        ]

-- provides some macros in code that do not need the user to specify one by one
-- each is a pair of (name, list of keybindings)
defaultMacrosMap:: [(String, [String])]
defaultMacrosMap =
    [
    ]
