{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Vim.Prompt.Wallpaper
    (
      mkWPPrompt
    , toggleFadeOut
    , wallpaperStartupHook
    , wallpaperLogHook
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.RPCPrompt
import XMonad.Prompt.Shell
import XMonad.Vim.Routine
import Data.List
import Data.Maybe
import XMonad.Util.Run
import Data.List.Split
import System.Directory
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Set as S
import XMonad.Hooks.FadeInactive
import XMonad.Vim.Workspaces
import Control.Monad
import Control.Applicative
import Data.IORef

changeWallpaperBin = "wallpaper-change"
-- wallpaperDirectory inside the home directory
wallpaperDirectoryWithinHome = "/Pictures/wallpapers/"
getWallpaperDirectory = do
    home <- io $ getHomeDirectory
    return $ home ++ wallpaperDirectoryWithinHome
changeWallpaperCmd = changeWallpaperBin
wallpaperOpenBin = "feh"
defaultFadeInactiveAmount = 0.85
wallpaperFadeFullAmount = 0.15

defaultWPChannel = "#standard"

doNotFadeOutWindows =  className =? "xine" <||> className =? "MPlayer"

defaultFadeTest floats =
    -- liftM not doNotFadeOutWindows <&&> isUnfocused <&&> disableFadingWithinClassNames <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)
    liftM not doNotFadeOutWindows <&&> fmap not isFocused <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)


data WallpaperPrompt = WPPrompt String

instance XPrompt WallpaperPrompt where
    showXPrompt (WPPrompt dir) = dir ++ " > "
    commandToComplete _ = id
    nextCompletion _ (c,_) l = (c', length c')
                where c' = let (cmd, arg) = splitArg c 
                           in if cmd `isCmdPrefixOf` "setch"
                                 then "setch " ++ (l !! exactMatchIndex (unescape arg))
                                 else if cmd `isCmdPrefixOf` "wallhaven"
                                 then "wallhaven " ++ (l !! exactMatchIndex (unescape arg))
                                 else  l !! exactMatchIndex cmd
                                    where exactMatchIndex a = case a `elemIndex` l of
                                                                    Just i -> if i >= length l - 1 then 0 else i+1
                                                                    Nothing -> 0    
    -- only check on the last word
    highlightPredicate _ cl (cmd,_) = let lastArg = case splitArg cmd of
                                                    (a, "") -> a
                                                    (_, b) -> b
                                    in unescape lastArg == cl

data WallpaperChannel = WPChannel String deriving (Typeable, Show, Read)
instance ExtensionClass WallpaperChannel where
    initialValue = WPChannel defaultWPChannel
    extensionType = PersistentExtension

-- available commands
---- next: go to the next wallpaper
---- setch: change the wallpaper channel
---- rate: tag the current wallpaper favorite (move it to the favorite folder) (and NOT moving it anywhere)
---- ban: move the wallpaper to the trash folder
---- open: open the current wallpaper in feh
wpComplFunc wallpaperDirectory conf str = 
    let (cmd, arg) = splitArg str
    -- the setch command default to use the wallhaven
    in if cmd `isCmdPrefixOf` "setch" || cmd `isCmdPrefixOf` "wallhaven"
                -- add in the . to denote the base wallpaper directory, and also to prevent auto complete 
                then filter (searchPredicate conf arg)
                   . sortBy (compareWithPriorList elemIndex ["#standard", "#favorites"])
                   . map (\s -> fromMaybe s (stripPrefix wallpaperDirectory s)) 
                   . lines <$> (runProcessWithInput "find" [wallpaperDirectory, "-mindepth", "1", "-type", "d"] "")
                else return $ filter (isPrefixOf cmd) ["next", "setch", "flickr", "wallhaven", "rate", "ban", "trash"]

replace st rep string = joinStr rep $ splitOn st string

wpAction wallpaperDirectory c ch str = 
    let (cmd, arg) = splitArg str
        downloadch ess downloadscript = do
               let changescript = "while [ -d "++ ess ++" ] && [ -z \"`find " ++ ess ++ " \\( -name '*.jpg' -o -name '*.png' -o -name '*.gif' \\) -print -quit 2>/dev/null`\" ]; do sleep 1; done"
               runProcessWithInput "/bin/sh" ["-c", "mkdir -p " ++ ess] ""
               spawn $ downloadscript ++ "; rmdir " ++ ess
               runProcessWithInput "/bin/sh" ["-c", changescript] ""
               return ()
    in if cmd `isCmdPrefixOf` "setch" || cmd `isCmdPrefixOf` "wallhaven"
          then do
                io $ setCurrentDirectory wallpaperDirectory
                -- save the arg into the database
                let ess = escapeQuery arg
                    exists = io $ doesDirectoryExist arg
                e <- exists
                let sorting
                        -- if the directory doesn't exists then we download the most favorited images
                        | not e = Just "favorites"
                        -- if the user specifies wallhaven deliberately to redownload a directory we then use random sorting
                        | cmd `isCmdPrefixOf` "wallhaven" = Just "random"
                        | otherwise = Nothing
                case sorting of
                     Just s -> let query = reverse $ takeWhile (/='/') $ reverse arg in
                               downloadch ess $ "wallhaven -d "++ ess ++ " -s " ++ s ++ " " ++ escapeQuery query 
                     _ -> return ()
                whenX (io $ doesDirectoryExist arg) $ do
                   XS.put $ WPChannel arg
                   spawn $ changeWallpaperCmd ++ " " ++ ess
                mkWPPrompt' c c
          else if cmd `isCmdPrefixOf` "flickr"
                then do
                       io $ setCurrentDirectory wallpaperDirectory
                       let searchDir = "#rss/flickr/" ++ arg
                           ess = escapeQuery searchDir
                       downloadch ess $ "rss-image-download "++ escapeQuery ("https://api.flickr.com/services/feeds/photos_public.gne?format=rss_200_enc&tags=" ++ arg) ++ " " ++ ess
                       whenX (io $ doesDirectoryExist searchDir) $ do
                          XS.put $ WPChannel searchDir
                          spawn $ changeWallpaperCmd ++ " " ++ ess
                       mkWPPrompt' c c
          else if cmd `isCmdPrefixOf` "trash"
                then if ch /= "." 
                        then do
                            XS.put $ WPChannel defaultWPChannel
                            spawn $  "dir="++escapeQuery (wallpaperDirectory ++ ch)++"; target=\"$HOME/.Trash/${dir##*/}\"; rm -rf \"$target\"; mv -f \"$dir\" \"$target\"" 
                            spawn $ changeWallpaperCmd ++ " " ++ (escapeQuery $ wallpaperDirectory++defaultWPChannel)
                            -- mkWPPrompt' (c {defaultText = "setch "}) c
                            mkWPPrompt' c c
                        else
                            mkWPPrompt' c c
          else case cmd of
                "next" -> spawn (changeWallpaperCmd ++ " .") >> mkWPPrompt' c c
                "rate" -> spawn $ changeWallpaperCmd ++ " -F" 
                "ban" -> spawn (changeWallpaperCmd ++ " -D .") >> mkWPPrompt' c c
                _ -> return ()

mkWPPrompt toggleFadeSet c = do
    fadeOutLogHook $ fadeIf (return True) wallpaperFadeFullAmount
    mkWPPrompt' c c
    fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount

mkWPPrompt' c' c = do
    wallpaperDirectory <- getWallpaperDirectory
    ch <- getWPChannel wallpaperDirectory
    io $ setCurrentDirectory $ wallpaperDirectory ++ ch
    mkXPrompt (WPPrompt ch) c' (wpComplFunc wallpaperDirectory c) (wpAction wallpaperDirectory c ch)

getWPChannel wallpaperDirectory = do
    WPChannel ch' <- XS.get 
    e <- io $ doesDirectoryExist $ wallpaperDirectory ++ ch'
    return $ if e then ch' else defaultWPChannel

getWPDirectory wallpaperDirectory = getWPChannel wallpaperDirectory >>= \c -> return $ wallpaperDirectory ++ c

toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s 
    | w `S.member` s = S.delete w s
    | otherwise = S.insert w s

wallpaperStartupHook = do
    wallpaperDirectory <- getWallpaperDirectory
    d <- getWPDirectory wallpaperDirectory
    spawn $ changeWallpaperCmd ++ " " ++ escapeQuery d

wallpaperLogHook toggleFadeSet = fadeOutLogHook $ fadeIf (defaultFadeTest toggleFadeSet) defaultFadeInactiveAmount
