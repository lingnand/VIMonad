module XMonad.Prompt.FMCPrompt
    ( mkFMCPrompt
    , FMCPrompt(..)
    , fmcAction
    , fmcComplFunc
    , splitArg
    , isCmdPrefixOf
    ) where

import XMonad.Prompt
import XMonad.Core
import Data.Char
import Data.List 
import Data.List.Split
import qualified Data.Text as T
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog (trim)

---- constant
fmcSetch = "setch"
--- added bitrate command to set the bitrate more conveniently (at the cost of restarting the service)
fmcBitRate = "kbps"
fmcAllowedBitRates = [ "64", "128", "192" ]

--- renamed skip to next so as to increase difference between commands; added kill to reboot the service
fmcOtherCompletions = ["play", "pause", "toggle", "next", "rate", "unrate", "ban", "webpage", "launch", "end"]
fmcAllCompletions = fmcOtherCompletions++[fmcSetch,fmcBitRate]

data FMCPrompt = FMCPrompt

wordInside word s = isInfixOf (map toLower word) $ map toLower s

instance XPrompt FMCPrompt where
    showXPrompt FMCPrompt = "fmc > "
    commandToComplete FMCPrompt = id
    nextCompletion FMCPrompt c l = if null l then c else compls
                                        where lastArg = last $ parseArgs c
                                              firstArg = head $ parseArgs c
                                              compls 
                                                | firstArg `isPrefixOf` fmcBitRate && (head l) `elem` fmcAllowedBitRates = "kbps " ++ (l !! exactMatchIndex)
                                                | firstArg `isPrefixOf` fmcSetch = "setch " ++ (head $ parseArgs (l !! nextChNumMatchIndex))
                                                | otherwise = skipLastWord c ++ (l !! exactMatchIndex)
                                              exactMatchIndex = case lastArg `elemIndex` l of
                                                                    Just i -> if i >= length l - 1 then 0 else i+1
                                                                    Nothing -> nextChFuzzyMatchIndex    
                                              nextChNumMatchIndex = case findIndex (\s -> lastArg == (head $ parseArgs s)) l of
                                                                        Just i -> if i >= length l - 1 then 0 else i+1
                                                                        Nothing -> nextChFuzzyMatchIndex    
                                              nextChFuzzyMatchIndex = case findIndex (wordInside lastArg) l of
                                                                           Just i -> i
                                                                           Nothing -> 0
    highlightPredicate FMCPrompt cl cmd = first == lastArg
                                        where first = fst $ splitArg cl
                                              args = parseArgs cmd
                                              lastArg = last args

splitArg s = case break isSpace s of
                  (a, _:b) -> (a, b)
                  (a, _) -> (a, "")
isCmdPrefixOf s c =  not (null s) && isPrefixOf s c

parseArgs = splitOn " "

fmcComplFunc s = 
    let (cmd, arg) = splitArg s
    in if cmd `isCmdPrefixOf` fmcSetch
          then fmap (fmap trim . tail . lines) $ runProcessWithInput "fmc" ["channels"] "" 
          else if cmd `isCmdPrefixOf` fmcBitRate
            then return fmcAllowedBitRates 
            else return $ filter (isPrefixOf cmd) fmcAllCompletions

fmcAction s = 
    let (cmd', arg) = splitArg s
        cmd = if cmd' `isCmdPrefixOf` fmcSetch then fmcSetch
                                            else if cmd' `isCmdPrefixOf` fmcBitRate then fmcBitRate
                                            else cmd'
    in spawn $ "fmc '" ++ cmd ++ " " ++ arg ++ "'"

mkFMCPrompt c = mkXPrompt FMCPrompt c fmcComplFunc fmcAction
