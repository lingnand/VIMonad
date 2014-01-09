module XMonad.Prompt.FMCPrompt
    ( mkFMCPrompt
    , FMCPrompt(..)
    , fmcAction
    , fmcComplFunc
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
outputWidth = 200
isOutput s = length s >= outputWidth

fmcChCompletions = ["s", "setch"]
fmcInfoCompletions = ["i", "info"]
fmcHelpCompletions = ["h", "help"]
--- added bitrate command to set the bitrate more conveniently (at the cost of restarting the service)
fmcBitRateCompletions = ["k", "kbps"]
fmcAllowedBitRates = [ "64", "128", "192" ]

--- renamed skip to next so as to increase difference between commands; added kill to reboot the service
fmcOtherCompletions = ["play", "pause", "toggle", "next", "rate", "unrate", "ban", "webpage", "launch", "end"]
fmcAllCompletions = ["info"]++fmcOtherCompletions++["setch","kbps"]
fmcHeaderCompletions = ["k","i","s","h"]

data FMCPrompt = FMCPrompt

wordInside word s = isInfixOf (map toLower word) $ map toLower s

instance XPrompt FMCPrompt where
    showXPrompt FMCPrompt = "douban.fm > "
    commandToComplete FMCPrompt = id
    completionToCommand FMCPrompt s = if isOutput s then "" else s
    nextCompletion FMCPrompt c l = if null l then "" else if isOutput (head l) then c else compls
                                        where lastArg = last $ parseArgs c
                                              firstArg = head $ parseArgs c
                                              compls 
                                                | firstArg `elem` fmcBitRateCompletions && (head l) `elem` fmcAllowedBitRates = if lastArg `elem` fmcBitRateCompletions
                                                                                              then "kbps " ++ head l
                                                                                              else skipLastWord c ++ (l !! exactMatchIndex)
                                                | firstArg `elem` fmcChCompletions && any ("私人兆赫" `isInfixOf`) l = if lastArg `elem` fmcChCompletions 
                                                                                                  then "setch " ++ (head $ parseArgs $ head l)
                                                                                                  else skipLastWord c ++ (head $ parseArgs (l !! nextChNumMatchIndex))
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
    highlightPredicate FMCPrompt cl cmd = (not $ isOutput cl) && first == lastArg
                                                where first = head $ parseArgs cl
                                                      args = parseArgs cmd
                                                      lastArg = last args

parseArgs s = if null $ trim s then [""] else words s

fmcComplFunc s  
    | null $ trim s = return fmcAllCompletions
    | findWord args fmcOtherCompletions = return []
    | findWord args fmcChCompletions  = do fmap (fmap trim . tail . lines) $ runProcessWithInput "fmc" ["channels"] "" 
    {-| (findWord args fmcInfoCompletions) || (null $ trim s) = do fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput "fmc" ["info"]-}
    -- default to displaying information from a web context in a single thread is unreliable (can lead to serious lag)
    | findWord args fmcInfoCompletions = do fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput "fmc" ["info"] "" 
    | findWord args fmcBitRateCompletions = return fmcAllowedBitRates 
    | findWord args fmcHelpCompletions = do fmap lines $ runProcessWithInput "fmc" ["help"] ""
    | otherwise = return $ (if length lastArg > 0 then filter (isPrefixOf lastArg) fmcHeaderCompletions else [])++(filter (isPrefixOf $ lastArg) $ fmcAllCompletions)
        where args = parseArgs s
              lastArg = last args

fmcAction s = spawn $ "fmc '" ++ (foldl ((++) . (++" ")) cmd $ tail args) ++ "'"
                        where args = parseArgs s
                              h = trim $ head args
                              cmd = if h `elem` fmcChCompletions then "setch" else if h `elem` fmcBitRateCompletions then "kbps" else h

mkFMCPrompt c = mkXPrompt FMCPrompt c fmcComplFunc fmcAction
