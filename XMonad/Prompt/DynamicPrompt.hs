module XMonad.Prompt.DynamicPrompt
    ( 
    ) where

import XMonad.Prompt
import XMonad.Core
import XMonad.Prompt.Shell

{----- the point of this module is to make running commands and applications 'a breeze'. The commands are used such that those that show output are promptly shown as completions-}

{----- dynamic prompt maintains a register that saves the 'current' working directory. This is useful especially for creating a dynamic workflow (switching to a workspace and automatically sets the directory-}

{----- constant-}
{-outputWidth = 200-}
{-isOutput s = length s >= outputWidth-}

{-fmcChCompletions = ["s", "setch"]-}
{-fmcInfoCompletions = ["i", "info"]-}
{-fmcHelpCompletions = ["h", "help"]-}
{---- added bitrate command to set the bitrate more conveniently (at the cost of restarting the service)-}
{-fmcBitRateCompletions = ["q", "quality"]-}
{-fmcAllowedBitRates = [ "64", "128", "192" ]-}

{---- renamed skip to next so as to increase difference between commands; added kill to reboot the service-}
{-fmcOtherCompletions = ["play", "pause", "toggle", "next", "ban", "rate", "unrate", "launch"]-}
{-fmcAllCompletions = ["info"]++fmcOtherCompletions++["setch","quality"]-}
{-fmcHeaderCompletions = ["q","i","s","h"]-}
{-wordInside word s = isInfixOf word $ map toLower s-}

-- dir is the current directory (shorten it for better display)
data DynamicPrompt = DPrompt dir

instance XPrompt DynamicPrompt where
    showXPrompt (DPrompt dir) = dir ++ " > "
    commandToComplete _ c = c
    completionToCommand _ = escape

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

-- presumably you should first get the correct path and then pass it here
dynamicPrompt dir c = do
    cmds <- io getCommands
    mkXPrompt (DPrompt $ shorten dir) (spawn . ("cd "++dir";"++))


{-instance XPrompt FMCPrompt where-}
    {-showXPrompt FMCPrompt = "douban.fm > "-}
    {-commandToComplete FMCPrompt = id-}
    {-completionToCommand FMCPrompt s = if isOutput s then "" else s-}
    {-nextCompletion FMCPrompt c l = if null l then "" else if isOutput (head l) then c else compls-}
                                        {-where lastArg = last $ parseArgs c-}
                                              {-firstArg = head $ parseArgs c-}
                                              {-compls -}
                                                {-| firstArg `elem` fmcBitRateCompletions && (head l) `elem` fmcAllowedBitRates = if lastArg `elem` fmcBitRateCompletions-}
                                                                                              {-then "quality " ++ head l-}
                                                                                              {-else skipLastWord c ++ (l !! exactMatchIndex)-}
                                                {-| firstArg `elem` fmcChCompletions && "私人兆赫" `isInfixOf` (head l) = if lastArg `elem` fmcChCompletions -}
                                                                                                  {-then "setch " ++ (head $ parseArgs $ head l)-}
                                                                                                  {-else skipLastWord c ++ (head $ parseArgs (l !! nextChNumMatchIndex))-}
                                                {-| otherwise = skipLastWord c ++ (l !! exactMatchIndex)-}
                                              {-exactMatchIndex = case lastArg `elemIndex` l of-}
                                                                    {-Just i -> if i >= length l - 1 then 0 else i+1-}
                                                                    {-Nothing -> nextChFuzzyMatchIndex    -}
                                              {-nextChNumMatchIndex = case findIndex (\s -> lastArg == (head $ parseArgs s)) l of-}
                                                                        {-Just i -> if i >= length l - 1 then 0 else i+1-}
                                                                        {-Nothing -> nextChFuzzyMatchIndex    -}
                                              {-nextChFuzzyMatchIndex = case findIndex (wordInside lastArg) l of-}
                                                                           {-Just i -> i-}
                                                                           {-Nothing -> 0-}
    {-highlightPredicate FMCPrompt cl cmd = (not $ isOutput cl) && if all isDigit first then first == lastArg else cl == lastArg-}
                                                {-where first = head $ parseArgs cl-}
                                                      {-args = parseArgs cmd-}
                                                      {-lastArg = last args-}

{-parseArgs s = if null $ trim s then [""] else words s-}

{-fmcComplFunc s  -}
    {-| null $ trim s = return fmcAllCompletions-}
    {-| findWord args fmcOtherCompletions = return []-}
    {-| findWord args fmcChCompletions  = do fmap (tail . lines) $ runProcessWithInput "fmc" ["channels"] "" -}
    {-[>| (findWord args fmcInfoCompletions) || (null $ trim s) = do fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput "fmc" ["info"]<]-}
    {--- default to displaying information from a web context in a single thread is unreliable (can lead to serious lag)-}
    {-| findWord args fmcInfoCompletions = do fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput "fmc" ["info"] "" -}
    {-| findWord args fmcBitRateCompletions = return fmcAllowedBitRates -}
    {-| findWord args fmcHelpCompletions = do fmap lines $ runProcessWithInput "fmc" ["help"] ""-}
    {-| otherwise = return $ (if length lastArg > 0 then filter (isPrefixOf lastArg) fmcHeaderCompletions else [])++(filter (isPrefixOf $ lastArg) $ fmcAllCompletions)-}
        {-where args = parseArgs s-}
              {-lastArg = last args-}

{-fmcAction s = spawn $ case s of-}
                            {-_  | h `elem` fmcChCompletions -> "fmc setch "++ (concat (tail args))-}
                               {-| h `elem` fmcBitRateCompletions -> "fmc quality "++sec-}
                               {-| otherwise -> "fmc "++s-}
                        {-where args = parseArgs s-}
                              {-h = head args-}
                              {-sec = if length args >= 2 then args !! 1 else ""-}

{-mkFMCPrompt c = mkXPrompt FMCPrompt c fmcComplFunc fmcAction-}
