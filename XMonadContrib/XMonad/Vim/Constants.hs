module XMonad.Vim.Constants
    (
    -- symbols
      typeables
    , alphas
    , nums
    , syms
    , extendedSequence
    , fullSequence
    , numberKeys
    , joinStr
    , charToKeyStroke
    -- directories, etc.
    , getScriptsDir
    , getBitmapsDir
    , getXMonadDir
    -- data
    , myTerminal
    ) where

import XMonad.Core
import Data.Char
import System.Directory
import Data.List
import Control.Monad.Trans

joinStr delimit [] = ""
joinStr delimit ls = foldl' ((++) . (++delimit)) (head ls) (tail ls)

charToKeyStroke c = if isUpper c then "S-"++[toLower c]
                                 else case c of
                                           '!' -> "S-1"
                                           '@' -> "S-2"
                                           '#' -> "S-3"
                                           '$' -> "S-4"
                                           '%' -> "S-5"
                                           '^' -> "S-6"
                                           '&' -> "S-7"
                                           '*' -> "S-8"
                                           '(' -> "S-9"
                                           ')' -> "S-0"
                                           '_' -> "S--"
                                           '+' -> "S-="
                                           '|' -> "S-\\"
                                           '~' -> "S-`"
                                           '{' -> "S-["
                                           '}' -> "S-]"
                                           ':' -> "S-;"
                                           '"' -> "S-'"
                                           '<' -> "S-,"
                                           '>' -> "S-."
                                           '?' -> "S-/"
                                           _ -> [c]

typeables = nums ++ easierSyms ++ lowerAlphas ++ mediumSyms ++ upperAlphas ++ restSyms
alphas = lowerAlphas ++ upperAlphas
lowerAlphas = ['a'..'z']
upperAlphas = fmap toUpper lowerAlphas
nums = ['1'..'9'] ++ "0"
easierSyms = "-="
mediumSyms = "[];',./"
restSyms = "\\!@#$%^&*()_+{}:\"<>?|" --`~"
syms = easierSyms ++ restSyms
extendedSequence s = filter (not . (`elem` s)) typeables
fullSequence s = s ++ extendedSequence s
numberKeys :: [(String, Int)]
numberKeys = flip zip [1..] $ [""] ++ fmap ((++" ") . joinStr " " . fmap charToKeyStroke . show) [2..9]

getScriptsDir :: MonadIO m => m String
getScriptsDir = getXMonadDir >>= return . (++"/scripts")
getBitmapsDir :: MonadIO m => m String
getBitmapsDir = getXMonadDir >>= return . (++"/dzen2")

myTerminal = "xterm"

