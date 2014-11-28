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
    -- directories, etc.
    , getMyScriptsDir
    , getMyBitmapsDir
    , getMyXMonadDir
    -- data
    , myTerminal
    ) where

import Data.Char
import System.Directory
import XMonad.Vim.Routine

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

getMyXMonadDir = getHomeDirectory >>= return . (++ "/.xmonad")
getMyScriptsDir = getMyXMonadDir >>= return . (++"/scripts")
getMyBitmapsDir = getMyXMonadDir >>= return . (++"/dzen2")

myTerminal = "xterm"

