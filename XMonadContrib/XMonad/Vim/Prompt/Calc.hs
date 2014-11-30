module XMonad.Vim.Prompt.Calc
    (
      CalculatorMode(..)
    , calcAction'
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Vim.Constants
import XMonad.Hooks.OnWindowsInserted

data CalculatorMode = CalcMode

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
    showXPrompt CalcMode = "calc > "
    commandToComplete CalcMode = id --send the whole string to `calc`
    completionFunction CalcMode = \s -> if (length s == 0) then return [] else do
        fmap (lines . trim) $ runProcessWithInput "calc" ["--", s] ""
    modeAction CalcMode c tx = calcAction' (return ()) (return ()) def True (\_ _ immi final _ -> immi >> final) c

calcAction' :: X () -> X () -> OnWindowsInserted -> Bool -> ((XPConfig -> XPConfig) -> [String] -> X () -> X () -> OnWindowsInserted -> X ()) -> String -> X ()
calcAction' immi final owi silent reprompt s = do
    scriptsDir <- getScriptsDir
    spawn (scriptsDir++"/xcalc "++escapeQuery s) 
    if silent then immi >> final
              else reprompt (\c -> c {defaultText = "calc "}) [] immi final owi
