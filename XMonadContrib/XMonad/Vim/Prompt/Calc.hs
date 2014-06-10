module XMonad.Vim.Prompt.Calc
    (
      CalculatorMode(..)
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run

data CalculatorMode = CalcMode

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
    showXPrompt CalcMode = "calc > "
    commandToComplete CalcMode = id --send the whole string to `calc`
    completionFunction CalcMode = \s -> if (length s == 0) then return [] else do
        fmap (lines . trim) $ runProcessWithInput "calc" ["--", s] ""
    modeAction CalcMode c tx = spawn $ "res=\"`calc -- '"++c++"'`\"; shopt -s extglob; res=\"${res##*([[:space:]])}\"; res=\"${res%%*([[:space:]])}\"; echo -n \"$res\" | xclip -selection clipboard"
