module XMonad.Prompt.RPCPrompt
    ( mkRPCPrompt
    , RPCPrompt(..)
    , rpcAction
    , rpcAction'
    , rpcComplFunc
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
import XMonad.Hooks.OnWindowsInserted

---- constant
rpcSetch = "setch"
--- added bitrate command to set the bitrate more conveniently (at the cost of restarting the service)
rpcBitRate = "kbps"
rpcAllowedBitRates = [ "64", "128", "192" ]

--- renamed skip to next so as to increase difference between commands; added kill to reboot the service
rpcOtherCompletions = ["play", "pause", "stop", "toggle", "next", "rate", "unrate", "ban", "webpage", "launch", "end"]
rpcAllCompletions = rpcOtherCompletions++[rpcSetch,rpcBitRate]
rpcAllChannels = 
    [ "999 Red-Heart"
    , "#top Jing+ tops"
    , "#psn Jing+ personal"
    , "#rand Jing+ rand"
    ,   "0 私人兆赫"
    ,   "1 华语"
    ,   "2 欧美"
    ,   "3 七零"
    ,   "4 八零"
    ,   "5 九零"
    ,   "6 粤语"
    ,   "7 摇滚"
    ,   "8 民谣"
    ,   "9 轻音乐"
    ,  "10 原声"
    ,  "13 爵士"
    ,  "14 电子"
    ,  "15 说唱"
    ,  "16 R&B" 
    ,  "17 日语"
    ,  "18 韩语"
    ,  "20 女声"
    ,  "22 法语"
    ,  "27 古典"
    ,  "28 动漫"
    ,  "32 咖啡馆"
    ,  "61 新歌"
    ,  "76 小清新"
    ,  "77 Easy" 
    , "179 豆瓣好歌曲"
    , "187 世界音乐"
    , "188 布鲁斯"
    , "189 拉丁"
    , "194 Pop" ]

data RPCPrompt = RPCPrompt

wordInside word s = isInfixOf (map toLower word) $ map toLower s

instance XPrompt RPCPrompt where
    showXPrompt _ = "rpc > "
    commandToComplete RPCPrompt = id
    nextCompletion RPCPrompt (c,_) l = (c', length c')
            where c' = if null l then c else compls
                  args = parseArgs c
                  firstArg = head args
                  tailArgs = tail args
                  lastArg = last args
                  lastChArg = if null tailArgs then "" else lastArg
                  compls
                    | firstArg == rpcSetch = skipLastWord c ++ (head $ parseArgs (l !! nextChNumMatchIndex))
                    | otherwise = skipLastWord c ++ (l !! exactMatchIndex)
                  exactMatchIndex = case lastArg `elemIndex` l of
                                        Just i -> if i >= length l - 1 then 0 else i+1
                                        Nothing -> 0    
                  nextChNumMatchIndex = case findIndex (\s -> lastChArg == (head $ parseArgs s)) l of
                                            Just i -> if i >= length l - 1 then 0 else i+1
                                            Nothing -> 0    
    highlightPredicate RPCPrompt cl (cmd,_) = first == lastArg
                                        where first = fst $ splitArg cl
                                              args = parseArgs cmd
                                              lastArg = last args

splitArg s = case break isSpace (trim s) of
                  (a, _:b) -> (a, b)
                  (a, _) -> (a, "")
isCmdPrefixOf s c =  not (null s) && isPrefixOf s c

parseArgs = splitOn " " . dropWhile isSpace

rpcComplFunc c s
    | head args == rpcSetch, allSpaceInBtw = return . filter (sp arg) $ rpcAllChannels
    | head args == rpcBitRate, allSpaceInBtw = return . filter (sp arg) $ rpcAllowedBitRates 
    | all null (init args) = return . filter (isPrefixOf arg) $ rpcAllCompletions
    | otherwise = return []
    where args = parseArgs s
          allSpaceInBtw = length args >= 2 && all null (init $ tail args)
          arg = last args
          sp = searchPredicate c

rpcAction' immi final owi s = 
    let (cmd', arg) = splitArg s
        cmd = if cmd' `isCmdPrefixOf` rpcSetch 
                 then if all isSpace arg 
                         then "vimch" 
                         else rpcSetch
                 else if cmd' `isCmdPrefixOf` rpcBitRate 
                        then rpcBitRate
                        else cmd'
        run = spawn $ "rpc " ++ cmd ++ " " ++ escapeQuery arg
    in if cmd `elem` ["webpage", "vimch"] 
          then applyOnWindowsInserted owi {
                     numberOfWindows = 1
                   , logFinished = \a b -> do
                       (logFinished owi) a b
                       final
               } >> run >> immi
          else run >> immi >> final
rpcAction = rpcAction' (return ()) (return ()) def

mkRPCPrompt c = mkXPrompt RPCPrompt c (rpcComplFunc c) rpcAction
