module XMonad.Vim.Routine
    ( 
      compareLists
    , compareLists'
    , compareWithPriorList
    , compareWithPostList
    , compareFileNames
    , wrapList
    , charToKeyStroke
    , joinStr
    , prepend
    , maybeToMaybe
    , seqn
    , unescape
    , processKey
    , addPrefix
    , logger
    , runEvaluation
    ) where

import Data.Char
import Data.List
import Data.Maybe
import XMonad
import XMonad.Vim.Constants
import XMonad.Util.Run

logger s = spawn $ "echo \"`date`: \"" ++ escapeQuery s ++ " >> ~/.xmonad/xmonad.log"

compareLists _ [] [] = EQ
compareLists _ [] _ = LT
compareLists _ _ [] = GT
compareLists f (a:as) (b:bs) = case f a b of
                                    EQ -> compareLists f as bs
                                    r -> r

-- a version that ranks empty lists last
compareLists' _ [] [] = EQ
compareLists' _ [] _ = GT
compareLists' _ _ [] = LT
compareLists' f a b = compareLists f a b

compareWithPriorList fun ls a b = 
    let r = reverse ls
    in case compare (fun b r) (fun a r) of
        EQ -> compare a b
        c -> c
compareWithPostList fun ls a b =
    case compare (fun a ls) (fun b ls) of
        EQ -> compare a b
        c -> c

-- a fuzzy kind of comparator that handles case such as f5.pdf vs f10.pdf
compareFileNames [] [] = EQ
compareFileNames [] b = LT
compareFileNames a [] = GT
compareFileNames a b = 
    case (span isDigit a, span isDigit b) of
         ((ab, aa), ([], _)) -> let ((ab', aa'), (bb', ba')) = (break isDigit aa, break isDigit b)
                                    ta' = ab ++ ab'
                                in if ta' == bb' then compareFileNames aa' ba'
                                                 else compare a b
         (([], _), (bb, ba)) -> compare a b
         ((ab, aa), (bb, ba)) -> case compare ai bi of
                                      EQ -> compareFileNames aa ba
                                      r -> r
                where ai = read ab :: Int
                      bi = read bb :: Int

wrapList c = [c]

prepend _ [] = []
prepend t (x:l) = t:x:(prepend t l)

maybeToMaybe = maybe Nothing

seqn n = sequence_ . take n . repeat 

unescape = let tk b cked a = case (b, a, cked) of
                                (_,       [],      _) -> reverse b
                                ('\\':bs, '\\':as, False) -> tk b True as 
                                ('\\':bs, ha:as,   False) -> tk (ha:bs) False as 
                                (_,       ha:as,   _) -> tk (ha:b) False as 
           in tk "" False

processKey (k, a) = 
    let keys = words k
        -- find all adjacent pairs which have the same key (and append them with M-)
        pks ks = case findIndex (isPrefixOf "M-") ks of
                     Just i | i + 1 < length ks -> case (fromJust $ stripPrefix "M-" wi, fromMaybe wa $ stripPrefix "M-" wa, splitAt i ks) of
                                                        -- attach both keys with the prefix
                                                        (a, b, (bf, _:_:af)) | a == b -> bf ++ (wi:(pks (wi:af)))
                                                                             | otherwise -> bf ++ (wi:(pks (wa:af)))
                                                        _ -> ks
                                        where wi = ks !! i
                                              wa = ks !! (i+1)
                     _ -> ks
        p = joinStr " " (pks keys)
    in [(k,a)] ++ if p /= k then [(p,a)] else []

addPrefix (k, a) = ("M-"++k, a)

-- bug: cannot contain pipe symbol which is wierd
runEvaluation s = runProcessWithInput "/bin/sh" ["-c", s] ""
