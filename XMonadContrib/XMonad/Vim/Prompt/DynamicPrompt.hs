module XMonad.Vim.Prompt.DynamicPrompt
    ( 
      tagLimit
    , dynamicPrompt
    , addOrTruncateTillPrefix
    , setInputAndDone
    , changeInputAndDone
    , cycleDictionaryForDPrompt
    ) where

import XMonad.Prompt
import XMonad.Core
import XMonad.Prompt.Shell
import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.Posix
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.OnWindowsInserted
import XMonad.Vim.Routine
import Data.Maybe
import XMonad.Prompt.TaskPrompt
import XMonad.Prompt.FMCPrompt
import XMonad.Vim.WorkspaceDirectories
import XMonad.Util.Run
import XMonad.Vim.Tag
import XMonad.Vim.Prompt.Calc
import XMonad.Vim.Prompt.Vimb
import XMonad.Vim.Prompt.Dict
import XMonad.Vim.Constants
import XMonad.Vim.Workspaces
import System.Directory
import qualified Data.Map as M

outputWidth = 200
outputHeight = 40

data DynamicPrompt = DPrompt String

instance XPrompt DynamicPrompt where
    showXPrompt (DPrompt dir) = dir ++ " > "
    commandToComplete _ = id
    nextCompletion _ = dpromptNextCompletion
    highlightPredicate _ = dpromptHighlightPredicate

-- helper functions
parseShellArgs str = let tk b a = if null a || (isSpace (head a) && not (null b) && head b /= '\\')
                                      then ([reverse b], a)
                                      else tk (head a:b) (tail a)
                         (arg, rest) = tk "" (dropWhile (==' ') str)
                     in if null rest then arg else arg ++ parseShellArgs rest
stripSuffix suf s = fmap reverse $ stripPrefix (reverse suf) (reverse s)
isOutput = (>= outputWidth) . length
limitSpace l str = take l $ str ++ repeat ' '

-- search widgets
fasdLimit = 20
findLimit = 20
tagLimit = 20
grepLimit = 50
whichLimit = 20
historyLimit = 20
evalLimit = 20
topLimit = 40

isGrepOutput s = isOutput s && ':' `elem` s
stripGrepOutput = head . splitOn ":"
isShortcutOutput s = length s > 2 && (s!!1) == ':'
stripShortcutOutput = last . splitOn ":"
-- the index i is the i'th arg from the end
isSearchFilter (i, s)
    -- just not performance-wise up to the standard for fasd right now.
    -- I have the feeling that the author hasn't done a terribly good job at optimizing this
    -- | i > 0 && s `elem` ["f", "a", "d", "z"] = True
    | i > 0 && s `elem` ["h", "l", "t", "which", "diff"] = True 
    -- we let the application to decide what to do after that
    | i > 0 && s `elem` ["g", "gp"] = True
    | i == 0 && (length s) `elem` [1, 2] && head s == '\'' && not ((last s) `elem` "v") = True
    | i > 0 && length s == 2 && head s == 'm' && not ((last s) `elem` "v") = True
    | i > 0 && s == "diff" = True
    | otherwise = False
-- isStatDiffOutput = (=~ " +\\| +[0-9]+ [+-]+")
isStatDiffOutput = (=~ " +\\| +[^ ]+ [^ ]+")
stripStatDiffOutput = trim . head . splitOn "|" 
filterDiffOutput ls = case findIndex (isPrefixOf "--- ") ls of
                            Just i | i > 0 && i < length ls - 1 && (cxt (i-1) (i+1) || cxt (i+1) (i-1)) ->
                                                case stripPrefix "--- a/" (ls !! i) of
                                                     Just r -> (trim r):fr
                                                     _ -> fr
                                   | otherwise -> fr
                                where fr = filterDiffOutput $ snd $ splitAt (i+2) ls
                                      cxt n m = isPrefixOf "+++ " (ls !! n) && isPrefixOf "index " (ls !! m)
                            _ -> []

data PromptWidget = PromptWidget { promptPrefix :: String
                                 , promptCommandToComplete :: String -> String
                                 , promptNextCompletion :: String -> [String] -> String
                                 , promptComplFunction :: XPConfig -> ComplFunction
                                 , promptAction :: X () -> X () -> OnWindowsInserted -> String -> X ()
                                 , promptHighlightPredicate :: String -> String -> Bool
                                 }
-- (prefix, commandToComplete, nextCompletion, complFunc, modeAction)
dwgt p pre cf act = PromptWidget { promptPrefix = pre
                                 , promptCommandToComplete = commandToComplete p
                                 , promptNextCompletion = nextCompletion p
                                 , promptComplFunction = cf
                                 , promptAction = act
                                 , promptHighlightPredicate = highlightPredicate p
                                 }
isWidgetFilter (i, s) = i > 0 && s `elem` (fmap promptPrefix dynamicPromptWidgets)
widgetCmd w c = let p = promptPrefix w++" " in joinStr p $ tail $ splitOn p c
findWidget pre = find ((==pre) . promptPrefix) dynamicPromptWidgets
findWidgetForAction c = fmap (\(r, w) -> (fromJust r, w)) $ find (isJust . fst) $ fmap (\w -> (stripPrefix (promptPrefix w ++ " ") c, w)) dynamicPromptWidgets

dwgtMode mode prompt = dwgt mode prompt (\c -> completionFunction mode) (\immi final _ s -> (modeAction mode) s "" >> immi >> final)
dwgtDictMode mode prompt = dwgt mode prompt (\c -> completionFunction mode) (\immi final owi s -> do
    applyOnWindowsInserted owi {
              numberOfWindows = 1
            , logFinished = \a b -> do
                (logFinished owi) a b
                final
        }
    (modeAction mode) s "" 
    immi)
dynamicPromptWidgets = [
        -- this follow the (prefix, prompt) format
        dwgt VBPrompt "vb" (\c -> vbComplFunc) (\immi final owi s -> do
            -- wait until the vb is completely spawned
            applyOnWindowsInserted owi {
                      numberOfWindows = 1
                    , logFinished = \a b -> do
                        (logFinished owi) a b
                        final
                }
            vbAction s
            immi
        )
      , dwgtDictMode defaultSDMode "sdcv-Collins"
      , dwgtDictMode mobySDMode "sdcv-Moby"
      , dwgtDictMode modernCHSDMode "sdcv-modernChinese"
      , dwgtDictMode bigCHSDMode "sdcv-bigChinese"
      , dwgt TaskPrompt "tk" (\c -> taskComplFunc) taskAction'
      , dwgt FMCPrompt "fmc" (\c -> fmcComplFunc) fmcAction'
      , dwgtMode CalcMode "calc"
    ]

evalStr s = let evalcomps = splitOn "`" s
                evallen = length evalcomps 
                t = tail evalcomps
            in if odd evallen && evallen >= 3 then (head evalcomps, head t, joinStr "`" $ tail t)
                                              else (s,"","")

dpromptNextCompletion c l = let hl = any (flip dpromptHighlightPredicate c) l in
                case dpromptBreak args of
                   (_, "z":bef) | not hl -> joinStr " " $ reverse bef ++ ["c", escape (head l)]
                   ([], ('\'':_):_) -> exactMatch $ fmap stripShortcutOutput l
                   (_, "which":bef) | all ((`elem` "~/") . head) l && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                                    | otherwise -> exactMatch l
                   (aft, "diff":bef) | not (null fdo) -> joinStr " " $ (case bef of
                                                                            "git":bef' -> reverse bef'
                                                                            _ -> reverse bef) ++ [escape (head fdo)]
                   (_, w:bef) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                           in joinStr " " $ reverse bef ++ [w, promptNextCompletion wp (widgetCmd wp c) l]
                   -- we have to specify manually so the 'm' case above when there is some sort of the string on its right would sift through here
                   (_, f:bef) | f `elem` ["f", "a", "d", "h", "l", "t", "which"] && not hl -> joinStr " " $ reverse bef ++ [escape (head l)]
                   _ | all isGrepOutput l -> joinStr " " $ takeWhile (\t -> t /="g" && t /= "gp") (init args) ++ case exactNext $ fmap stripGrepOutput l of
                                                                                          [] -> []
                                                                                          s -> [s]
                     | all isShortcutOutput l -> exactMatch $ fmap stripShortcutOutput l
                     | not (null fsdo) && (length fsdo) `elem` [length l, length l - 1] -> exactMatch $ fmap stripStatDiffOutput fsdo
                     | not (null fdo) -> exactMatch fdo
                     | not (null evalstr) ->  evall ++ escape (head l) ++ evalr
                     | all isOutput l && any ("commit" `isPrefixOf`) l && any ("Author" `isPrefixOf`) l && any ("Date" `isPrefixOf`) l -> exactMatch $ fmap (fromJust . stripPrefix "commit " . trim) $ filter ("commit " `isPrefixOf`) l
                     | isOutput (head l) -> c
                     | otherwise -> exactMatch l
             where lastArg = last $ args
                   args = parseShellArgs c
                   (evall, evalstr, evalr) = evalStr c
                   fdo = filterDiffOutput l
                   fsdo = filter isStatDiffOutput l
                   exactMatch ls = fromMaybe "" (stripSuffix lastArg c) ++ exactNext ls
                   exactNext ls = let rev = reverse ls in escape $ rev !! case findIndex (== unescape lastArg) rev of
                               Just i -> if i <= 0 then length ls - 1 else (i-1)
                               Nothing -> length ls - 1 

dpromptHighlightPredicate cl cmd = case dpromptBreak args of
                (_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                      in promptHighlightPredicate wp cl (widgetCmd wp cmd)
                _ | isGrepOutput cl -> stripGrepOutput cl == unescapedLastArg
                  | isShortcutOutput cl -> stripShortcutOutput cl == unescapedLastArg
                  | not (null unescapedLastArg) && ("--- a/"++unescapedLastArg) `isPrefixOf` cl -> True
                  | isStatDiffOutput cl -> stripStatDiffOutput cl == unescapedLastArg
                  | trimcl =~ "commit [0-9a-z]{40}" -> last (words trimcl) == unescapedLastArg
                  | isOutput cl -> False
                  | otherwise -> not (null args) && unescapedLastArg == cl 
             where args = parseShellArgs cmd
                   unescapedLastArg = unescape $ last args 
                   trimcl = trim cl

-- given an array of arguments break it in such a way that it divides the necessary keyword with what is after 
dpromptBreak args = 
    -- we would first break from the beginning to see if any widget matches up
    let stripIndex (a, b) = (fmap snd a, fmap snd b)
        rev = zip [0..] $ reverse args
    in case break isWidgetFilter rev of
         (a, []) -> stripIndex $ break isSearchFilter rev
         b -> stripIndex b

whenNull ma mb = do
    l <- ma
    if null l then mb else return l

dpromptComplFunc c cmds home hist myScriptsDir s = do
        let args = parseShellArgs s
            lastArg = if null args then "" else last args
            unescapedArgs = map unescape args
            sht = shortend home
            shtout = fmap (fmap sht)
            epd = expandd home
            sp = searchPredicate c
            fasd args = shtout $ fmap lines $ runProcessWithInput (myScriptsDir++"/xfasd") (show fasdLimit:args) ""
            sct pre = fmap lines $ runProcessWithInput (myScriptsDir++"/xshortcut") [ "print", pre ] ""
            ntailsp = length $ takeWhile isSpace (reverse s)
            output pro ags = fmap (map (limitSpace outputWidth . sht) . lines) $ runProcessWithInput pro ags ""
            (_,evalstr,_) = evalStr s
            trycmp = foldl whenNull (return [])
            scopecmp = case reverse unescapedArgs of
                                   "":fa:_ | ntailsp == 1 -> output "scope" [epd fa, show outputWidth, show outputHeight, show outputHeight]
                                   _ -> return [] 
            shellcmp = let (scs, sas) = case head unescapedArgs of
                                "man" -> (cmds, [])
                                "du" -> ([], ["file"])
                                "c" -> ([], ["directory"])
                                "cd" -> ([], ["directory"])
                                _ | length args > 1 -> ([], ["file"])
                                  | otherwise -> (cmds, ["file"])
                       in shellcmp' sas scs
            shellcmp' sas scs = shtout $ getShellCompl' False sas scs $ epd lastArg
            grepcmp' cmd aft = fmap (filter (':' `elem`)) $ output (myScriptsDir++"/"++cmd) $ show grepLimit:(fmap epd aft)
            grepcmp = grepcmp' "xgrep"
            pgrepcmp = grepcmp' "xpdfgrep"
            gitcmdcmp args = return $ filter (sp args) ["add", "am", "archive", "bisect", "branch", "bundle", "checkout", "cherry-pick", "citool", "clean", "clone", "commit", "describe", "diff", "fetch", "format-patch", "gc", "grep", "gui", "init", "log", "merge", "mv", "notes", "pull", "rebase", "reset", "rm", "shortlog", "show", "stash", "status", "submodule", "tag"]
        case dpromptBreak unescapedArgs of
                    (afs, "f":_) -> fasd $ ["-f", "-B", "viminfo"] ++ (reverse afs)
                    (afs, "d":_) -> fasd $ "-d" : (reverse afs)
                    (afs, "a":_) -> fasd $ ["-a", "-B", "viminfo"] ++ (reverse afs)
                    (afs, "z":_) -> fasd $ "-d" : (reverse afs)
                    (afs, "h":_) -> return $ take historyLimit $ filter (sp $ joinStr " " $ reverse afs) $ hist
                    ([], ('\'':pre):_) -> shtout $ sct pre
                    (afs, ('m':pre):_) -> shellcmp' ["directory"] []
                    (afs, "l":_) -> shtout $ fmap lines $ runProcessWithInput (myScriptsDir++"/xfind") (show findLimit:(fmap epd $ reverse afs)) ""
                    -- for g, we demand that at least SOME search term is entered, otherwise we don't generate the necessary output
                    (af:afs, "g":_) -> trycmp $ (if afs == [] then [shellcmp' ["file"] []] else []) ++ [grepcmp (reverse $ af:afs)]
                    (af:afs, "gp":_) | afs == [] -> shellcmp' ["file"] []
                                     | not (null afs) && af == "" -> pgrepcmp (reverse $ af:afs)
                    (afs, "t":_) -> getTagBin >>= \tagBin -> fmap (fmap (tagLibroot++) . lines) $ runProcessWithInput tagBin ([show tagLimit, "false"] ++ tagQuery (reverse afs)) ""
                    (afs, "which":_) -> 
                        let ha = head cas
                            cas = reverse afs
                        -- test if af is one of the commands
                        in trycmp [shtout $ fmap lines $ runProcessWithInput "which" cas "", shtout $ return $ take whichLimit $ filter (sp $ joinStr " " cas) cmds]
                    -- we need to make sure that we are using the current directory as a starting point
                    (afs, "diff":_) | ntailsp <= 1 -> trycmp [output (myScriptsDir++"/xdiff") (show outputWidth:show outputHeight:diffargs), shellcmp' ["file"] []]
                            where diffargs = if afs == [""] then [] else reverse afs
                    (_, w:_) | isJust (findWidget w) -> let wp = fromJust $ findWidget w 
                                                          in (promptComplFunction wp) c (widgetCmd wp s)
                -- grave key evaluation (evaluate the grave enclosed string in shell and show the output as autocompletion)
                    _ | not (null evalstr) -> shtout $ fmap (take evalLimit . lines) $ runProcessWithInput "/bin/sh" ["-c", "loader " ++ escapeQuery evalstr] ""
                      | otherwise -> case unescapedArgs of
                            "man":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xman") $ show outputWidth:show outputHeight:pa:pai:pas
                            "du":pa:pai:pas | lastArg == "" -> output (myScriptsDir++"/xdu") $ show outputHeight:pa:pai:pas
                            ["ip", pa] -> return $ filter (sp pa) ["addr", "addrlabel", "link", "maddr", "mroute", "neigh", "route", "rule", "tunnel"]
                            "ip":pas | lastArg == "" -> output "ip" $ init pas
                            ["top", ""] | ntailsp == 1-> output (myScriptsDir++"/xtop") [show topLimit]
                            ["free", ""] | ntailsp == 1 -> output "free" []
                            ["ifconfig", ""] | ntailsp == 1 -> output "ifconfig" []
                            ("git":gitcmds) | gitcmds `elem` [[""], ["status", ""]] && ntailsp == 1 -> trycmp [output "git" ["status"], gitcmdcmp ""]
                            -- only gives the prime command completion on three spaces
                            ["git", pa] -> gitcmdcmp pa
                            -- in all other instances we should give the log information
                            "git":pa:_ -> trycmp [output "git" $ ["log", "--grep", last unescapedArgs], scopecmp, shellcmp]
                            _ -> trycmp [scopecmp, shellcmp]


cmdsWithGUI = ["xterm", "retroarch", "gimp", "inkscape", "libreoffice", "xvim", "xmutt", "zathura", "vimb", "vb", "intellij-idea-ultimate-edition"]
dpromptAction c cmds home history hist myScriptsDir immi final owi s = 
        -- perform some special actions on some commands
        let args = parseShellArgs s
            follow = immi >> final
        in case (findWidgetForAction s, args) of
                (Just (rest, w), _) -> (promptAction w) immi final owi rest
                (_, [('m':pre:[]), pa]) -> spawn (myScriptsDir++"/xshortcut mark "++[pre]++" "++pa) >> follow
                (_, "reboot":_) -> removeAllWorkspaces >> spawn "reboot"
                (_, "systemctl":"poweroff":_) -> removeAllWorkspaces >> spawn "systemctl poweroff"
                (_, ha:pas) | ha `elem` ["cd", "c", "z"] -> do
                                 d <- if null pas 
                                         then return home 
                                         else case head pas of
                                                 "-" -> getCurrentWorkspaceOldDirectory
                                                 p -> return $ unescape p
                                 saveCurrentWorkspaceDirectory d
                                 dynamicPrompt' c cmds home history hist immi final owi
                            | otherwise -> do
                                -- we need to determine the sort of thing to do with the dphandler
                                fe <- io (doesFileExist ha)
                                de <- io (doesDirectoryExist ha)
                                let ha' = if null s then "." else s
                                    run = spawn $ "loader " ++ myScriptsDir++"/dphandler" ++" "++ha'
                                if fe || de || '/' `elem` ha' || ('.' `elem` ha' && pas == []) || ha `elem` cmdsWithGUI
                                   then do
                                        applyOnWindowsInserted owi {
                                                  numberOfWindows = 1
                                                , logFinished = \a b -> do
                                                    (logFinished owi) a b
                                                    final
                                            }
                                        run 
                                        immi
                                   else run >> follow
                                

divide' _ [] (r, w) = (reverse r, reverse w)
divide' p (x:xs) (r, w) = divide' p xs $ if p x then (x:r, w) else (r, x:w)
divide p l = divide' p l ([],[])

dynamicPrompt c immi final owi = do
    cmds <- io getCommands
    home <- io $ env "HOME" "~"
    history <- io readHistory
    let histp = (=~ "^~?/")
        histf a z = (filter histp a) ++ z
        hist = fmap unescape $ nub $ sort $ M.foldr histf [] history
    --     relative' = filter (\s -> isPrefixOf "./" s || isPrefixOf "../" s) hist
    -- relative <- filterM (\f -> do
    --         fe <- io $ doesFileExist f 
    --         de <- io $ doesDirectoryExist f
    --         return $ fe || de) relative'
    -- to better fasd performace, we can first extract out all the values for the fasd components
    ------ let isd p = io (getFileStatus p >>= return . isDirectory) `catchX` (return False)
    ------ fasdl <- fmap lines $ runProcessWithInput (myScriptsDir++"/xfasd") [] ""
    ------ isds <- mapM isd fasdl
    -- let (fasdd', fasdf') = divide snd $ zip fasdl isds
    --     fasdd = fst $ unzip fasdd'
    --     fasdf = fst $ unzip fasdf'
    -- dynamicPrompt' c cmds home (absolute++relative)
    dynamicPrompt' c cmds home history hist immi final owi

dynamicPrompt' c cmds home history hist immi final owi = do
    d <- getCurrentWorkspaceDirectory
    io $ setCurrentDirectory d
    myScriptsDir <- io getMyScriptsDir
    -- only deal with directories or files at the moment; not doing check on the file / directories to save performance
    mkXPromptWithHistoryAndReturn (DPrompt $ shortend home d) c (dpromptComplFunc c cmds home hist myScriptsDir) (dpromptAction c cmds home history hist myScriptsDir immi final owi) history
    return ()

-- cycling between different dictionary
defaultDictionaries = ["sdcv-Collins", "sdcv-Moby", "sdcv-modernChinese", "sdcv-bigChinese"]
dpromptPrefices = defaultDictionaries ++ ["vb", "calc", "tk", "fmc"]
cycleDictionaryForDPrompt dir = do
    str <- getInput
    let ls = (if dir == Next then id else reverse) defaultDictionaries
        nstr = case findIndex (`isPrefixOf` str) ls of 
                    Just i -> (ls !! ((i+1) `mod` (length ls))) ++ (maybe "" ((" "++) . dropWhile isSpace) $ stripPrefix (ls !! i) str)
                    _ -> head ls ++ " " ++ removePrefix str dpromptPrefices
    setInput nstr
    endOfLine

removePrefix str pres = if null args then str
                                     else case break (`elem` pres) args of
                                               (_, _:afs) -> joinStr " " afs
                                               _ -> str
            where args = words str

addOrTruncateTillPrefix prefix = do
    str <- getInput
    if prefix `isPrefixOf` str
       then setInput prefix
       else setInput $ prefix ++ removePrefix str dpromptPrefices
    endOfLine

setInputAndDone str = setInput str >> setSuccess True >> setDone True
changeInputAndDone fun = getInput >>= setInput . fun >> setSuccess True >> setDone True
