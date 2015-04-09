module XMonad.Vim.Prompt.DynamicPrompt
    ( 
      tagLimit
    , dynamicPrompt
    , addOrTruncateTillPrefix
    , setInputAndDone
    , changeInputAndDone
    , cycleDictionaryForDPrompt
    , parseShellArgs
    ) where

import XMonad.Prompt
import XMonad.Core
import XMonad.Prompt.Shell
import Data.Char
import Data.List
import Data.List.Split
import Control.Applicative
import Text.Regex.Posix
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.OnWindowsInserted
import XMonad.Vim.Routine
import Data.Maybe
import XMonad.Prompt.TaskPrompt
import XMonad.Prompt.RPCPrompt
import XMonad.Vim.WorkspaceDirectories
import XMonad.Util.Run
import XMonad.Vim.Tag
import XMonad.Vim.Prompt.Calc
import XMonad.Vim.Prompt.Vimb
import XMonad.Vim.Prompt.Dict
import XMonad.Vim.Constants
import XMonad.Vim.Workspaces
import XMonad.Vim.CIM
import XMonad.Vim.Term
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

try' f _ [] = Nothing
try' f b (h:l) = maybe (try' f (h:b) l) Just (f (reverse b) h l)

try:: ([a] -> a -> [a] -> Maybe b) -> [a] -> Maybe b
try = flip try' []

tryRev f l = try (\b i a -> f (reverse a) i (reverse b)) (reverse l)

-- search widgets
fasdLimit = 20
findLimit = 20
tagLimit = 20
grepLimit = 50
whichLimit = 20
historyLimit = 20
evalLimit = 20
topLimit = 40
cimLowerLimit = 20
cimUpperLimit = 50

isGrepOutput s = isOutput s && ':' `elem` s
stripGrepOutput = head . splitOn ":"
isShortcutOutput s = length s > 2 && (s!!1) == ':'
stripShortcutOutput = last . splitOn ":"
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
-- for cim
breakCIMSuggestion s = case break (=='\'') s of
                            (bef, _:ccs) | all (\c -> c >= 'a' && c <= 'z') bef, all (not . isAscii) ccs -> Just (bef, ccs)
                            _ -> Nothing
breakCIMSuggestions [] = Just []
breakCIMSuggestions (h:l)
    | Just r <- breakCIMSuggestion h, Just rs <- breakCIMSuggestions l = Just (r:rs)
    | otherwise = Nothing

data PromptWidget = PromptWidget { promptPrefix :: String
                                 , promptCommandToComplete :: String -> String
                                 , promptNextCompletion :: (String, Int) -> [String] -> (String, Int)
                                 , promptComplFunction :: XPConfig -> ComplFunction
                                 -- the prompt action takes
                                 -- immi (the X action that should be run immediately
                                 -- final (the X action that should be run after everything finished)
                                 -- the OnWindowsInserted data
                                 -- whether the user indicated the current command should be a silent one
                                 -- a re-prompt action which allows the widget to retrigger the dynamicprompt if necessary
                                                                                                                                 -- finally the string for process
                                 , promptAction :: X () -> X () -> OnWindowsInserted -> Bool -> ((XPConfig -> XPConfig) -> [String] -> X () -> X () -> OnWindowsInserted -> X ()) -> String -> X ()
                                 , promptHighlightPredicate :: String -> (String, Int) -> Bool
                                 }
-- (prefix, commandToComplete, nextCompletion, complFunc, modeAction)
dwgt p pre cf act = PromptWidget { promptPrefix = pre
                                 , promptCommandToComplete = commandToComplete p
                                 , promptNextCompletion = nextCompletion p
                                 , promptComplFunction = cf
                                 , promptAction = act
                                 , promptHighlightPredicate = highlightPredicate p
                                 }
widgetCmd w c = let p = promptPrefix w++" " in joinStr p $ tail $ splitOn p c
findWidget pre = find ((==pre) . promptPrefix) dynamicPromptWidgets

dwgtDictMode mode prompt = dwgt mode prompt (\c -> completionFunction mode) $ \immi final owi sil _ s -> do
    applyOnWindowsInserted owi {
              numberOfWindows = 1
            , logFinished = \a b -> do
                (logFinished owi) a b
                final
        }
    (modeAction mode) s "" 
    immi
dynamicPromptWidgets = [
        -- this follow the (prefix, prompt) format
        dwgt VBPrompt "vb" (\c -> vbComplFunc) (\immi final owi sil _ s -> do
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
      , dwgt RPCPrompt "rpc" rpcComplFunc rpcAction'
      , dwgt CalcMode "calc" (\_ -> completionFunction CalcMode) calcAction'
    ]

evalStr s = let evalcomps = splitOn "`" s
                evallen = length evalcomps 
                t = tail evalcomps
            in if odd evallen && evallen >= 3 then (head evalcomps, head t, joinStr "`" $ tail t)
                                              else (s,"","")

dpromptNextCompletion (c,os) l
    -- for pinyin completion
    -- the pattern .*([汉字]+|\\)[a-z]+
    -- when the chinese character has been completed as least once
    | Just sugs <- breakCIMSuggestions l
    , (bef, lat) <- splitAt os c, h:_ <- reverse bef, not (isAscii h)
      -- try to strip the part from bef where we can find an exact match
    , ((npy, nch), Just (py, rbef)) <- exactNext' (\(py, chs) -> let r = stripSuffix chs bef
                                                                 in (isJust r, Just (py, fromJust r))) sugs
    , Just aft' <- stripPrefix npy (py++lat)
    -- strip leading / so that the user can trigger again
    , aft <- fromMaybe aft' $ stripPrefix "'" aft' = let c' = rbef ++ nch ++ aft in (c', length c' - length aft)
    -- this is when first completion a chinese character
    | (bef, _:pys) <- break (=='\\') c
    , pys =~ "^[a-z]+('[a-z]+)*$"
    , (py, lat) <- break (=='\'') pys
    , Just (spy, chs) <- breakCIMSuggestion (head l)
    , Just r <- stripPrefix spy py, aft' <- r ++ lat
    -- strip leading / so that the user can trigger again
    , aft <- fromMaybe aft' $ stripPrefix "'" aft' = let c' = bef ++ chs ++ aft in (c', length c' - length aft)
    | Just a <- tryRev rwp args = a
    | Just a <- tryRev rp args = a
    | all isGrepOutput l = eof . joinStr " " $ takeWhile (\t -> t /="g" && t /= "gp") (init args) 
                                               ++ 
                                               case exactNext $ stripGrepOutput <$> l of
                                                   [] -> []
                                                   s -> [s]
    | all isShortcutOutput l = eof . exactMatch $ stripShortcutOutput <$> l
    | fsdo@(_:_) <- filter isStatDiffOutput l, length l - length fsdo <= 1 = eof . exactMatch $ stripStatDiffOutput <$> fsdo
    | fdo@(_:_) <- filterDiffOutput l = eof $ exactMatch fdo
    | (evall, evalstr@(_:_), evalr) <- evalStr c =  eof $ evall ++ escape (head l) ++ evalr
    | all isOutput l, any ("commit" `isPrefixOf`) l, any ("Author" `isPrefixOf`) l, any ("Date" `isPrefixOf`) l = eof . exactMatch $ fromJust . stripPrefix "commit " . trim <$> (filter ("commit " `isPrefixOf`) l)
    | isOutput (head l) = eof c
    | otherwise = eof $ exactMatch l
    where hl = any (flip dpromptHighlightPredicate (c, length c)) l 
          eof s = (s, length s) 
          args = parseShellArgs c
          lastArg = last $ args
          exactMatch ls = fromMaybe "" (stripSuffix lastArg c) ++ exactNext ls
          exactNext = escape . fst . exactNext' (\a -> (a == unescape lastArg, Nothing))
          exactNext' p ls = let rev = reverse ls 
                                ci [] = (rev !! (length ls - 1), Nothing)
                                ci ((i, e):_) | (True, v) <- p e = (rev !! (if i <= 0 then length ls - 1 else (i-1)), v)
                                ci (_:l) = ci l
                            in ci $ zip [0..] rev
          -- reverse widget processing
          rwp bef w _ | Just wp <- findWidget w = let (wdg, wof) = promptNextCompletion wp (wcmd, length wcmd - length c + os) l
                                                      wcmd = widgetCmd wp c
                                                      diff = length wdg - wof
                                                      fc = joinStr " " $ bef ++ [w, wdg]
                                                  in Just (fc, length fc - diff) 
          rwp _ _ _ = Nothing
          -- other reverse processing
          rp bef "z" _ | not hl = Just . eof . joinStr " " $ bef ++ ["c", escape (head l)]
          rp _ ['\'', _] [] = Just . eof . exactMatch $ fmap stripShortcutOutput l
          rp bef "which" _ | all ((`elem` "~/") . head) l, not hl = Just . eof . joinStr " " $ bef ++ [escape (head l)]
                           | otherwise = Just . eof $ exactMatch l
          rp bef "diff" _ | fdo@(_:_) <- filterDiffOutput l = Just . eof . joinStr " " $ (case reverse bef of
                                                                                              "git":bef' -> init bef
                                                                                              _ -> bef) ++ [escape (head fdo)]
          rp bef f _ | f `elem` ["f", "a", "d", "h", "l", "t", "which"], not hl = Just . eof . joinStr " " $ bef ++ [escape (head l)]
          rp _ _ _ = Nothing

dpromptHighlightPredicate cl (cmd,os)
    -- CIM highlight
    | Just (py, chs) <- breakCIMSuggestion cl = isJust $ stripSuffix chs (take os cmd)
    | Just p <- tryRev rwp args = p
    | isGrepOutput cl = stripGrepOutput cl == unescapedLastArg
    | isShortcutOutput cl = stripShortcutOutput cl == unescapedLastArg
    | not (null unescapedLastArg), ("--- a/"++unescapedLastArg) `isPrefixOf` cl = True
    | isStatDiffOutput cl = stripStatDiffOutput cl == unescapedLastArg
    | trimcl <- trim cl, trimcl =~ "commit [0-9a-z]{40}" = last (words trimcl) == unescapedLastArg
    | isOutput cl = False
    | otherwise = not (null args) && unescapedLastArg == cl 
    where args = parseShellArgs cmd
          unescapedLastArg = unescape $ last args 
          -- reverse widget process
          rwp _ w _ | Just wp <- findWidget w, wcmd <- widgetCmd wp cmd = Just $ promptHighlightPredicate wp cl (wcmd, length wcmd - length cmd + os)
          rwp _ _ _ = Nothing

trycmp [] = return []
trycmp (h:l) = do
    hl <- h
    if not (null hl)
       then return hl 
       else trycmp l

dpromptComplFunc c cmds home hist cimdb precompl myScriptsDir s
    | null s, _:_ <- precompl = return precompl
    | (bef, _:pys) <- break (=='\\') s
    , pys =~ "^[a-z]+('[a-z]+)*$"
    , (py, _) <- break (=='\'') pys = do
        -- complete the pinyin by procedurally trying backwards
        let trypy _ "" = return []
            trypy limit w = do
                l <- io $ wordList cimdb w
                let r = zip (repeat w) l
                    len = length l
                if len < limit then return . (r++) =<< trypy (limit-len) (init w)
                               else return r
        fmap (\(w, c) -> w++"'"++c) . nubBy (\(_, a) (_, b) -> a == b) . take cimUpperLimit <$> trypy cimLowerLimit py
    | Just a <- tryRev rwp args = a
    | Just a <- tryRev rp args = a
     -- grave key evaluation (evaluate the grave enclosed string in shell and show the output as autocompletion)
    | (_, evalstr@(_:_), _) <- evalStr s = 
        fmap sht . take evalLimit . lines <$> (runEvaluation $ "loader " ++ escapeQuery evalstr)
    | otherwise = p unescapedArgs 
    where args = parseShellArgs s
          unescapedArgs = map unescape args
          sht = shortend home
          epd = expandd home
          sp = searchPredicate c
          fasd args = fmap sht . lines <$> (runProcessWithInput (myScriptsDir++"/xfasd") (show fasdLimit:args) "")
          sct pre = lines <$> (runProcessWithInput (myScriptsDir++"/xshortcut") [ "print", pre ] "")
          ntailsp = length $ takeWhile isSpace (reverse s)
          output pro ags = map (limitSpace outputWidth . sht) . lines <$> (runProcessWithInput pro ags "")

          -- commands
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
          shellcmp' sas scs = fmap sht <$> (getShellCompl' False sas scs $ epd $ if null args then "" else last args)
          grepcmp' cmd aft = filter (':' `elem`) <$> (output (myScriptsDir++"/"++cmd) $ show grepLimit:(epd <$> aft))
          grepcmp = grepcmp' "xgrep"
          pgrepcmp = grepcmp' "xpdfgrep"

          -- the actual reverse process function (for widget)
          rwp _ w _ | Just wp <- findWidget w = Just $ (promptComplFunction wp) c (widgetCmd wp s)
          rwp _ _ _ = Nothing
          -- the actual reverse process function (for non widget)
          -- history
          rp _ "h" afs = Just . return $ take historyLimit $ filter (sp $ joinStr " " $ unescape <$> afs) $ hist
          -- marking interface
          rp [] ('\'':pre) (_:_) = Just $ shellcmp' ["directory"] []
          rp _ ('\'':pre) [] = Just $ sct pre
          -- find
          rp _ "l" afs = Just $ fmap sht . lines <$> (runProcessWithInput (myScriptsDir++"/xfind") (show findLimit:(fmap epd $ unescape <$> afs)) "")
          -- for g, we demand that at least SOME search term is entered, otherwise we don't generate the necessary output
          rp _ "g" (af:afs) = Just . trycmp $ (if afs == [] then [shellcmp' ["file"] []] else []) 
                                              ++ 
                                              [grepcmp (unescape <$> af:afs)]
          rp _ "gp" [af] = Just $ shellcmp' ["file"] []
          rp _ "gp" afs' | "":_:_ <- reverse afs' = Just . pgrepcmp $ unescape <$> afs'
          -- tag
          rp _ "t" afs = Just $ getTagBin >>= \tagBin -> fmap (tagLibroot++) . lines <$> (runProcessWithInput tagBin ([show tagLimit, "false"] ++ tagQuery (unescape <$> afs)) "")
          -- which
          rp _ "which" afs | cas <- unescape <$> afs = Just $ trycmp [fmap sht . lines <$> (runProcessWithInput "which" cas ""), 
                                                                      return . fmap sht . take whichLimit $ filter (sp $ joinStr " " cas) cmds]
          -- we need to make sure that we are using the current directory as a starting point
          rp _ "diff" afs | ntailsp <= 1 = Just $ trycmp [output (myScriptsDir++"/xdiff") (show outputWidth:show outputHeight:diffargs), 
                                                          shellcmp' ["file"] []]
              where diffargs = case afs of
                                [""] -> [] 
                                _ -> unescape <$> afs
          rp _ _ _ = Nothing

          gitcmdcmp a = return $ filter (sp a) ["add", "am", "archive", "bisect", "branch", "bundle", "checkout", "cherry-pick", "citool", "clean", "clone", "commit", "describe", "diff", "fetch", "format-patch", "gc", "grep", "gui", "init", "log", "merge", "mv", "notes", "pull", "rebase", "reset", "rm", "shortlog", "show", "stash", "status", "submodule", "tag"]
          -- the actual forward process function
          p ["man", a, ""] = output (myScriptsDir++"/xman") [show outputWidth, show outputHeight, a]
          p ["du", a, ""] = output (myScriptsDir++"/xdu") [show outputHeight, a]
          p ["ip", a] = return $ filter (sp a) ["addr", "addrlabel", "link", "maddr", "mroute", "neigh", "route", "rule", "tunnel"]
          p ["ip", a, ""] = output "ip" [a]
          p ["top", ""] | ntailsp == 1 = output (myScriptsDir++"/xtop") [show topLimit]
          p ["free", ""] | ntailsp == 1 = output "free" []
          p ["ifconfig", ""] | ntailsp == 1 = output "ifconfig" []
          -- git command
          p ("git":gitcmds) | gitcmds `elem` [[""], ["status", ""]], ntailsp == 1 = trycmp [output "git" ["status"], gitcmdcmp ""]
          -- only gives the prime command completion on two spaces
          p ["git", a] = gitcmdcmp a
          -- in all other instances we should give the log information
          p as@("git":_:_) = trycmp [output "git" $ ["log", "--grep", last as], scopecmp, shellcmp]
          p _ = trycmp [scopecmp, shellcmp]

cmdsWithGUI = ["chromium", "firefox", "xterm", "xeval", "retroarch", "gimp", "inkscape", "libreoffice", "xvim", "xmutt", "zathura", "vimb", "vb", "intellij-idea-ultimate-edition", "gvim", "fba", "gba"]
dpromptAction c cmds home hist cimdb myScriptsDir immi final owi history str = 
    -- perform some special actions on some commands
    let (silent, args, s) = case parseShellArgs str of
                                "sil":res -> (True, res, joinStr " " res)
                                as -> (False, as, str)
        follow = immi >> final
    in case args of
        ha:pas | Just w <- find ((==ha) . promptPrefix) dynamicPromptWidgets -> (promptAction w) immi final owi silent (\ch pre immi final owi -> dynamicPrompt' (ch c) cmds home history hist cimdb pre immi final owi) $ joinStr " " pas
        ['\'',pre]:pa:_ -> spawn (myScriptsDir++"/xshortcut mark "++[pre]++" "++pa) >> follow
        "diff":pas -> runTerm "vimdiff" "vimdiff" $ "loader vimdiff " ++ joinStr " " pas
        "reboot":_ -> removeAllWorkspaces >> spawn "reboot"
        "systemctl":"poweroff":_ -> removeAllWorkspaces >> spawn "systemctl poweroff"
        ha:pas | ha `elem` ["cd", "c", "z"] -> chdir (if null pas then "" else unescape (head pas))
               | otherwise -> do
                   let uha = expandd home (unescape ha)
                   -- we need to determine the sort of thing to do with the dphandler
                   fe <- io (doesFileExist uha)
                   de <- io (doesDirectoryExist uha)
                    -- if the directory exists then we try to cd inside
                   if de || validDirShortcut uha
                      then chdir uha
                      else do
                          let s' = if null (trim s) then "." else s
                              run = myScriptsDir++"/dphandler" ++" "++s'
                          -- if the file does not exist but it is not one of the commands
                          -- then we can pretty much assume that it's a new file to be created
                          if fe || not (uha `elem` cmds) || ha `elem` cmdsWithGUI
                             then do
                                  applyOnWindowsInserted owi {
                                            numberOfWindows = 1
                                          , logFinished = \a b -> do
                                              (logFinished owi) a b
                                              final
                                      }
                                  spawn run >> immi
                             else do
                                 -- we check if the user has silent as the first argument
                                 if silent then spawn run >> follow
                                           else do precompl <- map (limitSpace outputWidth) . lines <$> (runEvaluation run)
                                                   dynamicPrompt' c cmds home history hist cimdb precompl immi final owi
               where chdir d = do
                         dir <- if null d
                                   then return home
                                   else case d of
                                             "-" -> getCurrentWorkspaceOldDirectory
                                             p -> return p
                         saveCurrentWorkspaceDirectory dir
                         dynamicPrompt' c cmds home history hist cimdb [] immi final owi
                     validDirShortcut d = d `elem` ["-"]

divide' _ [] (r, w) = (reverse r, reverse w)
divide' p (x:xs) (r, w) = divide' p xs $ if p x then (x:r, w) else (r, x:w)
divide p l = divide' p l ([],[])

dynamicPrompt c cimdb immi final owi = do
    cmds <- io getCommands
    home <- io $ env "HOME" "~"
    history <- io readHistory
    let histp = (=~ "^~?/")
        histf a z = (filter histp a) ++ z
        hist = unescape <$> (nub $ sort $ M.foldr histf [] history)
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
    -- c precompl immi final owi
    dynamicPrompt' c cmds home history hist cimdb [] immi final owi

dynamicPrompt' c cmds home history hist cimdb precompl immi final owi = do
    d <- getCurrentWorkspaceDirectory
    io $ setCurrentDirectory d
    myScriptsDir <- getScriptsDir
    -- only deal with directories or files at the moment; not doing check on the file / directories to save performance
    mkXPromptWithHistoryAndReturn (DPrompt $ shortend home d) c (dpromptComplFunc c cmds home hist cimdb precompl myScriptsDir) (dpromptAction c cmds home hist cimdb myScriptsDir immi final owi) history
    return ()

-- cycling between different dictionary
defaultDictionaries = ["sdcv-Collins", "sdcv-Moby", "sdcv-bigChinese", "sdcv-modernChinese"]
dpromptPrefices = defaultDictionaries ++ ["vb", "calc", "tk", "rpc"]
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
