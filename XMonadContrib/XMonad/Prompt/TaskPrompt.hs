module XMonad.Prompt.TaskPrompt 
    ( mkTaskPrompt
    , TaskWarriorPrompt(..)
    , taskAction
    , taskAction'
    , taskComplFunc
    ) where

import XMonad.Prompt
import XMonad.Core
import Text.Regex.Posix
import Data.Char
import Data.List 
import Data.List.Split
import Data.Maybe
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog (trim)
import XMonad.Hooks.OnWindowsInserted

---- some constants here
outputWidth = 200

-------------- command and filter definitions
---- new commands added in: up, upcoming
taskListCmds = ["active", "all", "blocked", "blocking", "burndown.daily", "burndown.monthly", "burndown.weekly", "calendar", "colors", "columns", "completed", "count", "diagostics", "help", "history.annual", "history.monthly", "ids", "information", "lon", "long", "list", "ls", "minimal", "up", "upcoming", "newest", "oldest", "overdue", "projects", "ready", "next", "recurring", "reports", "show", "stats", "summary", "tags", "timesheet", "udas", "unblocked", "uuids", "version", "waiting"]

---- new filter command: open
taskFilterCmds = ["active", "all", "blocked", "blocking", "burndown.daily", "burndown.weekly", "burndown.monthly", "completed", "count", "export", "ghistory.annual", "ghistory.monthly", "history.annual", "history.monthly", "ids", "uuids", "information", "list", "long", "ls", "minimal", "up", "upcoming", "newest", "next", "ready", "oldest", "overdue", "projects", "recurring", "unblocked", "waiting", "annotate", "append", "delete", "denotate", "done", "duplicate", "edit", "modify", "open", "prepend", "start", "stop", "_urgency"]

---- new action command: appoint, note, open
taskActionCmds = ["add", "annotate", "append", "note", "appoint", "config", "delete", "denotate", "done", "duplicate", "edit", "execute", "export", "import", "log", "merge", "modify", "open", "prepend", "pull", "push", "shell", "start", "stop", "undo"]

taskOtherCmds = ["_aliases", "_columns", "_commands", "_config", "_ids", "_projects", "_show", "_tags", "_udas", "_uuids", "_version", "_zshcommands", "_zshids", "_zshuuids"]

taskReviewCmds = ["modify"]

taskAttribs = ["description", "project","priority","due","recur","scheduled", "until", "limit", "wait", "depends", "entry"]
taskDateAttribs = ["due", "until", "scheduled", "wait", "entry"]
taskModifiers = ["before", "after", "none", "any" , "is" , "isnt", "has", "hasnt", "startswith", "endswith", "word", "noword"]

taskVirtualTags = [ "BLOCKED", "UNBLOCKED", "BLOCKING", "DUE", "DUETODAY", "TODAY", "OVERDUE", "ACTIVE", "SCHEDULED", "CHILD", "UNTIL", "WAITING", "ANNOTATED" ]    
---- new virtual tag: APPOINTMENT
taskVirtualCustomTags = [ "APPOINTMENT" ]

--- also there is the need to get tags / ids, if necessary, for example

taskCompatibleDateExps = ["now", "today", "yesterday", "tmr", "tomorrow" , "Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"]
taskOnlyDateExps = ["sow", "soww", "socw", "som", "soq", "soy", "eow", "eoww", "eocw", "eom", "eoq", "eoy"]
taskAllDateExps = taskCompatibleDateExps ++ taskOnlyDateExps

taskDateSuffix = ["days"]
taskTimeSuffix = ["am", "pm"]
taskDurSuffix = ["hr", "min"]

data TaskWarriorPrompt = TaskPrompt

-- id related functions
isFilledOutput s = length s >= outputWidth
--- given the output stream, extract the lines that are relevant to the tasks
taskLines :: [String] -> [String]
{-taskLines (x:y:xs) -}
    {-[>| x =~ "^ *ID" && y =~ "^[- ]*-[- ]*$" = filter isTaskIdLine $ fst $ break (all isSpace) xs<]-}
    {--- a more stripped down version / useful for uuid completion-}
    {-| x =~ "^ *(ID|Completed" && y =~ "^[- ]*-[- ]*$" = filter (\l -> isTaskIdLine l || isUUIDLine l) $ fst $ break (all isSpace) xs-}
    {-| otherwise = taskLines (y:xs)-}
{-taskLines _ = []-}
taskLines = filter (\l -> isTaskIdLine l || isUUIDLine l) 
---- assuming l is already identified as some output
lastId c = breakAt (\c -> c `elem` ",: ") c  
isTaskIdLine l = (l =~ "^\\s*[0-9]+ +.*[a-zA-Z]+") && not (l =~ "^\\s*[0-9]+ tasks?\\s*$")
taskIdFromLine = head . words

-- open line related functions
isOpenLine l = l =~ "^[0-9]+\\) .* \\(\".*\"\\)$"
indexFromOpenLine l = reverse $ tail $ reverse $ head $ words l
parseOpen args = do fmap lines $ runProcessWithInput "~/.tk/tkparseopen_compl" args ""    

-- uuid line related functions
isUUIDLine :: String -> Bool
isUUIDLine l = not (null $ uuidFromLine l)
uuidFromLine :: String -> String
uuidFromLine l = l =~ "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"

-- executions
taskExecute args = do fmap (map (fillSpace outputWidth) . lines) $ runProcessWithInput "tk" args ""   
filterReturnFromFixedList attribute value list = return $ map ((attribute++) . (':' :))  $ filter (wordInside value) list
filterReturnFromTaskProcess attribute value args = do fmap (map  ((attribute++) . (':' :)) . filter (wordInside value) . lines) $ runProcessWithInput "tk" args "" 


instance XPrompt TaskWarriorPrompt where
    showXPrompt TaskPrompt = "task > "
    commandToComplete TaskPrompt = id
    completionToCommand TaskPrompt s 
        | isFilledOutput s = if isTaskIdLine s then taskIdFromLine s else if isUUIDLine s then uuidFromLine s else ""
        | isOpenLine s = indexFromOpenLine s
        | otherwise = s
    nextCompletion TaskPrompt c l
        | isFilledOutput headline = case tasklines of
                [] -> c
                _ | isTaskIdLine headTaskLine -> behinds ++ (argMatch nearest (head . words) tasklines)
                  | isUUIDLine headTaskLine -> behinds ++ (argMatch nearest uuidFromLine tasklines)
                  | otherwise -> c
        | isOpenLine headline = skipLastWord c ++ (argMatch lastArg indexFromOpenLine l)
        | otherwise = skipLastWord c ++ (argMatch lastArg id l)
                 where lastArg = last $ parseArgs c
                       headline = head l
                        -- for task id matching
                       tasklines = taskLines l
                       headTaskLine = head tasklines
                       (behinds , nearest) = lastId c  
                       argMatch arg transArg ls = transArg  $ ls !! case findIndex ((== arg) . transArg) ls of
                                                       Just i -> if i >= length ls - 1 then 0 else i+1
                                                       Nothing -> case findIndex (wordInside arg) ls of
                                                                       Just fi -> fi
                                                                       Nothing -> 0
    highlightPredicate TaskPrompt cl cmd 
        | isFilledOutput cl = if isTaskIdLine cl then lastVar == taskIdFromLine cl else if isUUIDLine cl then lastVar == uuidFromLine cl else False
        | isOpenLine cl = lastArg == indexFromOpenLine cl
        | otherwise = lastArg == cl
                  where lastArg = last $ parseArgs cmd
                        taskId = head $ words cl
                        (behinds , lastVar) = lastId cmd  

taskAction' immi final owi s = 
    let run = spawn $ "export EDITOR='xvim'; tk "++listCmdStrippedStr s++if noActionCmd then " open" else []
        wait = noActionCmd || head cmdAfter `elem` ["open", "note"]
    in if wait 
          then applyOnWindowsInserted owi {
                  numberOfWindows = 1
                , logFinished = \a b -> do
                    (logFinished owi) a b
                    final
            } >> run >> immi
          else run >> immi >> final
    where noActionCmd = null cmdAfter
          (filters, cmdAfter) = break (\e -> e `elem` taskActionCmds++taskOtherCmds) $ parseArgs s
-- check for the presence of action command, if there's no action command defaults to open the task
-- we will strip away the list commands at all times. Reason: to support dynamic viewing, when you switch to a new view (through one of the list command), you can continue through the list and no need to delete back (essentially list commands become another kind of filter)
taskAction = taskAction' (return ()) (return ()) def

listCmdStrippedStr c = tail $ foldr ((++) . (" "++)) [] (listCmdStrippedArgs $ parseArgs c)

listCmdStrippedArgs args = listBefore ++ if null listAfter then [] else tail listAfter
    where (listBefore, listAfter) = break (\e -> e `elem` taskListCmds) $ args

taskComplFunc s 
    ---- we've already had our action / other commands, we only need to provide filter / modifier completion
    | not $ null cmdAfter = 
        -- first determine if there's an open command
        if head cmdAfter == "open" 
           then case length cmdAfter of
                     -- we have our previous word being 'open' -> should return the options now
                     2 -> parseOpen $ listCmdStrippedArgs filters
                     _ -> return []
           else do
                cps <- taskFilterCompl args True
                {-if null cps && (last cmdBehinds) `elem` taskReviewCmds then taskExecute filters else return cps-}
                -- the regex enables the review capability (unless there are two spaces at the end otherwise it will display the filtered results, review mode
                if null cps || s =~ "[^ ]+ $" then taskExecute filters else return cps
    ---- If we've had one of the list commands, then straightforward returning the output
    {-| findWord args $ taskListCmds = taskExecute args-}
    ---- If the user has not typed anything, or there's filter but there's only one space at the end
    | null s || s =~ "[^ ]+ $" = taskExecute args
    ---- this is when the user has typed in some words in the filtering phase
    {-| (length $ trim $ last args) /= 0 = do -}
    | otherwise = do
            filterCompls <- taskFilterCompl args False 
            if null filterCompls then taskExecute args else return filterCompls
    ---- otherwise just execute it
    {-| otherwise = taskExecute args-}
            where args = parseArgs s
                  (filters, cmdAfter) = break (\e -> e `elem` taskActionCmds++taskOtherCmds) args

-- given the string on the command line return the autocompletion modifiers for the last word in the string; hasCmd specifies whether there's already a command word found in the prompt line
taskFilterCompl :: [ String ] -> Bool -> IO [ String ]
taskFilterCompl args hasCmd = 
    let lastWord = last args 
        attribAndValue = splitOn ":" lastWord 
        attrib = head attribAndValue 
        attribFields = splitOn "." attrib 
        attribName = head attribFields 
        nameIsPrefixOf = isPrefixOf attribName 
        value = last attribAndValue in
        case length attribAndValue of
             1 -> case length attribFields of
                       2 -> return $ map ((attribName++) . ('.' :)) $ filteredListContainingWord (last attribFields) taskModifiers
                       --- want to return attributes as well as commands because we can't be sure!
                       1 | attribName =~ "^[+-]" -> if hasCmd 
                                                       then do fmap (map (head attribName :) . filter (wordInside (tail attribName)) . (++taskVirtualTags++taskVirtualCustomTags) . filter (\t -> not $ t `elem` taskVirtualCustomTags) . lines) $ runProcessWithInput "tk" ["_tags"] "" 
                                                       else do fmap (map (head attribName :) . filter (wordInside (tail attribName)) . lines) $ runProcessWithInput "tk" ((init args)++["_tags"])  "" 
                        -- for the case of <project id>,
                         | attribName =~ "^[0-9]+," -> taskExecute []
                        -- if there's command then return the attributes, otherwise if there's filter, return the actions, otherwise return list commands and attributes
                         | otherwise -> return $ filter (wordPrefixed attribName) $ if hasCmd then taskAttribs 
                                                                                              else if not (null args) && not (null $ filter (\a -> not $ null a) (init args)) then taskAttribs++taskFilterCmds 
                                                                                              else taskListCmds++taskActionCmds++taskAttribs
                       _ -> return []
             2 | nameIsPrefixOf "project" -> filterReturnFromTaskProcess attrib value $ (if hasCmd then [] else init args)++["_projects"] 
               | nameIsPrefixOf "priority" -> filterReturnFromFixedList attrib value ["H", "M", "L"]
               | nameIsPrefixOf "depends" -> taskExecute []
               | isJust $ find nameIsPrefixOf taskDateAttribs ->
                   -- want to first be sure if there's "@" inside, if yes, then that's a date format string and will be dealt separately
                   let dateAndTime = splitOn "@" value 
                       generalDate = head dateAndTime in
                       case length dateAndTime of
                            1 -> if generalDate =~ "^[0-9]+" 
                                    then let (n, suffix) = partition isDigit generalDate in
                                             return $ map ((attrib++) . (':' :) . (n++)) $ filteredListContainingWord suffix $ taskDateSuffix++taskDurSuffix
                                    else return $ map ((attrib++) . (':' :)) $ filteredListContainingWord generalDate taskAllDateExps
                            2 -> let time = last dateAndTime in
                                     if time =~ "^[0-9]+"
                                        then let (n, suffix) = partition isDigit time in
                                                 return $ map ((attrib++) . (':' :) . (generalDate++) . ('@' :) . (n++)) $ filteredListContainingWord suffix taskTimeSuffix 
                                        else return []
                            _ -> return []
             _ -> return []



---- helpers
parseArgs s = splitOn " " s
filteredListContainingWord word =  filter (wordInside word) 
sl = map toLower
wordInside word s = isInfixOf (sl word) $ sl s
wordPrefixed word s = isPrefixOf (sl word) $ sl s
breakAt condition str = (behinds, nearest)
    where fields = break condition (reverse str)
          nearest = reverse $ fst fields
          behinds = reverse $ snd fields


mkTaskPrompt c = mkXPrompt TaskPrompt c taskComplFunc taskAction
