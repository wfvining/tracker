-- Track task completion.

import System.Environment
import System.IO
import System.Directory
import Data.Time

-- Tracker can do four things:
-- "add" -> add a task.
-- "remove" -> remove a task.
-- "view" -> show details of a task, or a synopsis of all tasks.
-- "advance" -> note progress on a task.
dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("remove", remove),
            ("view", view),
            ("advance", advance),
            ("amend", amend),
            ("undo", undo)
           ]

-- A task file name is "."++taskName++".tkr"
taskToFile :: String -> String
-- I have serious problems :-)
taskToFile = ('.':) . flip (++) ".tkr"

-- Add the specified task with the specified target. If the task
-- already exists (ie the .<taskName>.tkr file is present in the
-- current directory then no action is taken and a message is
-- displayed.
add :: [String] -> IO ()
add [taskName, target] = do
  let taskFile = taskToFile taskName
  exists <- doesFileExist taskFile
  if exists
  then putStrLn "Task already exists"
  else writeFile taskFile (target ++ "\n0\n")

-- remove a task, will prompt the user for confirmation.  A -q option
-- can be used preceding the name of the task to supress this behavior
remove :: [String] -> IO ()
remove [taskName] = do
  putStrLn $ "Removing task \"" ++ taskName ++ "\" [yes/no]"
  answer <- getLine
  if answer == "yes"
  then removeFile $ taskToFile taskName
  else putStrLn "Task saved."
remove ["-q", taskName] = do
  removeFile $ taskToFile taskName

-- take 5 is a janky way of rounding to 2 decimal places if we get
-- into the 100% complete range this will be problematic, but if
-- your tasks are that complete I think your problems are bigger than
-- mine. That's why amend exists
summarize :: String -> String
summarize str =
    (take 5 (show $ (cur / target) * 100)) ++ "% complete with " 
                                              ++ (show $ target - cur)
                                              ++ " tasks remaining."
        where (tgt:cv:_) = lines str
              cur        = read cv :: Float
              target     = read tgt :: Float

view (taskName:[]) = do
  let taskFile = taskToFile taskName
  exists <- doesFileExist taskFile
  if exists
  then do 
    contents <- readFile taskFile
    putStrLn $ summarize contents
  else putStrLn ""
view [] = viewAll

-- Show a list of available tasks.
-- An available task is on that resides in the current directory.
viewAll = do
  files <- getDirectoryContents "."
  mapM_ (\f -> do putStr "  " 
                  putStr f
                  putStr ": "
                  view [f]) $ getTasks files
      where getTasks = 
                map (tail . (\f -> reverse . (drop 4) . reverse $ f))
                    . filter isTrackerFile
            isTrackerFile ('.':fname) = take 4 (reverse fname) == "rkt."
            isTrackerFile _           = False

-- Edit a task. Takes four arguments:
-- The task name
-- A procedure that take the current number of sub-tasks and returns
-- the new number of sub-tasks.
-- A procedure that taked the current number of comletes sub-tasks
-- and returns the new number of completed sub-tasks.
-- A proocedure that takes the previous number of sub-tasks, the
-- current number of sub-tasks and returns a record of the change.
-- In the case of an advance this will be the comment.
editTask :: String -> (Float -> Float) -> (Float -> Float) ->
            (String -> String -> String) -> IO ()
editTask taskName tgtProc curProc noteProc = do
  let tf = taskToFile taskName
  handle <- openFile tf ReadMode
  (tempName, tempHandle) <- openTempFile "." (taskName ++ ".temp.tkr")
  contents <- hGetContents handle
  d <- getCurrentTime
  let (tgt:cur:updates) = lines contents
      tgt' = show $ tgtProc (read tgt :: Float)
      cur' = show $ curProc (read cur :: Float)
      date = show d
  hPutStr tempHandle $ unlines $ (tgt':cur':updates)
    ++ [(noteProc tgt tgt') ++ " " ++ date]
  hClose handle
  hClose tempHandle
  removeFile tf
  renameFile tempName tf

-- The very simplest of undo functionality. Currently only allows the
-- undoing of a single previous change. Future functionality may
-- include the ability to undo many previous changes.
undo [taskName] = do
  let tf = taskToFile taskName
  handle <- openFile tf ReadMode
  contents <- hGetContents handle
  case (words (last $ lines contents)) of
    ("Amend":was:_:is) -> 
        editTask taskName
                 (\_ -> (read was :: Float))
                 id
                 (\_ _ -> "Undo")
    ("Undo":_) -> putStrLn "Cannot undo previous undo (for the time being)."
    (adv:_) ->
        editTask taskName
                 id
                 (flip (-) (read adv :: Float))
                 (\_ _ -> "Undo")
  hClose handle

-- Note progress on a task, recording the new level of completion and
-- logging the change.
advance (taskName:amount:comment) =
  editTask
    taskName
    id
    ((+) (read amount :: Float))
    (\_ _ -> (amount ++ (' ':(unwords comment))))

-- Change the number of sub-tasks required for task completion.
--
-- The new number can be preceded by a '+' or '-' sign, indicating
-- that the adjustment should be added or subtracted from the current
-- number of sub-tasks. If there is no prefix, then the number of
-- sub-tasks is replaced by the given adjustment.
amend [taskName,('+':adj)] =
  editTask 
    taskName
    ((+) (read adj :: Float))
    id
    (\tgt tgt' -> "Amend " ++ tgt ++ " -> " ++ tgt')
amend [taskName,('-':adj)] =
  editTask
    taskName
    ((flip (-)) (read adj :: Float))
    id
    (\tgt tgt' -> "Amend " ++ tgt ++ " -> " ++ tgt')
amend [taskName,newAmt] =
  editTask
    taskName
    (\_ -> (read newAmt :: Float))
    id
    (\tgt tgt' -> "Amend " ++ tgt ++ " -> " ++ tgt')

main = do
  args <- getArgs
  case args of
    (command:args) -> do
        let (Just action) = lookup command dispatch
        action args
    [] -> do
         putStrLn "Available tasks:"
         viewAll

