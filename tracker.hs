-- Track task completion.

import System.Environment
import System.IO
import System.Directory

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
            ("amend", amend)]

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
view [] = do viewAll

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
            isTrackerFile ('.':fname) = if take 4 (reverse fname) == "rkt."
                                        then True
                                        else False
            isTrackerFile fname       = False

-- Very rough and extremely inneficient.
-- TODO: save comment.
--       timestamp.
advance (taskName:amount:comment) = do
  let tf = taskToFile taskName
  handle <- openFile tf ReadMode
  (tempName, tempHandle) <- openTempFile "." (taskName ++ ".temp.tkr")
  contents <- hGetContents handle
  let (tgt:cv:updates) = lines contents
      cur = read cv :: Float
      add = read amount :: Float
  hPutStr tempHandle $ unlines $ (tgt:(show (cur+add)):updates) 
                ++ [amount ++ (' ':(unwords comment))]
  hClose handle
  hClose tempHandle
  removeFile tf
  renameFile tempName tf

amend [taskName,newAmt] = do
  let tf = taskToFile taskName
  handle <- openFile tf ReadMode
  (tempName, tempHandle) <- openTempFile "." (taskName ++ ".temp.tkr")
  contents <- hGetContents handle
  let (cur:rest) = lines contents
  hPutStr tempHandle $ unlines ((newAmt:rest) 
                                ++ ["0 Amend " ++ cur ++ "->" ++ newAmt])
  hClose handle
  hClose tempHandle
  removeFile tf
  renameFile tempName tf

main = do
  args <- getArgs
  case args of
    (command:args) -> do
        let (Just action) = lookup command dispatch
        action args
    [] -> do
         putStrLn "Available tasks:"
         viewAll

