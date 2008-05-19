
> module Main where
> import Workflow
> import Task
> import IO
> import Data.Char

> n1 = Node (-1) RequireAll defaultGuard passthrough
> n2 = Node 2 RequireAll defaultGuard (acceptAndCreateTask 1 "Hello" "Here is where one would say hello")
> n3 = Node 3 RequireAll defaultGuard (acceptAndCreateTask 2 "Introduce" "Here is where you give your name")
> n4 = Node 4 RequireAll defaultGuard (acceptAndCreateTask 3 "Shake" "Here is where you shake hands")
> n5 = Node 5 RequireAll defaultGuard (acceptAndCreateTask 4 "Pleasantry" "Here is where you say something like, 'Nice to meet you'")
> n6 = Node 6 RequireAll defaultGuard (acceptAndCreateTask 5 "Converse" "Start a conversation")

> graph = graphFromArcs
>   [ (NodeArcs n1 [] [n2, n3]),
>     (NodeArcs n2 [n1] [n6]),
>     (NodeArcs n3 [n1] [n4, n5]),
>     (NodeArcs n4 [] [n6]),
>     (NodeArcs n5 [] [n6]),
>     (NodeArcs n6 [n2,n4,n5] []) ]

> handleTask :: Task -> WfInstance [Task] -> IO (WfInstance [Task])
> handleTask task wf =
>   do putStrLn $ "Task name: " ++ (getName task)
>      putStrLn $ "Task desc: " ++ (getDesc task)
>      putStrLn $ "Task state: " ++ show (getState task)
>      case (getState task) of
>        Open -> do putStr "Would you like to complete this task (Y/N): "
>                   response <- getLine
>                   case (map (toUpper) response) of
>                     "Y" -> do newWf <- completeTask task wf
>                               putStrLn "Task Completed"
>                               return newWf
>                     otherwise -> do putStrLn "Suit yourself."
>                                     return wf
>        Complete -> do return wf

> getTask _ [] = Left "Invalid task number"
> getTask taskNumber tasks@(first:rest)
>   | taskNumber < 1  = Left "Invalid task number"
>   | taskNumber == 1 = Right first
>   | otherwise       = getTask (taskNumber - 1) rest

> showTokens []     = do return ()
> showTokens (x:xs) =
>   do putStrLn (show x)
>      showTokens xs

> processTasks wf@(WfInstance graph tokenList tasks) =
>  do putStrLn ""
>     showTaskList tasks
>     putStr "> "
>     cmd <- getLine
>     case (getCmdType cmd) of
>       ShowTokenCmd -> do showTokens tokenList
>                          processTasks wf
>       TaskCmd ->
>         case (getTask ((read cmd)::Integer) tasks) of
>           Left msg -> do putStrLn msg
>                          processTasks wf
>           Right task -> do newWf <- handleTask task wf
>                            processTasks newWf
>       BadCmd     -> do putStrLn $ cmd ++ " is not a valid command or task entry"
>                        processTasks wf

> data CmdType = ShowTokenCmd | TaskCmd | BadCmd

> getCmdType input
>   | (map (toUpper) input) == "T" = ShowTokenCmd
>   | all (isDigit) input          = TaskCmd
>   | otherwise                    = BadCmd

> main = do hSetBuffering stdout NoBuffering
>           case (startWorkflow graph []) of
>             Left msg -> putStrLn msg
>             Right wfInstanceIO -> do wf <- wfInstanceIO
>                                      processTasks wf