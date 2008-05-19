
> module Main where
> import Workflow
> import Task
> import IO
> import Data.Char
> import System.Directory

> n1 = Node StartNodeId RequireAll defaultGuard completeExecution
> n2 = Node (NodeId 2) RequireAll defaultGuard (acceptAndCreateTask 1 "Hello" "Here is where one would say hello")
> n3 = Node (NodeId 3) RequireAll defaultGuard (acceptAndCreateTask 2 "Introduce" "Here is where you give your name")
> n4 = Node (NodeId 4) RequireAll defaultGuard (acceptAndCreateTask 3 "Shake" "Here is where you shake hands")
> n5 = Node (NodeId 5) RequireAll defaultGuard (acceptAndCreateTask 4 "Pleasantry" "Here is where you say something like, 'Nice to meet you'")
> n6 = Node (NodeId 6) RequireAll (\x y -> SkipNode) completeExecution
> n7 = Node (NodeId 7) RequireAll defaultGuard (acceptAndCreateTask 5 "Converse" "Start a conversation")

> graph = graphFromArcs
>   [ (NodeArcs n1 [] [n2, n3]),
>     (NodeArcs n2 [n1] [n7]),
>     (NodeArcs n3 [n1] [n4, n5]),
>     (NodeArcs n4 [n3] [n7]),
>     (NodeArcs n5 [n3] [n6]),
>     (NodeArcs n6 [n5] [n7]),
>     (NodeArcs n7 [n2,n4,n6] []) ]

> handleTask :: Task -> WfInstance [Task] -> IO (WfInstance [Task])
> handleTask task wf =
>     do putStrLn $ "Task name: " ++ (getName task)
>        putStrLn $ "Task desc: " ++ (getDesc task)
>        putStrLn $ "Task state: " ++ show (getState task)
>        case (getState task) of
>            Open -> do putStr "Would you like to complete this task (Y/N): "
>                       response <- getLine
>                       case (map (toUpper) response) of
>                           "Y" -> do newWf <- completeTask task wf
>                                     putStrLn "Task Completed"
>                                     return newWf
>                           otherwise -> do putStrLn "Suit yourself."
>                                           return wf
>            Complete -> do return wf

> getTask _ [] = Left "Invalid task number"
> getTask taskNumber tasks@(first:rest)
>     | taskNumber < 1  = Left "Invalid task number"
>     | taskNumber == 1 = Right first
>     | otherwise       = getTask (taskNumber - 1) rest

> showTokens []     = do return ()
> showTokens (x:xs) =
>     do putStrLn (show x)
>        showTokens xs

> processTasks wf@(WfInstance graph [] tasks) =
>     do putStrLn "Workflow complete!"

> processTasks wf@(WfInstance graph tokenList tasks) =
>     do putStrLn ""
>        showTaskList tasks
>        putStr "> "
>        cmd <- getLine
>        case (getCmdType cmd) of
>            ShowTokenCmd -> do showTokens tokenList
>                               processTasks wf
>            TaskCmd ->
>                case (getTask ((read cmd)::Integer) tasks) of
>                    Left msg -> do putStrLn msg
>                                   processTasks wf
>                    Right task -> do newWf <- handleTask task wf
>                                     processTasks newWf
>            BadCmd -> do putStrLn $ cmd ++ " is not a valid command or task entry"
>                         processTasks wf

> data CmdType = ShowTokenCmd | TaskCmd | BadCmd

> getCmdType input
>     | (map (toUpper) input) == "T" = ShowTokenCmd
>     | all (isDigit) input          = TaskCmd
>     | otherwise                    = BadCmd

> main =
>     do hSetBuffering stdout NoBuffering
>        case (startWorkflow graph []) of
>            Left msg -> putStrLn msg
>            Right wfInstanceIO -> do wf <- wfInstanceIO
>                                     processTasks wf

> selectWorkflow wfList =
>     do showWorkflows wfList 1
>        putStr "Select workflow: "
>

> showWorkflows [] _ = do return ()
> showWorkflows (wf:rest) counter =
>  do putStrLn $ show counter ++ ": " ++ wf
>     showWorkflows rest (counter + 1)

> getWorkflowList =
>     do fileList <- getDirectoryContents wfDir
>        return $ (useFullPath.filterWfs) fileList
>   where
>     wfDir = "/home/paul/workspace/functional-workflow/test-wf/"
>     filterWfs = (filter (hasExtension ".wf"))
>     useFullPath = (map (\f->wfDir ++ f))

> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)