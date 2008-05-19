
> module Task where
> import Workflow

> data TaskState = Open | Complete
>  deriving Show

> data Task =
>   Task {
>     getTokId   :: [Int],
>     getTaskId  :: String,
>     getName    :: String,
>     getDesc    :: String,
>     getState   :: TaskState
>   }

> showTaskList tasks =
>   do putStrLn "Tasks:"
>      if (null tasks)
>        then putStrLn "  No tasks to display"
>        else showTasks tasks 1

> showTasks [] _ = do return ()
> showTasks (task:rest) counter =
>  do putStrLn $ show counter ++ ": " ++ (getName task) ++ " - " ++ show (getState task)
>     showTasks rest (counter + 1)

> acceptAndCreateTask taskId name desc token wf@(WfInstance graph tokenList tasks) =
>   do return $ WfInstance graph tokenList ((newTask token taskId name desc):tasks)

> newTask token taskId name desc = Task (tokenId token) taskId name desc Open

> closeTask task@(Task tokId taskId name desc state) (WfInstance graph tokenList tasks) =
>     WfInstance graph tokenList newTaskList
>   where
>     newTaskList = map (\t-> if (getTaskId t == getTaskId task)
>                               then closedTaskInstance task
>                               else t ) tasks
>

> closedTaskInstance task@(Task tokId taskId name desc state) = Task tokId taskId name desc Complete

> completeTask task wf = completeExecution token (closeTask task wf)
>   where
>     token = getTokenForId (getTokId task) wf