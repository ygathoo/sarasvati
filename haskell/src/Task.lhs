
> module Task where
> import Workflow

> data Task =
>   Task {
>     token :: Token,
>     name  :: String,
>     desc  :: String
>   }

> completeTask task graph tokenList = completeExecution (token task) graph tokenList