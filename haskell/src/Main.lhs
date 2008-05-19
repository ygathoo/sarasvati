
> module Main where
> import Workflow

> n1 = Node (-1) passthrough
> n2 = Node 2 passthrough
> n3 = Node 3 passthrough
> n4 = Node 4 passthrough
> n5 = Node 5 passthrough
> n6 = Node 6 passthrough

> graph = graphFromArcs
>   [ (NodeArcs n1 [] [n2, n3]),
>     (NodeArcs n2 [n1] [n6]),
>     (NodeArcs n3 [n1] [n4, n5]),
>     (NodeArcs n4 [] [n6]),
>     (NodeArcs n5 [] [n6]),
>     (NodeArcs n6 [n2,n4,n5] []) ]

> main = do case (startWorkflow graph) of
>             Left msg -> putStrLn msg
>             Right tokenListIO -> putStrLn "Workflow started"