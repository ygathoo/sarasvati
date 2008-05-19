
> module Main where
> import Workflow

> n1 = Node 1 acceptFork
> n2 = Node 2 acceptP
> n3 = Node 3 acceptFork
> n4 = Node 4 acceptP
> n5 = Node 5 acceptP
> n6 = Node 6 acceptJoin

> graph = graphFromArcList [1..6]
>   [ (NodeArcs n1 [] [n2, n3]),
>     (NodeArcs n2 [n1] [n6]),
>     (NodeArcs n3 [n1] [n4, n5]),
>     (NodeArcs n4 [] [n6]),
>     (NodeArcs n5 [] [n6]),
>     (NodeArcs n6 [n2,n4,n5] []) ]

> main =
>   do putStrLn "Hello"
