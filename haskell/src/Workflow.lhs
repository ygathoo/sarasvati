
> module Workflow where

> data Token =
>   Token {
>     node::Node
>  }
>  deriving (Show)

> data NodeType = PassThrough | Fork | Join

> data Node =
>   Node {
>     nodeType::NodeType,
>     outputs::[Node]
>   }

> instance Show WFNode where
>   show a = "[Node outputs" ++ (show (outputs a)) ++ "]"

> passthrough node [] = []

> passthrough node (x:xs) =