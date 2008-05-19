
> module Workflow where

> data Token =
>   Token {
>     tokenId::[Int],
>     node::Node
>  }
>  deriving (Show)

> data NodeType = PassThrough | Fork | Join

> data Node =
>   Node {
>     name    :: String,
>     nodeType:: NodeType,
>     inputs  :: [Node],
>     outputs :: [Node]
>   }

> instance Show Node where
>   show a = "[Node outputs" ++ (show (outputs a)) ++ "]"

> removeToken token [] = []
> removeToken token (x:xs)
>    | (tokenId token) == (tokenId x) = xs
>    | otherwise = x : removeToken token xs

> accept :: Token -> [Token] -> Node -> [Token]
> accept token tokenList (Node name PassThrough inputs []) = removeToken token tokenList
> accept token tokenList (Node name PassThrough inputs [x]) =
>    accept newToken newTokenList x
>  where
>    newToken = Token (tokenId token) x
>    newTokenList = newToken : removeToken token tokenList