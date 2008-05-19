
> module Workflow where

> data Token =
>   Token {
>     tokenId::[Integer],
>     currNode::Node,
>     prevNode::Node
>  }
>  deriving (Show)

> instance Eq Token where
>   t1 == t2 = (tokenId t1) == (tokenId t2)

> data Node =
>   Node {
>     nodeId  :: Integer,
>     inputs  :: [Node],
>     outputs :: [Node],
>     accept  :: (Token->[Token]->[Token])
>   }

> instance Eq Node where
>   node1 == node2 = (nodeId node1) == (nodeId node2)

> instance Show Node where
>   show a = "[Node outputs" ++ (show (outputs a)) ++ "]"

> removeToken token [] = []
> removeToken token (first:rest)
>    | token == first = rest
>    | otherwise = first : removeToken token rest

> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>   | predicate x = xs
>   | otherwise = x : (removeFirst predicate xs)

> nextId (Token (t:rest) _ _ ) = (t+1):rest

> nextForkId (Token (t:rest) _ _ ) counter = ((t+1):rest) ++ [counter]

> acceptP :: Token -> [Token] -> [Token]
> acceptP token tokenList
>   | null $ outputs $ currentNode = tokenList
>   | otherwise = (accept output) newToken tokenList
>  where
>    newToken = Token (nextId token) output currentNode
>    currentNode = currNode token
>    output = head $ outputs currentNode

> acceptF :: Token -> [Token] -> [Token]
> acceptF token tokenList
>   | null outNodes = tokenList
>   | otherwise = split outNodes tokenList 0
>   where
>     currentNode = currNode token
>     outNodes = outputs currentNode
>     createToken nextNode counter = Token (nextForkId token counter) nextNode currentNode
>     split [] tokenList _ = tokenList
>     split (node:rest) tokenList counter = split rest ((accept node) (createToken node counter) tokenList) (counter + 1)

> acceptJ :: Token -> [Token] -> [Token]
> acceptJ token tokenList
>   | not areAllInputsPresent = token : tokenList
>   | otherwise = (accept outputNode) newToken $ removeInputTokens inputNodes tokenList
>   where
>     areAllInputsPresent = all (inputNodeHasToken (token : tokenList) ) inputNodes
>     inputNodeHasToken [] node = False
>     inputNodeHasToken (tok:rest) node = ( (currNode tok) == currentNode &&
>                                           (prevNode tok) == node ) ||
>                                         inputNodeHasToken rest node
>     removeInputTokens [] tokenList = tokenList
>     removeInputTokens (inputNode:rest) tokenList = removeInputTokens rest $
>                                                      removeFirst (\tok->(prevNode tok) == inputNode) tokenList
>     currentNode = currNode token
>     inputNodes = inputs currentNode
>     outputNode = head $ outputs currentNode
>     newToken = Token (nextId token) outputNode currentNode