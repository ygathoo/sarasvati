
> module Workflow where
> import Data.Map (Map)
> import qualified Data.Map as Map

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
>     accept  :: (Token -> WFGraph -> [Token] -> [Token])
>   }

> instance Eq Node where
>   node1 == node2 = (nodeId node1) == (nodeId node2)

> instance Show Node where
>   show a = "[Node id: " ++ (show (nodeId a)) ++ "]"

> data NodeArcs =
>   NodeArcs {
>     node :: Node,
>     nodeInputs :: [Node],
>     nodeOutputs :: [Node]
>  }

> data WFGraph =
>   WFGraph {
>     nodeMap :: Map Integer NodeArcs
>   }

> instance Show WFGraph where
>  show graph = "[WFGraph " ++ show (Map.size $ nodeMap graph) ++ "]"

> outputs graph node = nodeOutputs $ (nodeMap graph) Map.! (nodeId node)

> inputs graph node = nodeInputs $ (nodeMap graph) Map.! (nodeId node)

> graphFromArcList ids arcs = WFGraph $ Map.fromList $ zip ids arcs

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

acceptP
  Accept function for a 'passthrough' node. This type of node is assumed to have one input and one output.
  The function just passes through to the next node in line.

> acceptP :: Token -> WFGraph -> [Token] -> [Token]
> acceptP token graph tokenList
>   | null outputNodes = tokenList
>   | otherwise        = (accept output) newToken graph tokenList
>  where
>    newToken    = Token (nextId token) output currentNode
>    currentNode = currNode token
>    outputNodes = outputs graph currentNode
>    output      = head outputNodes

acceptFork
  Accept function for 'fork' node. This type of node is assumed to have one input and more than one inputs.
  Will generate tokens for each output node.

> acceptFork :: Token -> WFGraph -> [Token] -> [Token]
> acceptFork token graph tokenList
>   | null outputNodes = tokenList
>   | otherwise        = split outputNodes tokenList 0
>   where
>     currentNode                    = currNode token
>     outputNodes                    = outputs graph currentNode
>     newToken nextNode counter      = Token (nextForkId token counter) nextNode currentNode
>     split [] tokenList _           = tokenList
>     split (x:xs) tokenList counter = split xs ((accept x) (newToken x counter) graph tokenList) (counter + 1)

acceptJoin
  Accept function for a 'join' node. This type of node is assumed to have more than one input and a single output.
  Behaves as barrier, where all tokens from all inputs collect before moving to the next node.

  Requires that a token from every input (counting the currently processed token) exist before
  passing a token to the output node.

> acceptJoin :: Token -> WFGraph -> [Token] -> [Token]
> acceptJoin token graph tokenList
>   | not areAllInputsPresent = token : tokenList
>   | null outputNodes        = removeInputTokens inputNodes tokenList
>   | otherwise               = (accept outputNode) newToken graph $ removeInputTokens inputNodes tokenList
>   where
>     areAllInputsPresent                = all (inputNodeHasToken (token : tokenList) ) inputNodes
>
>     inputNodeHasToken [] node          = False
>     inputNodeHasToken (tok:rest) node  = ( (currNode tok) == currentNode &&
>                                            (prevNode tok) == node ) ||
>                                          inputNodeHasToken rest node
>
>     removeInputTokens [] tokenList     = tokenList
>     removeInputTokens (x:xs) tokenList = removeInputTokens xs $
>                                                       removeFirst (\tok->(prevNode tok) == x) tokenList
>
>     currentNode                        = currNode token
>     inputNodes                         = inputs graph currentNode
>     outputNodes                        = outputs graph currentNode
>     outputNode                         = head $ outputNodes
>     newToken                           = Token (nextId token) outputNode currentNode